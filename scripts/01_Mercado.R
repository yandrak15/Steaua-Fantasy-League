# ===================== scripts/01_Mercado.R =====================
# Usa variables inyectadas por main.R:
# - DATA_DIR       : carpeta /data
# - NAVEGADOR      : TRUE/FALSE (Selenium)
# - INDICE         : TRUE/FALSE (recalcular indice)
# - MERCADO        : TRUE/FALSE (scrapear mercado)

if (!exists("DATA_DIR"))  DATA_DIR  <- file.path(getwd(), "data")
if (!exists("NAVEGADOR")) NAVEGADOR <- TRUE
if (!exists("INDICE"))    INDICE    <- TRUE
if (!exists("MERCADO"))   MERCADO   <- TRUE

pkgs <- c("rvest","xml2","tidyverse","lubridate","data.table","stringr")
if (NAVEGADOR) pkgs <- c(pkgs, "RSelenium","wdman")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create(DATA_DIR, showWarnings = FALSE)

num_clean <- function(x){
  x <- str_replace_all(x, "\\.|\\+|\\s*\\(.*?\\)", "")
  x <- str_trim(x)
  suppressWarnings(as.numeric(x))
}

wait_el <- function(remDr, using = "css selector", value, timeout_ms = 12000L, poll_ms = 150L, one = FALSE){
  t0 <- Sys.time()
  repeat{
    el <- try(if (one) remDr$findElement(using, value) else remDr$findElements(using, value), TRUE)
    ok <- !inherits(el, "try-error") && length(el) > 0
    if (ok) return(el)
    if (as.numeric(difftime(Sys.time(), t0, "secs"))*1000 > timeout_ms) return(NULL)
    Sys.sleep(poll_ms/1000)
  }
}

indice_path        <- file.path(DATA_DIR, "indice.csv")
tabla_mercado_path <- file.path(DATA_DIR, "tabla_mercado.csv")

# --------- Selenium opcional ----------
remDr <- NULL
if (NAVEGADOR) {
  # Recomendado: dejar que Chrome/driver lo gestione wdman sin fijar versión
  # En runners Linux puede requerir permisos; si falla, ejecuta sin Selenium.
  driver <- try(wdman::chrome(port = 1995L), silent = TRUE)
  if (!inherits(driver, "try-error")) {
    remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = 1995L, browserName = "chrome")
    remDr$open()
    remDr$navigate("https://www.futbolfantasy.com/laliga/estadisticas/jugador")
  } else {
    warning("No se pudo iniciar Selenium/ChromeDriver. Continuo sin navegador.")
    NAVEGADOR <- FALSE
  }
}

# --------- ÍNDICE ----------
if (INDICE && NAVEGADOR) {
  indice_old <- if (file.exists(indice_path)) fread(indice_path) else data.table(Jugador=character(), url=character())
  setnames(indice_old, names(indice_old), c("Jugador","url"), skip_absent=TRUE)

  indice_new <- data.table()
  repeat {
    if (is.null(wait_el(remDr, "css selector", "table#dtStatsJugador", 20000L))) break
    html     <- remDr$getPageSource()[[1]]
    htmldata <- read_html(html)

    tabla <- htmldata %>% html_element("table#dtStatsJugador")
    if (is.na(html_name(tabla))) break

    anchors <- tabla %>% html_elements("tbody tr td:nth-child(1) a.dtlink_player")
    Jugador <- anchors %>% html_text2() %>% str_squish()
    hrefs   <- anchors %>% html_attr("href")
    url     <- xml2::url_absolute(hrefs, "https://www.futbolfantasy.com")

    tmp <- data.table(Jugador = Jugador, url = url) %>% dplyr::filter(Jugador != "", !is.na(url))
    indice_new <- rbind(indice_new, tmp, fill = TRUE)

    btn_next <- wait_el(remDr, "id", "dtStatsJugador_next", 4000L, one = TRUE)
    if (is.null(btn_next)) break
    clase <- try(btn_next$getElementAttribute("class")[[1]], TRUE); clase <- if (inherits(clase, "try-error")) "" else clase
    if (stringr::str_detect(clase, "disabled")) break
    try(btn_next$clickElement(), TRUE)
    Sys.sleep(0.5)
  }

  indice <- merge(unique(indice_new), indice_old, by=c("Jugador","url"), all=TRUE)
  fwrite(indice, indice_path)
} else if (!file.exists(indice_path)) {
  # Sin Selenium no podemos navegar; requiere tener indice previo
  indice <- data.table(Jugador=character(), url=character())
  fwrite(indice, indice_path)
} else {
  indice <- fread(indice_path)
}

# --------- MERCADO ----------
if (MERCADO && NAVEGADOR) {
  tabla_mercado_old <- if (file.exists(tabla_mercado_path)) fread(tabla_mercado_path) else data.table()
  tabla_mercado_new <- data.table()

  n <- nrow(indice)
  for (i in seq_len(n)) {
    url <- indice$url[i]
    jugador_nombre <- indice$Jugador[i]
    cat("\n", format(Sys.time(), "%H:%M:%S"), "- mercado:", jugador_nombre, "-", i, "/", n, "\n")

    remDr$navigate(url)

    # Espera filas o mensaje 'sin datos'
    has_rows <- FALSE
    t0 <- Sys.time()
    repeat{
      rows <- try(remDr$findElements("css selector", "#dataTable .row:not(.font-weight-bold), table#dataTable tbody tr"), TRUE)
      nodata <- try(remDr$findElements("xpath", "//*[contains(.,'Sin histórico') or contains(.,'No hay datos')]"), TRUE)
      if (!inherits(rows,"try-error") && length(rows) > 0) { has_rows <- TRUE; break }
      if (!inherits(nodata,"try-error") && length(nodata) > 0) { has_rows <- FALSE; break }
      if (as.numeric(difftime(Sys.time(), t0, "secs")) > 12) { has_rows <- FALSE; break }
      Sys.sleep(0.2)
    }
    if (!has_rows){ cat(" - Sin tabla. Siguiente ->", jugador_nombre, "\n"); next }

    html     <- remDr$getPageSource()[[1]]
    htmldata <- read_html(html)

    nombre   <- htmldata %>% html_element("div.jugador-nombre.text-center") %>% html_text2()
    equipo   <- htmldata %>% html_element(".img-underphoto.text-center img[alt]") %>% html_attr("alt")
    posicion <- htmldata %>%
      html_element(xpath = paste0("//div[contains(@class,'info')][.//img[@alt='LaLiga Fantasy Oficial'] or contains(., 'LaLiga F.')]", "//span[contains(@class,'position-box')]")) %>%
      html_text2()

    if (is.na(nombre) || nombre == "") nombre <- jugador_nombre

    filas <- htmldata %>% html_elements("#dataTable .row:not(.font-weight-bold)")
    if (!length(filas)) filas <- htmldata %>% html_elements("table#dataTable tbody tr")
    if (!length(filas)) { cat(" - DOM sin filas. Siguiente ->", jugador_nombre, "\n"); next }

    prec_dt <- rbindlist(lapply(filas, function(f){
      divs <- f %>% html_elements("div")
      if (length(divs) >= 3){
        data.table(
          Fecha   = divs[1] %>% html_text2(),
          Cambios = divs[2] %>% html_text2(),
          Precio  = divs[3] %>% html_text2()
        )
      } else {
        tds <- f %>% html_elements("td")
        if (length(tds) >= 3){
          data.table(
            Fecha   = tds[1] %>% html_text2(),
            Cambios = tds[2] %>% html_text2(),
            Precio  = tds[3] %>% html_text2()
          )
        } else data.table(Fecha=NA_character_, Cambios=NA_character_, Precio=NA_character_)
      }
    }), fill = TRUE)

    prec_dt <- prec_dt[!is.na(Fecha) & !is.na(Precio)]
    if (!nrow(prec_dt)) { cat(" - Filas vacías tras limpieza. Siguiente ->", jugador_nombre, "\n"); next }

    prec_dt[, `:=`(
      Precio            = {x <- gsub("\\.|\\+|\\s*\\(.*?\\)","", Precio); x <- trimws(x); suppressWarnings(as.numeric(x))},
      Fecha             = as.Date(paste0(Fecha, "/2025"), format = "%d/%m/%Y"),
      `Jugador.Fantasy` = nombre,
      `Equipo.Real`     = equipo,
      Posicion          = posicion
    )]

    tabla_mercado_new <- rbind(tabla_mercado_new, prec_dt, fill = TRUE)
    Sys.sleep(0.2)
  }

  # Limpieza + merge
  tabla_mercado_new <- tabla_mercado_new %>%
    dplyr::filter(!is.na(Fecha), !is.na(Precio)) %>%
    dplyr::distinct(`Jugador.Fantasy`, `Equipo.Real`, Posicion, Fecha, .keep_all = TRUE) %>%
    dplyr::arrange(`Jugador.Fantasy`, `Equipo.Real`, Posicion, Fecha)

  if (nrow(tabla_mercado_old)){
    tabla_mercado <- merge(
      tabla_mercado_old, tabla_mercado_new,
      by = c("Jugador.Fantasy","Equipo.Real","Posicion","Fecha","Cambios","Precio"),
      all = TRUE
    )
  } else {
    tabla_mercado <- tabla_mercado_new
  }

  fwrite(indice,        indice_path)
  fwrite(tabla_mercado, tabla_mercado_path)

} else {
  message("MERCADO saltado (sin Selenium). Asegúrate de tener ", basename(tabla_mercado_path))
}
