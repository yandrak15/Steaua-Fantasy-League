# ===================== scripts/03_ETL_Fantasy.R =====================
if (!exists("DATA_DIR")) DATA_DIR <- file.path(getwd(), "data")
if (!exists("OUT_XLSX")) OUT_XLSX <- file.path(DATA_DIR, "fantasy_modelo.xlsx")

library(data.table)
library(openxlsx)
library(lubridate)
options(scipen = 999)

# Entradas
f_fich <- file.path(DATA_DIR, "Fantasy.xlsx")
f_clau <- file.path(DATA_DIR, "clausulas.csv")
f_hist <- file.path(DATA_DIR, "tabla_mercado.csv")

Fichajes  <- as.data.table(openxlsx::read.xlsx(f_fich, sheet = "Fichajes"))
Premios   <- as.data.table(openxlsx::read.xlsx(f_fich, sheet = "Premios"))
Puntos    <- as.data.table(openxlsx::read.xlsx(f_fich, sheet = "Puntos"))
Jugadores <- as.data.table(openxlsx::read.xlsx(f_fich, sheet = "Nombres"))
Claus     <- if (file.exists(f_clau)) fread(f_clau) else data.table()
Mercado   <- if (file.exists(f_hist)) fread(f_hist) else data.table()

SALDO_INICIAL <- 1e8

setDT(Fichajes); setDT(Premios); setDT(Claus)

# ---------------- Resumen ----------------
equipos <- setdiff(sort(unique(c(Fichajes$Comprador, Fichajes$Vendedor))), "Liga")

ingresos <- Fichajes[!is.na(Vendedor),  .(Ingresos = sum(Precio, na.rm=TRUE)), by=.(Equipo = Vendedor)]
gastos   <- Fichajes[!is.na(Comprador), .(Gastos   = sum(Precio, na.rm=TRUE)), by=.(Equipo = Comprador)]
premios  <- Premios[, .(Premios = sum(Premio, na.rm=TRUE)), by=Equipo]
claus    <- Claus[,   .(Clausulas = sum(delta_clausula, na.rm=TRUE)), by=Equipo]

setorder(Fichajes, Jugador, Fecha)
ult <- Fichajes[, .SD[.N], by = Jugador]
plantilla_cnt <- ult[Comprador %in% equipos, .(Jugadores_Plantilla = .N), by = .(Equipo = Comprador)]
fichajes_cnt  <- Fichajes[Comprador %in% equipos, .(Fichajes = .N), by = .(Equipo = Comprador)]
ventas_cnt    <- Fichajes[Vendedor  %in% equipos, .(Ventas   = .N), by = .(Equipo = Vendedor)]

idx_equipo <- data.table(Equipo = equipos)[
  ingresos, on="Equipo"][gastos, on="Equipo"][premios, on="Equipo"][claus, on="Equipo"]
for (v in c("Ingresos","Gastos","Premios","Clausulas")) idx_equipo[is.na(get(v)), (v):=0]

idx_equipo[, `Saldo Actual (Clausulas)` := SALDO_INICIAL + Ingresos + Premios - Gastos - Clausulas]
idx_equipo[, `Saldo Actual` := SALDO_INICIAL + Ingresos + Premios - Gastos]
idx_equipo <- Reduce(function(x,y) y[x, on="Equipo"],
                     list(idx_equipo, plantilla_cnt, fichajes_cnt, ventas_cnt))
setcolorder(idx_equipo, c("Equipo","Saldo Actual","Saldo Actual (Clausulas)","Ingresos","Gastos","Premios",
                          "Clausulas","Ventas","Fichajes","Jugadores_Plantilla"))
fwrite(idx_equipo, file.path(DATA_DIR, "idx_equipo.csv"))

# ---------------- Plantillas / intervalos ----------------
to_date <- function(x){
  if (inherits(x,"Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin="1899-12-30"))
  x <- as.character(x)
  d <- suppressWarnings(lubridate::dmy(x))
  d[is.na(d)] <- suppressWarnings(lubridate::ymd(x[is.na(d)]))
  as.Date(d)
}
for (d in list(Fichajes,Puntos)) setDT(d)
Fichajes[, `:=`(Fecha = to_date(Fecha), Precio = as.numeric(Precio))]
Puntos[,   `:=`(Fecha = to_date(Fecha), Puntos = as.numeric(Puntos), Premio = as.numeric(Premio))]

# contrapartes explícitas
buys0  <- Fichajes[, .(Equipo = Comprador, Jugador, `Jugador.Fantasy`, Posicion,
                       Fecha_Compra = Fecha, Precio_Compra = Precio, Comprado_a = Vendedor)]
sells0 <- Fichajes[, .(Equipo = Vendedor,  Jugador, `Jugador.Fantasy`, Posicion,
                       Fecha_Venta  = Fecha, Precio_Venta  = Precio, Vendido_a = Comprador)]

evt <- rbind(
  buys0[,  .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, Fecha = Fecha_Compra, Precio = Precio_Compra, rol="BUY",  Comprado_a, Vendido_a = NA_character_)],
  sells0[, .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, Fecha = Fecha_Venta,  Precio = Precio_Venta,  rol="SELL", Comprado_a = NA_character_, Vendido_a)]
)
setorder(evt, Equipo, Jugador, `Jugador.Fantasy`, Fecha, rol)
evt[, ciclo := cumsum(rol=="BUY"), by=.(Equipo, Jugador)]

buys  <- evt[rol=="BUY",  .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo,
                            Fecha_Compra = Fecha, Precio_Compra = Precio, Comprado_a)]
sells <- evt[rol=="SELL" & ciclo>0, .SD[1L], by=.(Equipo, Jugador, ciclo)
][,    .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo,
         Fecha_Venta = Fecha, Precio_Venta = Precio, Vendido_a)]

activos  <- buys[!sells, on=.(Equipo, Jugador, ciclo)][
  , `:=`(Fecha_Venta = as.Date(NA), Precio_Venta = NA_real_, En_Plantilla = 1L, Vendido_a = NA_character_)]
vendidos <- sells[buys, on=.(Equipo, Jugador, ciclo)][, `:=`(En_Plantilla = 0L)]

intervalos <- rbindlist(list(
  activos[,  .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo, Fecha_Compra, Precio_Compra,
               Fecha_Venta, Precio_Venta, En_Plantilla, Comprado_a, Vendido_a)],
  vendidos[, .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo, Fecha_Compra, Precio_Compra,
               Fecha_Venta, Precio_Venta, En_Plantilla, Comprado_a, Vendido_a)]
), use.names=TRUE, fill=TRUE)

intervalos <- intervalos[!(is.na(Fecha_Venta) & En_Plantilla == 0) & Equipo != "Liga"]
intervalos[, Dias_Club := as.integer(fifelse(is.na(Fecha_Venta), Sys.Date(), Fecha_Venta) - Fecha_Compra)]
intervalos[, `:=`(start = Fecha_Compra, end = fifelse(is.na(Fecha_Venta), Sys.Date(), Fecha_Venta))]

# sumar puntos/premios dentro del intervalo
pts <- Puntos[!is.na(Fecha), .(Jugador, `Jugador.Fantasy`, Equipo, Fecha, Puntos, Premio)]
joined <- intervalos[
  pts, on=.(Equipo, Jugador, `Jugador.Fantasy`, start <= Fecha, end >= Fecha),
  nomatch=0L, allow.cartesian=TRUE
]
agg <- joined[, .(Puntos = sum(Puntos, na.rm=TRUE),
                  Premios = sum(Premio, na.rm=TRUE)),
              by = .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo,
                     Fecha_Compra, Precio_Compra, Fecha_Venta, Precio_Venta,
                     En_Plantilla, Dias_Club, Comprado_a, Vendido_a)]

mercado_intervalos <- merge(
  intervalos[, .(Equipo, Jugador, `Jugador.Fantasy`, Posicion, ciclo,
                 Fecha_Compra, Precio_Compra, Fecha_Venta, Precio_Venta,
                 En_Plantilla, Dias_Club, Comprado_a, Vendido_a)],
  agg,
  by = c("Equipo","Jugador","Jugador.Fantasy","Posicion","ciclo",
         "Fecha_Compra","Precio_Compra","Fecha_Venta","Precio_Venta",
         "En_Plantilla","Dias_Club","Comprado_a","Vendido_a"),
  all.x = TRUE
)
mercado_intervalos[is.na(Puntos),  Puntos := 0]
mercado_intervalos[is.na(Premios), Premios:= 0]
mercado_intervalos[, Rentabilidad := fifelse(is.na(Precio_Venta), 0, Precio_Venta) + Premios - Precio_Compra]
fwrite(mercado_intervalos, file.path(DATA_DIR, "mercado_intervalos.csv"))

# ---- Precio actual/último por Jugador.Fantasy + Equipo.Real ----
if (nrow(Mercado)) {
  Mercado[, `:=`(Fecha = to_date(Fecha), Precio = as.numeric(Precio))]
  setorder(Mercado, `Jugador.Fantasy`, `Equipo.Real`, Fecha)
  ult_precio <- Mercado[, .SD[.N], by = .(`Jugador.Fantasy`, `Equipo.Real`)][
    , .(`Jugador.Fantasy` = `Jugador.Fantasy`,
        Posicion,
        `Equipo.Real`,
        Fecha_Mercado = Fecha,
        Precio_Actual = Precio)
  ]
} else {
  ult_precio <- data.table(`Jugador.Fantasy`=character(), Posicion=character(),
                           `Equipo.Real`=character(), Fecha_Mercado=as.Date(character()),
                           Precio_Actual=numeric())
}

plantilla <- mercado_intervalos[En_Plantilla == 1L,
  .(Equipo, Jugador, `Jugador.Fantasy`, Posicion,
    Fecha_Compra, Precio_Compra, Puntos, Premios, Dias_Club, Comprado_a)]

plantilla <- ult_precio[plantilla, on = .(`Jugador.Fantasy`, Posicion)]
plantilla[, `Rendimiento`  := Premios - Precio_Compra]
plantilla[, `Venta_Rapida` := Precio_Actual + Premios - Precio_Compra]
fwrite(plantilla, file.path(DATA_DIR, "plantilla.csv"))

precio_plantilla <- plantilla[, .(`Plantila` = sum(Precio_Actual, na.rm = TRUE)), by = Equipo]
idx_equipo <- merge(idx_equipo, precio_plantilla, by = "Equipo", all.x = TRUE)

# ---------------- Relaciones mercado (estadística extra) ----------------
DT <- as.data.table(mercado_intervalos)
buy  <- DT[!is.na(Equipo) & !is.na(Comprado_a) & !is.na(Precio_Compra),
           .(from = Equipo, to = Comprado_a, c_n = 1L, c_e = as.numeric(Precio_Compra), v_n = 0L, v_e = 0)]
sell <- DT[!is.na(Equipo) & !is.na(Vendido_a) & !is.na(Precio_Venta),
           .(from = Equipo, to = Vendido_a, c_n = 0L, c_e = 0, v_n = 1L, v_e = as.numeric(Precio_Venta))]
REL <- rbindlist(list(buy, sell), use.names = TRUE, fill = TRUE)
DIR <- REL[, .(c_n = sum(c_n), c_e = sum(c_e), v_n = sum(v_n), v_e = sum(v_e)), by = .(from, to)]
DIR_rev <- copy(DIR)
setnames(DIR_rev, c("from","to","c_n","c_e","v_n","v_e"), c("to","from","c_n_ba","c_e_ba","v_n_ba","v_e_ba"))
PAIR <- merge(DIR, DIR_rev, by = c("from","to"), all.x = TRUE)
for (v in c("c_n_ba","c_e_ba","v_n_ba","v_e_ba")) PAIR[is.na(get(v)), (v) := 0]
PAIR[, Equipo1 := pmin(from, to)]
PAIR[, Equipo2 := pmax(from, to)]
PAIR <- PAIR[from == Equipo1 & to == Equipo2]
PAIR[, `:=`(
  `A→B_Compras_N` = c_n, `A→B_Compras_€` = c_e, `A→B_Ventas_N` = v_n, `A→B_Ventas_€` = v_e,
  `B→A_Compras_N` = v_n, `B→A_Compras_€` = v_e, `B→A_Ventas_N` = c_n, `B→A_Ventas_€` = c_e
)]
PAIR[, `# Movs` := `A→B_Compras_N` + `A→B_Ventas_N`]
PAIR[, `€ Total`:= `A→B_Compras_€` + `A→B_Ventas_€`]
PAIR[, `Compras_N` := pmax(`A→B_Compras_N`, `B→A_Ventas_N`)]
PAIR[, `Compras_€` := pmax(`A→B_Compras_€`,`B→A_Ventas_€`)]
PAIR[, `Ventas_N`  := pmax(`A→B_Ventas_N`,`B→A_Compras_N`)]
PAIR[, `Ventas_€`  := pmax(`A→B_Ventas_€`,`B→A_Compras_€`)]
PAIR[, `Equipo_Real1` := from]
PAIR[, `Equipo_Real2` := to]
RL_PAIR2 <- PAIR[, .(Equipo1, Equipo2, `Equipo_Real1`, `Equipo_Real2`,
                     `A→B_Compras_N`, `A→B_Compras_€`, `A→B_Ventas_N`, `A→B_Ventas_€`,
                     `B→A_Compras_N`, `B→A_Compras_€`, `B→A_Ventas_N`, `B→A_Ventas_€`,
                     `Compras_N`, `Compras_€`, `Ventas_N`, `Ventas_€`, `# Movs`, `€ Total`)]
setorder(RL_PAIR2, -`€ Total`, -`# Movs`, Equipo1, Equipo2)

# ---------------- Excel final ----------------
fix_dates <- function(DT, cols){
  for (c in intersect(cols, names(DT))) DT[, (c) := to_date(get(c))]
  DT
}

idx_equipo_xlsx         <- copy(idx_equipo)
mercado_intervalos_xlsx <- fix_dates(copy(mercado_intervalos), c("Fecha_Compra","Fecha_Venta"))
plantilla_xlsx          <- fix_dates(copy(plantilla),          c("Fecha_Compra","Fecha_Mercado","Fecha_Venta"))
puntos_xlsx             <- fix_dates(copy(Puntos),             c("Fecha"))
premios_xlsx            <- copy(Premios)
claus_xlsx              <- copy(Claus)
tabla_mercado_xlsx      <- fix_dates(copy(Mercado),            c("Fecha"))
rel_pair_xlsx           <- RL_PAIR2

wb <- openxlsx::createWorkbook()
add_tab <- function(wb, name, DF){
  addWorksheet(wb, name)
  writeData(wb, name, DF)
  setColWidths(wb, name, cols = 1:ncol(DF), widths = "auto")
  st_date <- createStyle(numFmt = "yyyy-mm-dd")
  st_num  <- createStyle(numFmt = "#,##0")
  date_cols <- which(sapply(DF, inherits, "Date"))
  if (length(date_cols))
    addStyle(wb, name, st_date, rows = 2:(nrow(DF)+1), cols = date_cols, gridExpand = TRUE)
  num_cols <- which(sapply(DF, is.numeric))
  if (length(num_cols))
    addStyle(wb, name, st_num, rows = 2:(nrow(DF)+1), cols = num_cols, gridExpand = TRUE)
  freezePane(wb, name, firstActiveRow = 2, firstActiveCol = 1)
}

add_tab(wb, "idx_equipo",         idx_equipo_xlsx)
add_tab(wb, "mercado_intervalos", mercado_intervalos_xlsx)
add_tab(wb, "plantilla",          plantilla_xlsx)
add_tab(wb, "Puntos",             puntos_xlsx)
add_tab(wb, "Premios",            premios_xlsx)
add_tab(wb, "Clausulas",          claus_xlsx)
add_tab(wb, "Historico",          tabla_mercado_xlsx)
add_tab(wb, "stats_equipos",      rel_pair_xlsx)

openxlsx::saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
