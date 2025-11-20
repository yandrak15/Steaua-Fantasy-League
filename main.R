# ===================== main.R =====================
options(scipen = 999)

# ---- Rutas del proyecto ----
ROOT_DIR  <- getwd()
DATA_DIR  <- file.path(ROOT_DIR, "data")
SCR_DIR   <- file.path(ROOT_DIR, "scripts")
OUT_XLSX  <- file.path(DATA_DIR, "fantasy_modelo.xlsx")

dir.create(DATA_DIR, showWarnings = FALSE)
dir.create(SCR_DIR,  showWarnings = FALSE)

# ---- Flags por entorno (puedes sobreescribir con variables de entorno) ----
# USE_SELENIUM=true/false  |  REBUILD_INDEX=true/false  |  SCRAPE_MERCADO=true/false
USE_SELENIUM  <- tolower(Sys.getenv("USE_SELENIUM", "false")) %in% c("1","true","yes")
REBUILD_INDEX <- tolower(Sys.getenv("REBUILD_INDEX","true"))  %in% c("1","true","yes")
SCRAPE_MERC   <- tolower(Sys.getenv("SCRAPE_MERCADO","true"))%in% c("1","true","yes")

# ---- Dependencias mínimas (CRAN) ----
needed <- c(
  "data.table","readxl","openxlsx","lubridate","stringr","xml2","rvest","DT","bs4Dash"
)
# Para Selenium (opcional):
if (USE_SELENIUM) needed <- c(needed, "RSelenium","wdman")

to_install <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(needed, require, character.only = TRUE))

message("== Iniciando ETL ==")

# --------------------------------------------
# 1) MERCADO (opcional Selenium)
# --------------------------------------------
if (SCRAPE_MERC) {
  message("1) Ejecutando 01_Mercado.R")
  source(file.path(SCR_DIR, "01_Mercado.R"), local = list(
    DATA_DIR       = DATA_DIR,
    NAVEGADOR      = USE_SELENIUM,   # TRUE para Selenium, FALSE para sólo parseo local
    INDICE         = REBUILD_INDEX,  # rehacer índice de jugadores
    MERCADO        = TRUE            # rascar histórico de mercado
  ))
} else {
  message("1) Saltado 01_Mercado.R (SCRAPE_MERCADO=FALSE)")
}

# --------------------------------------------
# 2) CLAUSULAS
# --------------------------------------------
message("2) Ejecutando 02_Clausulas.R")
source(file.path(SCR_DIR, "02_Clausulas.R"), local = list(
  DATA_DIR = DATA_DIR
))

# --------------------------------------------
# 3) ETL FINAL + EXCEL
# --------------------------------------------
message("3) Ejecutando 03_ETL_Fantasy.R")
source(file.path(SCR_DIR, "03_ETL_Fantasy.R"), local = list(
  DATA_DIR = DATA_DIR,
  OUT_XLSX = OUT_XLSX
))

message("== ETL completada. Salida: ", OUT_XLSX)
