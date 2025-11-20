# Fallar rápido si algo rompe
options(error = function(e){ message("ETL FAILED: ", conditionMessage(e)); q(status = 1) })

# Cargas globales (sólo si las necesitas en todo el pipeline)
library(data.table)
library(lubridate)
library(openxlsx)
library(RSelenium)
library(wdman)

source("scripts/01_Mercado.R")
source("scripts/02_Clausulas.R")
source("scripts/03_ETL_Fantasy.R")

dir.create("data", showWarnings = FALSE)

scr   <- run_scrape()              # usa paquete::función dentro
claus <- run_clausulas(scr)        # idem
out   <- build_xlsx(scr, claus)    # idem
message("OK: ", out)
