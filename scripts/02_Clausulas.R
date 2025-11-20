# ===================== scripts/02_Clausulas.R =====================
if (!exists("DATA_DIR")) DATA_DIR <- file.path(getwd(), "data")

library(data.table)
library(openxlsx)

f_fichajes <- file.path(DATA_DIR, "Fantasy.xlsx")
f_mercado  <- file.path(DATA_DIR, "tabla_mercado.csv")
f_out      <- file.path(DATA_DIR, "clausulas.csv")

Mercado     <- as.data.table(openxlsx::read.xlsx(f_fichajes, sheet = "Fichajes"))
mercado1    <- fread(f_mercado)

setDT(Mercado)
Mercado[, Fecha := as.Date(Fecha)]
Mercado <- Mercado[Comprador != "Liga"]

# Eventos por equipo y ciclo
evt <- rbind(
  Mercado[, .(Equipo = Comprador, Jugador, `Jugador.Fantasy`, Fecha, Precio, rol = "BUY")],
  Mercado[, .(Equipo = Vendedor,  Jugador, `Jugador.Fantasy`, Fecha, Precio, rol = "SELL")]
)
setorder(evt, Equipo, Jugador, Fecha, rol)
evt[, ciclo := cumsum(rol == "BUY"), by = .(Equipo, Jugador)]

buys  <- evt[rol == "BUY",  .(Equipo, Jugador, `Jugador.Fantasy`, ciclo, Fecha_Compra = Fecha, Precio_Compra = Precio)]
sells <- evt[rol == "SELL" & ciclo > 0, .SD[1L], by = .(Equipo, Jugador, ciclo)
][, .(Equipo, Jugador, `Jugador.Fantasy`, ciclo, Fecha_Venta = Fecha, Precio_Venta = Precio)]

# Descarta "Liga"
buys  <- buys[Equipo  != "Liga"]
sells <- sells[Equipo != "Liga"]

intervalos <- merge(buys, sells, by = c("Equipo","Jugador","Jugador.Fantasy","ciclo"), all.x = TRUE)
intervalos <- intervalos[!is.na(Fecha_Venta)]

# Precio de mercado el día de compra (roll back)
setDT(mercado1)
setkey(mercado1, `Jugador.Fantasy`, Fecha)
intervalos[, `:=`(Fecha_Compra = as.IDate(Fecha_Compra), Fecha_Venta = as.IDate(Fecha_Venta))]

mkt_buy <- mercado1[
  intervalos[, .(`Jugador.Fantasy` = `Jugador.Fantasy`, Fecha = Fecha_Compra)],
  on = .(`Jugador.Fantasy`, Fecha), roll = Inf
][, .(`Jugador.Fantasy`, Fecha_Compra = Fecha, mkt_compra = Precio)]

setkey(mkt_buy, `Jugador.Fantasy`, Fecha_Compra)
intervalos <- mkt_buy[intervalos, on = .(`Jugador.Fantasy` = `Jugador.Fantasy`, Fecha_Compra)]

# Máximo de mercado entre compra y venta (foverlaps)
mkt <- copy(mercado1)[, `:=`(start = as.IDate(Fecha), end = as.IDate(Fecha))]
setkey(mkt, `Jugador.Fantasy`, start, end)

iv  <- copy(intervalos)[, .(
  id    = .I,
  `Jugador.Fantasy` = `Jugador.Fantasy`,
  start = Fecha_Compra,
  end   = Fecha_Venta
)]
setkey(iv, `Jugador.Fantasy`, start, end)

ov <- foverlaps(mkt, iv, nomatch = 0L)
max_por_id <- ov[, .(max_mkt_interval = max(Precio, na.rm = TRUE)), by = id]

intervalos[, id := .I]
intervalos <- max_por_id[intervalos, on = "id"]

res <- intervalos[, .(
  Equipo, Jugador, Fecha_Compra, Precio_Compra,
  Fecha_Venta, Precio_Venta, mkt_compra, max_mkt_interval
)]

# Cláusula base
res[, clausula_base := fifelse(
  is.na(Precio_Compra) | Precio_Compra == 0,
  1.666666666 * mkt_compra,
  pmax(Precio_Compra, max_mkt_interval, na.rm = TRUE)
)]

# Detección y delta (mitad para tu liga)
res[, clausula_modificada := !is.na(Precio_Venta) & (Precio_Venta > clausula_base)]
res[, delta_clausula      := fifelse(clausula_modificada, (Precio_Venta - clausula_base)/2, 0)]

clausulas <- res[clausula_modificada == TRUE][
  , .(Equipo, Jugador, Fecha_Compra, Precio_Compra,
      Fecha_Venta, Precio_Venta, mkt_compra, max_mkt_interval,
      clausula_base, delta_clausula, clausula_modificada)
]

fwrite(clausulas, f_out)
