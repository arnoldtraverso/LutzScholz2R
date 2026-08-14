# Construye los datasets empaquetados a partir de los CSV exportados del Excel.
# Ejecutar desde la raiz del paquete:  Rscript data-raw/prepare_data.R
lee <- function(f) {
  df <- utils::read.csv(file.path("data-raw", f), check.names = FALSE)
  names(df)[1] <- "anio"
  df
}

# Precipitacion media mensual (mm) de la cuenca Huancane, 1981-2016.
precip_media_huancane <- lee("precip_media_huancane.csv")
# Caudales medios mensuales observados (m3/s) - estacion Huancane, 1981-2015.
caudal_huancane <- lee("caudal_huancane.csv")
# Numeros aleatorios ~N(0,1) usados en la generacion de la plantilla original.
aleatorios_huancane <- lee("aleatorios_huancane.csv")

if (!dir.exists("data")) dir.create("data")
save(precip_media_huancane, file = "data/precip_media_huancane.rda", version = 2)
save(caudal_huancane,       file = "data/caudal_huancane.rda",       version = 2)
save(aleatorios_huancane,   file = "data/aleatorios_huancane.rda",   version = 2)
cat("Datasets guardados en data/\n")
