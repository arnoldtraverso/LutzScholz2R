#' Convierte una serie ano x mes en matriz numerica de 12 columnas
#'
#' Acepta una `data.frame` con una columna de anio (detectada por nombre `anio`/
#' `year`/`ano` o descartada si es la primera columna no mensual) o una matriz ya
#' numerica. Devuelve una matriz con 12 columnas (enero a diciembre).
#'
#' @param x `data.frame` o matriz ano x mes.
#' @return Matriz numerica con 12 columnas y los anios como `rownames` si existen.
#' @export
as_matriz_mensual <- function(x) {
  if (is.matrix(x) && ncol(x) == 12) return(x)
  if (is.data.frame(x)) {
    nm <- tolower(names(x))
    col_anio <- which(nm %in% c("anio", "ano", "year", "año"))
    anios <- NULL
    if (length(col_anio) == 1) {
      anios <- x[[col_anio]]
      x <- x[, -col_anio, drop = FALSE]
    }
    # Nos quedamos con las primeras 12 columnas numericas (ene..dic)
    num <- vapply(x, is.numeric, logical(1))
    m <- as.matrix(x[, num, drop = FALSE][, 1:12, drop = FALSE])
    if (!is.null(anios)) rownames(m) <- anios
    return(m)
  }
  stop("`x` debe ser una data.frame o matriz ano x mes.")
}

#' Lee una serie mensual (precipitacion o caudal) desde CSV
#'
#' El CSV debe tener una columna `anio` seguida de 12 columnas mensuales
#' (enero a diciembre).
#'
#' @param path Ruta al archivo CSV.
#' @return `data.frame` con columna `anio` y 12 columnas mensuales.
#' @export
leer_serie_csv <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE)
  names(df)[1] <- "anio"
  df
}

#' Lee una serie mensual desde una hoja de Excel
#'
#' Envoltura sobre `readxl::read_excel`. Requiere el paquete `readxl`.
#'
#' @param path Ruta al archivo `.xlsx`.
#' @param sheet Nombre o indice de la hoja.
#' @param range Rango opcional (p. ej. "B10:N45") con anio + 12 meses.
#' @param col_names Nombres de columna a asignar.
#' @return `data.frame` con columna `anio` y 12 columnas mensuales.
#' @export
leer_serie_excel <- function(path, sheet, range = NULL,
                             col_names = c("anio", tolower(meses_abrev()))) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Instala el paquete 'readxl' para leer archivos Excel.")
  }
  df <- readxl::read_excel(path, sheet = sheet, range = range,
                           col_names = col_names)
  as.data.frame(df)
}
