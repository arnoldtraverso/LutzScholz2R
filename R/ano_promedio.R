#' Conversion de lamina (mm/mes) a caudal (m3/s)
#'
#' @param mm Lamina mensual en mm.
#' @param area Area de la cuenca en km2.
#' @param dias Dias del mes.
#'
#' @return Caudal medio en m3/s.
#' @export
mm_a_m3s <- function(mm, area, dias) {
  # mm sobre km2 -> m3/s : mm * area(km2) * 1000 / (dias * 86400) = mm * area / (dias * 86.4)
  mm * area / (dias * 86.4)
}

#' Balance del ano promedio (modelo deterministico)
#'
#' Calcula el caudal medio mensual del ano promedio combinando la precipitacion
#' efectiva con la contribucion de la retencion:
#' \deqn{Caudal_{mm} = PE + Gi - Ai}
#'
#' @param precip Puede ser (a) un vector de 12 con la precipitacion media mensual
#'   (enero a diciembre) o (b) una `data.frame`/matriz ano x mes; en el segundo caso
#'   se usan las medias mensuales.
#' @param params Objeto [lutz_params].
#'
#' @return `data.frame` (12 filas) con columnas `mes`, `dias`, `P`, `PE`, `Gi`,
#'   `Ai`, `caudal_mm`, `caudal_m3s`.
#' @export
#'
#' @examples
#' ano_promedio(c(130.24, 90.01, 102.13, 45.12, 12.47, 5.94,
#'                3.5, 11.06, 29.56, 46.68, 54.84, 96.18))
ano_promedio <- function(precip, params = huancane_params()) {
  stopifnot(inherits(params, "lutz_params"))
  P <- precip_media_mensual(precip)

  pe  <- pe_usbr(P, params)
  ret <- retencion(params)

  caudal_mm  <- pe$PE + ret$Gi - ret$Ai
  caudal_m3s <- mm_a_m3s(caudal_mm, params$area, params$dias_mes)

  data.frame(
    mes        = meses_abrev(),
    dias       = params$dias_mes,
    P          = P,
    PE         = pe$PE,
    Gi         = ret$Gi,
    Ai         = ret$Ai,
    caudal_mm  = caudal_mm,
    caudal_m3s = caudal_m3s
  )
}

#' Precipitacion media mensual a partir de una serie o un vector
#'
#' @param precip Vector de 12 valores o `data.frame`/matriz ano x mes.
#' @return Vector de 12 con la media mensual (enero a diciembre).
#' @keywords internal
#' @export
precip_media_mensual <- function(precip) {
  if (is.numeric(precip) && is.null(dim(precip))) {
    stopifnot(length(precip) == 12)
    return(as.numeric(precip))
  }
  m <- as_matriz_mensual(precip)
  colMeans(m, na.rm = TRUE)
}

#' Abreviaturas de los meses (enero a diciembre)
#' @keywords internal
#' @export
meses_abrev <- function() {
  c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
    "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
}
