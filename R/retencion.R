#' Coeficientes de descarga de la retencion (agotamiento)
#'
#' Durante los meses de estiaje la retencion de la cuenca se descarga siguiendo una
#' curva de agotamiento `bi = b0^k`, donde `k` es la posicion del mes dentro de la
#' secuencia de estiaje.
#'
#' @param b0 Relacion de caudales mensual (coeficiente de agotamiento a 30 dias).
#' @param meses_gasto Indices (1 = enero) de los meses de estiaje.
#'
#' @return Vector de 12 con `bi` (0 fuera de los meses de estiaje).
#' @export
coef_descarga <- function(b0, meses_gasto = 4:9) {
  bi <- numeric(12)
  bi[meses_gasto] <- b0^seq_along(meses_gasto)
  bi
}

#' Contribucion de la retencion: gasto y abastecimiento
#'
#' Calcula, mes a mes, el gasto de la retencion `Gi` (en los meses de estiaje) y el
#' abastecimiento `Ai` (recarga de la retencion), a partir del gasto total `R`.
#'
#' \deqn{Gi = R \cdot bi / \sum bi}
#' \deqn{Ai = R \cdot ai}
#'
#' @param params Objeto [lutz_params].
#'
#' @return `data.frame` (12 filas) con columnas `bi`, `Gi`, `ai`, `Ai`.
#' @export
retencion <- function(params = huancane_params()) {
  stopifnot(inherits(params, "lutz_params"))
  bi <- coef_descarga(params$b0, params$meses_gasto)
  suma_bi <- sum(bi)
  Gi <- ifelse(bi > 0, params$retencion * bi / suma_bi, 0)
  Ai <- params$retencion * params$ai
  data.frame(bi = bi, Gi = Gi, ai = params$ai, Ai = Ai)
}
