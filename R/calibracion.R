#' Calibracion por regresion multiple
#'
#' Ajusta el modelo autoregresivo de la relacion precipitacion-escorrentia del
#' ano promedio:
#' \deqn{Q_t = b_1 + b_2 Q_{t-1} + b_3 PE_t}
#' El caudal del mes anterior (`Qt-1`) se toma de forma circular (diciembre precede
#' a enero), tal como en la plantilla original.
#'
#' @param caudal Vector de 12 caudales medios mensuales (m3/s) del ano promedio,
#'   normalmente `ano_promedio(...)$caudal_m3s`.
#' @param pe Vector de 12 con la precipitacion efectiva mensual del ano promedio.
#'
#' @return Objeto `lutz_calibracion` con:
#'   \item{coef}{coeficientes `b1`, `b2`, `b3`.}
#'   \item{s}{error tipico (desviacion estandar de los residuos).}
#'   \item{r2}{coeficiente de determinacion R^2.}
#'   \item{r2_aj}{R^2 ajustado.}
#'   \item{corr}{coeficiente de correlacion multiple.}
#'   \item{n}{numero de observaciones.}
#'   \item{fit}{el objeto `lm` subyacente.}
#' @export
calibrar_regresion <- function(caudal, pe) {
  stopifnot(length(caudal) == 12, length(pe) == 12)
  q_lag <- c(caudal[12], caudal[-12])          # rezago circular
  datos  <- data.frame(Qt = caudal, Qt_1 = q_lag, PE = pe)
  fit    <- stats::lm(Qt ~ Qt_1 + PE, data = datos)

  co  <- stats::coef(fit)
  sm  <- summary(fit)
  structure(
    list(
      coef  = c(b1 = unname(co[1]), b2 = unname(co[2]), b3 = unname(co[3])),
      s     = sm$sigma,
      r2    = sm$r.squared,
      r2_aj = sm$adj.r.squared,
      corr  = sqrt(sm$r.squared),
      n     = nrow(datos),
      fit   = fit
    ),
    class = "lutz_calibracion"
  )
}

#' @export
print.lutz_calibracion <- function(x, ...) {
  cat("<lutz_calibracion>  Qt = b1 + b2*Qt-1 + b3*PE\n")
  cat(sprintf("  b1 (intercepto) : %.4f\n", x$coef["b1"]))
  cat(sprintf("  b2 (Qt-1)       : %.4f\n", x$coef["b2"]))
  cat(sprintf("  b3 (PE)         : %.4f\n", x$coef["b3"]))
  cat(sprintf("  Error tipico S  : %.4f\n", x$s))
  cat(sprintf("  R^2 / R^2 aj    : %.4f / %.4f\n", x$r2, x$r2_aj))
  cat(sprintf("  Correlacion     : %.4f  (n = %d)\n", x$corr, x$n))
  invisible(x)
}
