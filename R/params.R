#' Parametros del modelo Lutz Scholz
#'
#' Construye la estructura de parametros que gobierna todo el modelo. Los valores
#' por defecto corresponden a la cuenca Huancane (Puno), calibrada en la plantilla
#' `INFORMACION - 3.xlsx`, y sirven como caso de validacion.
#'
#' @param area Area de la cuenca en km2.
#' @param b0 Relacion de caudales / coeficiente de agotamiento mensual (30 dias).
#'   Se relaciona con el coeficiente de agotamiento diario `a` por `b0 = exp(-a*30)`.
#' @param a Coeficiente de agotamiento diario. Solo informativo; el modelo usa `b0`.
#' @param retencion Gasto total de la retencion de la cuenca `R` (mm).
#' @param q0 Caudal base (m3/s) usado como caudal inicial (Qt-1) del primer mes
#'   en la generacion de la serie.
#' @param meses_gasto Indices de los meses (1 = enero) en que la retencion aporta
#'   gasto (estiaje). Por defecto abril a setiembre.
#' @param ai Vector de 12 coeficientes de abastecimiento de la retencion (enero a
#'   diciembre). Deben sumar 1. En Huancane fueron ajustados en la calibracion.
#' @param c1,c2 Pesos de mezcla de las curvas de precipitacion efectiva PE II y
#'   PE III (`c1 + c2 = 1`).
#' @param coef_pe Lista con los coeficientes polinomiales (a0..a5) de las curvas
#'   USBR I, II y III.
#' @param dias_mes Vector de 12 con los dias de cada mes.
#'
#' @return Objeto de clase `lutz_params` (una lista).
#' @export
lutz_params <- function(area = 3631.1925,
                        b0 = 0.755569974571448,
                        a = 0.009342762711487769,
                        retencion = 47,
                        q0 = 2.54,
                        meses_gasto = 4:9,
                        ai = c(0.39, -0.28, -0.12, 0.04, 0.11, 0.11,
                               0.08, 0.08, 0.11, 0.07, 0.08, 0.33),
                        c1 = 0.8438713967492294,
                        c2 = 0.15612860325077055,
                        coef_pe = coef_pe_usbr(),
                        dias_mes = c(31, 28, 31, 30, 31, 30,
                                     31, 31, 30, 31, 30, 31)) {
  stopifnot(
    length(ai) == 12,
    length(dias_mes) == 12,
    abs(sum(ai) - 1) < 1e-6,
    abs((c1 + c2) - 1) < 1e-6
  )
  structure(
    list(area = area, b0 = b0, a = a, retencion = retencion, q0 = q0,
         meses_gasto = meses_gasto, ai = ai, c1 = c1, c2 = c2,
         coef_pe = coef_pe, dias_mes = dias_mes),
    class = "lutz_params"
  )
}

#' @rdname lutz_params
#' @export
huancane_params <- function() {
  lutz_params()
}

#' Coeficientes polinomiales de las curvas USBR de precipitacion efectiva
#'
#' Coeficientes (a0..a5) de los polinomios de 5 grado de las tres curvas estandar
#' del metodo del United States Bureau of Reclamation, tal como estan tabulados en
#' la plantilla original.
#'
#' @return Lista con los vectores `I`, `II` y `III` (cada uno de longitud 6).
#' @export
coef_pe_usbr <- function() {
  list(
    I   = c(-0.018, -0.0185,  0.001105, -1.204e-05,  1.44e-07, -2.85e-10),
    II  = c(-0.021,  0.1358, -0.002296,  4.349e-05, -8.9e-08,  -8.79e-11),
    III = c(-0.028,  0.2756, -0.004103,  5.534e-05,  1.24e-07, -1.42e-09)
  )
}

#' @export
print.lutz_params <- function(x, ...) {
  cat("<lutz_params>\n")
  cat(sprintf("  Area cuenca      : %.3f km2\n", x$area))
  cat(sprintf("  Retencion R      : %.1f mm\n", x$retencion))
  cat(sprintf("  Agotamiento b0   : %.4f\n", x$b0))
  cat(sprintf("  Caudal base q0   : %.2f m3/s\n", x$q0))
  cat(sprintf("  Mezcla PE (c1/c2): %.4f / %.4f\n", x$c1, x$c2))
  cat(sprintf("  Meses de gasto   : %s\n", paste(x$meses_gasto, collapse = ", ")))
  invisible(x)
}
