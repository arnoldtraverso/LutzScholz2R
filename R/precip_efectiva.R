#' Precipitacion efectiva de una curva USBR
#'
#' Evalua el polinomio de 5 grado de una curva del metodo USBR sobre un vector de
#' precipitacion mensual.
#'
#' @param P Vector de precipitacion mensual (mm).
#' @param coef Vector de 6 coeficientes polinomiales `c(a0, a1, a2, a3, a4, a5)`.
#'
#' @return Vector de precipitacion efectiva (mm) de la misma longitud que `P`.
#' @export
#'
#' @examples
#' pe_curva(c(130.24, 90.01), coef_pe_usbr()$II)
pe_curva <- function(P, coef) {
  stopifnot(length(coef) == 6)
  potencias <- outer(P, 0:5, `^`)      # matriz n x 6
  as.numeric(potencias %*% coef)
}

#' Precipitacion efectiva por mezcla de curvas USBR
#'
#' Combina las curvas PE II y PE III con los pesos de mezcla del modelo calibrado:
#' `PE = c1 * PE_II + c2 * PE_III`.
#'
#' @param P Vector de precipitacion mensual (mm).
#' @param params Objeto [lutz_params].
#'
#' @return `data.frame` con columnas `P`, `PE_II`, `PE_III` y `PE`.
#' @export
#'
#' @examples
#' pe_usbr(c(130.24, 90.01, 102.13), huancane_params())
pe_usbr <- function(P, params = huancane_params()) {
  stopifnot(inherits(params, "lutz_params"))
  pe2 <- pe_curva(P, params$coef_pe$II)
  pe3 <- pe_curva(P, params$coef_pe$III)
  data.frame(
    P     = P,
    PE_II = pe2,
    PE_III = pe3,
    PE    = params$c1 * pe2 + params$c2 * pe3
  )
}
