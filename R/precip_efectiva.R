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

#' Tabla de calibracion de las curvas de precipitacion efectiva (USBR)
#'
#' Replica la tabla `CALIBRACION!B2:J18` de la plantilla original: calcula la
#' precipitacion efectiva mensual con las tres curvas USBR (I, II, III) y deriva
#' los pesos de mezcla de dos pares de curvas adyacentes (I-II y II-III) de modo
#' que la precipitacion efectiva anual resultante sea igual a
#' `coef_precip * P_total_anual` (fila "Coeficientes", celda `D18`).
#'
#' @param precip Vector de 12 o `data.frame`/matriz ano x mes de precipitacion (mm).
#' @param params Objeto [lutz_params].
#' @param coef_precip Coeficiente de precipitacion efectiva anual (celda `D18`,
#'   0.25 en la calibracion original de Huancane).
#'
#' @return Lista con: `mensual` (data.frame de 12 filas: mes, dias, P, PE_I, PE_II,
#'   PE_mix_I_II, PE_III, PE_mix_II_III), `totales` (data.frame de 1 fila con los
#'   mismos totales anuales) y `coef` (lista con `d18`, `e18`, `f18`, `g18`, `h18`,
#'   `i18`, `j18`: los pesos de mezcla, replicando la fila "Coeficientes").
#' @export
#'
#' @examples
#' pe_calibracion(c(130.24, 90.01, 102.13, 45.12, 12.47, 5.94,
#'                   3.5, 11.06, 29.56, 46.68, 54.84, 96.18))
pe_calibracion <- function(precip, params = huancane_params(), coef_precip = 0.25) {
  stopifnot(inherits(params, "lutz_params"))
  P <- precip_media_mensual(precip)

  PE_I   <- pe_curva(P, params$coef_pe$I)
  PE_II  <- pe_curva(P, params$coef_pe$II)
  PE_III <- pe_curva(P, params$coef_pe$III)

  tot_P      <- round(sum(P), 2)
  tot_PE_I   <- round(sum(PE_I), 2)
  tot_PE_II  <- round(sum(PE_II), 2)
  tot_PE_III <- round(sum(PE_III), 2)
  objetivo   <- coef_precip * tot_P

  e18 <- (objetivo - tot_PE_II) / (tot_PE_I - tot_PE_II)    # peso PE_I  en mezcla I-II
  f18 <- 1 - e18                                             # peso PE_II en mezcla I-II
  h18 <- (objetivo - tot_PE_III) / (tot_PE_II - tot_PE_III)  # peso PE_II en mezcla II-III
  i18 <- 1 - h18                                              # peso PE_III en mezcla II-III

  PE_mix_I_II   <- e18 * PE_I + f18 * PE_II
  PE_mix_II_III <- h18 * PE_II + i18 * PE_III

  mensual <- data.frame(
    mes = meses_abrev(), dias = params$dias_mes, P = P,
    PE_I = PE_I, PE_II = PE_II, PE_mix_I_II = PE_mix_I_II,
    PE_III = PE_III, PE_mix_II_III = PE_mix_II_III
  )
  totales <- data.frame(
    P = tot_P, PE_I = tot_PE_I, PE_II = tot_PE_II,
    PE_mix_I_II = round(sum(PE_mix_I_II), 2), PE_III = tot_PE_III,
    PE_mix_II_III = round(sum(PE_mix_II_III), 2)
  )

  list(
    mensual = mensual,
    totales = totales,
    coef = list(d18 = coef_precip, e18 = e18, f18 = f18, g18 = e18 + f18,
                h18 = h18, i18 = i18, j18 = h18 + i18)
  )
}
