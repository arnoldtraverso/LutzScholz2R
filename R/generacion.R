#' Precipitacion efectiva de una serie completa (ano x mes)
#'
#' Aplica una curva USBR (o la mezcla) a cada valor de una matriz de precipitacion
#' ano x mes. Sirve como insumo de la generacion estocastica.
#'
#' @param precip `data.frame` o matriz ano x mes de precipitacion (mm).
#' @param params Objeto [lutz_params].
#' @param curva Curva a emplear: `"II"` (por defecto, reproduce la plantilla
#'   original), `"I"`, `"III"` o `"mix"` (mezcla c1*PE_II + c2*PE_III).
#'
#' @return Matriz ano x mes de precipitacion efectiva.
#' @export
pe_serie <- function(precip, params = huancane_params(), curva = c("II", "I", "III", "mix")) {
  curva <- match.arg(curva)
  m <- as_matriz_mensual(precip)
  aplica <- function(P) {
    switch(curva,
      I   = pe_curva(P, params$coef_pe$I),
      II  = pe_curva(P, params$coef_pe$II),
      III = pe_curva(P, params$coef_pe$III),
      mix = params$c1 * pe_curva(P, params$coef_pe$II) +
            params$c2 * pe_curva(P, params$coef_pe$III)
    )
  }
  out <- apply(m, 2, aplica)
  dimnames(out) <- dimnames(m)
  out
}

#' Generacion de la serie sintetica de caudales (proceso markoviano)
#'
#' Genera caudales medios mensuales mediante el modelo autoregresivo de primer
#' orden con termino estocastico:
#' \deqn{Q_t = | b_1 + b_2 Q_{t-1} + b_3 PE_t + z_t \cdot S \cdot \sqrt{1 - R^2} |}
#' La serie se encadena mes a mes y ano tras ano; el primer mes usa el caudal base
#' `q0` como `Qt-1`.
#'
#' @param pe_matriz Matriz ano x mes de precipitacion efectiva (ver [pe_serie]).
#' @param calib Objeto [calibrar_regresion].
#' @param params Objeto [lutz_params] (aporta `q0`).
#' @param aleatorios Matriz ano x mes de numeros aleatorios `z`. Si es `NULL` se
#'   generan con `rnorm`.
#' @param seed Semilla para `rnorm` cuando `aleatorios` es `NULL` (reproducibilidad).
#' @param r2 Coeficiente de determinacion para el termino de ruido. Por defecto el
#'   R^2 ajustado de la calibracion (como en la plantilla original).
#'
#' @return `data.frame` ano x mes con los caudales generados (m3/s), mas columna
#'   `anio` si la matriz de PE trae anios en `rownames`.
#' @export
generar_serie <- function(pe_matriz, calib, params = huancane_params(),
                          aleatorios = NULL, seed = NULL, r2 = calib$r2_aj) {
  stopifnot(inherits(calib, "lutz_calibracion"))
  pe <- as_matriz_mensual(pe_matriz)
  n  <- nrow(pe)

  if (is.null(aleatorios)) {
    if (!is.null(seed)) set.seed(seed)
    z <- matrix(stats::rnorm(n * 12), nrow = n, ncol = 12)
  } else {
    z <- as_matriz_mensual(aleatorios)
    stopifnot(nrow(z) == n)
  }

  b   <- calib$coef
  fac <- calib$s * sqrt(1 - r2)

  Q <- matrix(NA_real_, n, 12)
  q_prev <- params$q0
  for (i in seq_len(n)) {
    for (mo in 1:12) {
      q_prev <- abs(b["b1"] + b["b2"] * q_prev + b["b3"] * pe[i, mo] + z[i, mo] * fac)
      Q[i, mo] <- q_prev
    }
  }
  colnames(Q) <- meses_abrev()
  out <- as.data.frame(Q)
  if (!is.null(rownames(pe))) out <- cbind(anio = as.integer(rownames(pe)), out)
  out
}
