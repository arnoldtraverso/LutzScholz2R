#' Eficiencia de Nash-Sutcliffe
#'
#' @param obs Vector de valores observados.
#' @param sim Vector de valores simulados.
#' @return Coeficiente NSE (1 = ajuste perfecto).
#' @export
nash_sutcliffe <- function(obs, sim) {
  ok <- stats::complete.cases(obs, sim)
  obs <- obs[ok]; sim <- sim[ok]
  1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
}

#' Coeficiente de determinacion R^2
#' @inheritParams nash_sutcliffe
#' @return R^2 (cuadrado de la correlacion de Pearson).
#' @export
r2 <- function(obs, sim) {
  ok <- stats::complete.cases(obs, sim)
  stats::cor(obs[ok], sim[ok])^2
}

#' Raiz del error cuadratico medio
#' @inheritParams nash_sutcliffe
#' @return RMSE en las unidades de los datos.
#' @export
rmse <- function(obs, sim) {
  ok <- stats::complete.cases(obs, sim)
  sqrt(mean((obs[ok] - sim[ok])^2))
}

#' Sesgo porcentual (PBIAS)
#' @inheritParams nash_sutcliffe
#' @return PBIAS en porcentaje.
#' @export
pbias <- function(obs, sim) {
  ok <- stats::complete.cases(obs, sim)
  obs <- obs[ok]; sim <- sim[ok]
  100 * sum(sim - obs) / sum(obs)
}

#' Eficiencia de Kling-Gupta (KGE)
#'
#' \deqn{KGE = 1 - \sqrt{(r-1)^2 + (\alpha-1)^2 + (\beta-1)^2}}
#' donde `r` es la correlacion de Pearson, `alpha = sd(sim)/sd(obs)` (variabilidad
#' relativa) y `beta = mean(sim)/mean(obs)` (sesgo relativo).
#'
#' @inheritParams nash_sutcliffe
#' @return Coeficiente KGE (1 = ajuste perfecto).
#' @export
kge <- function(obs, sim) {
  ok <- stats::complete.cases(obs, sim)
  obs <- obs[ok]; sim <- sim[ok]
  r     <- stats::cor(obs, sim)
  alpha <- stats::sd(sim) / stats::sd(obs)
  beta  <- mean(sim) / mean(obs)
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

#' Tabla resumen de bondad de ajuste
#'
#' @inheritParams nash_sutcliffe
#' @return `data.frame` con NSE, KGE, R^2, RMSE y PBIAS.
#' @export
bondad_ajuste <- function(obs, sim) {
  data.frame(
    metrica = c("Nash-Sutcliffe", "KGE", "R2", "RMSE", "PBIAS (%)"),
    valor   = c(nash_sutcliffe(obs, sim), kge(obs, sim), r2(obs, sim),
                rmse(obs, sim), pbias(obs, sim))
  )
}
