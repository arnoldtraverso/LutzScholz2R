#' Lanzar el aplicativo Shiny de LutzScholz2R
#'
#' Abre la interfaz grafica del modelo Lutz Scholz. La app esta empaquetada en
#' `inst/Lutz-app/` y se localiza con [system.file()], siguiendo el mismo patron
#' que usa R-SWAT (`showRSWAT`).
#'
#' @param launch.browser Logico; abrir el navegador automaticamente (por defecto
#'   `TRUE` en sesion interactiva).
#' @param port Puerto TCP en que servir la app. `NULL` deja que Shiny elija.
#' @param ... Argumentos adicionales pasados a [shiny::runApp()].
#'
#' @return Invisiblemente `NULL`. Se llama por su efecto (arranca el servidor).
#' @export
#'
#' @examples
#' \dontrun{
#'   run_lutz_app()
#' }
run_lutz_app <- function(launch.browser = interactive(), port = NULL, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("El paquete 'shiny' es necesario para ejecutar la app.", call. = FALSE)
  }
  app_dir <- system.file("Lutz-app", package = "LutzScholz2R")
  if (app_dir == "" || !file.exists(file.path(app_dir, "app.R"))) {
    stop("No se encontro la app. Reinstala el paquete LutzScholz2R.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, port = port, ...)
}

#' @rdname run_lutz_app
#' @export
showLutz <- run_lutz_app
