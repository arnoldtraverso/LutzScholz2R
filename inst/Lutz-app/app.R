# Cargar librerías necesarias
library(shiny)
library(bslib) # El nuevo estándar para diseño en Shiny

# 1. Definición de la Interfaz de Usuario (UI) ------------------------------
ui <- page_navbar(
  # ¡Aquí aplicamos la magia de Bootswatch!
  theme = bs_theme(bootswatch = "cosmo"),
  
  title = "LutzScholz2R",
  
  # Módulo 1: Carga y Exploración de Datos
  nav_panel(
    title = "Carga y Exploración",
    icon = icon("upload"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Subir Archivos Históricos", class = "bg-primary text-white"),
        fileInput("file_precip", "Precipitación Mensual (CSV/Excel)"),
        fileInput("file_temp", "Temperatura Mensual (CSV/Excel)")
      ),
      card(
        card_header("Exploración Visual", class = "bg-info text-white"),
        p("Aquí integraremos el gráfico interactivo (plotly) de la serie temporal para validar consistencia de datos.")
      )
    )
  ),
  
  # Módulo 2: Parámetros de la Cuenca
  nav_panel(
    title = "Parámetros de Cuenca",
    icon = icon("sliders"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Geomorfología", class = "bg-warning"),
        numericInput("area", "Área de la cuenca (km²):", value = 3631.19, min = 0.1),
        numericInput("altitud", "Altitud media (m.s.n.m.):", value = 3800, min = 0),
        numericInput("latitud", "Latitud (°Sur):", value = 15, min = 0)
      ),
      card(
        card_header("Metodología", class = "bg-success text-white"),
        selectInput("metodo_escurrimiento", "Coeficiente de Escurrimiento:",
                    choices = c("Misión Alemana (Recomendado para sierra)" = "alemana",
                                "Método de L. Turc" = "turc"))
      )
    )
  ),
  
  # Módulo 3: Balance Hídrico (Año Promedio)
  nav_panel(
    title = "Balance Hídrico",
    icon = icon("tint"),
    card(
      card_header("Año Promedio (Modelo Determinístico)", class = "bg-primary text-white"),
      p("Aquí se renderizará la tabla mensual con P, PE (USBR), Gasto, Abastecimiento y Caudal Promedio.")
    )
  ),
  
  # Módulo 4: Generación de Serie Extendida
  nav_panel(
    title = "Serie Extendida",
    icon = icon("chart-line"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Generación de Caudales (Proceso Markoviano)", class = "bg-primary text-white"),
        p("Aquí se visualizará el hidrograma de la serie sintética generada vs observada.")
      ),
      card(
        card_header("Test Estadísticos", class = "bg-info text-white"),
        p("Resultados T-Student y Fisher para validar la generación de la serie.")
      )
    )
  ),
  
  # Módulo 5: Modelos ART
  nav_panel(
    title = "Modelos ART",
    icon = icon("project-diagram"),
    card(
      card_header("Modelos Autoregresivos Traverso (ART)", class = "bg-success text-white"),
      p("Espacio para calcular el Modelo ART2: Qm = f(P, PE, ETo) y evaluar las medidas de bondad de ajuste (Nash-Sutcliffe, Schultz, etc.).")
    )
  )
)

# 2. Definición de la Lógica del Servidor (Server) ---------------------------
server <- function(input, output, session) {
  # Lógica reactiva pendiente de programar
}

# 3. Ejecutar la Aplicación --------------------------------------------------
shinyApp(ui, server)