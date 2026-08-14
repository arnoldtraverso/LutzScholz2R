# =============================================================================
#  LutzScholz2R - Aplicativo Shiny del modelo hidrologico Lutz Scholz
#  Modela caudales medios mensuales de una cuenca a partir de su precipitacion.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)

# --- Carga del motor: paquete instalado o, en desarrollo, sourcing de R/ -------
# precip_media_huancane/caudal_huancane ya no se cargan por defecto: el usuario
# sube sus propias series por CSV. aleatorios_huancane se mantiene disponible
# para la opcion de generacion con los aleatorios del Excel (fase de Serie Extendida).
if (requireNamespace("LutzScholz2R", quietly = TRUE)) {
  library(LutzScholz2R)
  data("aleatorios_huancane", package = "LutzScholz2R")
} else {
  # Modo desarrollo: buscar la raiz del paquete (carpeta con DESCRIPTION)
  raiz <- getwd()
  while (!file.exists(file.path(raiz, "DESCRIPTION")) &&
         dirname(raiz) != raiz) raiz <- dirname(raiz)
  for (f in list.files(file.path(raiz, "R"), pattern = "\\.R$", full.names = TRUE))
    source(f)
  load(file.path(raiz, "data", "aleatorios_huancane.rda"), envir = globalenv())
}

MESES <- meses_abrev()

# Convierte una tabla ano x mes en serie larga con fecha (dia 1 de cada mes)
a_serie_larga <- function(df, valor = "caudal") {
  m <- as_matriz_mensual(df)
  anios <- if (!is.null(rownames(m))) as.integer(rownames(m)) else seq_len(nrow(m))
  data.frame(
    fecha = as.Date(sprintf("%d-%02d-01", rep(anios, each = 12), rep(1:12, times = nrow(m)))),
    valor = as.numeric(t(m))
  )
}

# =============================================================================
#  UI
# =============================================================================
ui <- page_navbar(
  theme = bs_theme(bootswatch = "superhero"),
  title = "LutzScholz2R",

  # -- Modulo 1: Carga y Exploracion ------------------------------------------
  nav_panel(
    title = "Carga y Exploracion", icon = icon("upload"),

    # Descripcion del modelo y de la app (sobre los boxes de ingreso)
    card(
      card_header(
        tagList(icon("water"), " Modelo hidrologico Lutz Scholz"),
        class = "bg-dark text-white"
      ),
      layout_columns(
        col_widths = c(6, 6),
        div(
          tags$h6(tags$b("¿Qué es?")),
          tags$p(
            "Modelo determinístico-estocástico desarrollado",
            " por el experto alemán Lutz Scholz, en el marco del",
            " PLAN MERIS II para generar ", tags$b("caudales medios mensuales"),
            " en cuencas de la sierra peruana a partir de la ",
            tags$b("precipitación"), ". Calcula un ", tags$b("año promedio"),
            " (balance determinístico) y luego ", tags$b("extiende la serie"),
            " de forma estocástica mediante un proceso markoviano de primer orden."
          )
        ),
        div(
          tags$h6(tags$b("¿Cómo funciona la app?")),
          tags$ol(
            class = "mb-0",
            tags$li("Cargas la ", tags$b("precipitación"), " y el ",
                    tags$b("caudal observado"), " (Huancane por defecto o tus CSV)."),
            tags$li("Ajustas los ", tags$b("parámetros"), " de la cuenca."),
            tags$li("Revisas el ", tags$b("balance del año promedio"), "."),
            tags$li("Generas la ", tags$b("serie extendida"),
                    " y evalúas la bondad de ajuste.")
          )
        )
      ),
      tags$p(
        class = "text-muted small mb-0 mt-2",
        tags$b("Flujo del modelo:"),
        " Precipitación → Precip. efectiva (USBR) → Retención → ",
        "Año promedio → Calibración → Serie generada"
      )
    ),

    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Datos de entrada", class = "bg-primary text-white"),
        p("Sube la precipitacion areal y el caudal observado de tu cuenca",
          "(CSV: columna 'anio' + 12 columnas mensuales)."),
        fileInput("file_precip", "Precipitacion media mensual (CSV)", accept = ".csv"),
        fileInput("file_caudal", "Caudal observado mensual (CSV)", accept = ".csv")
      ),
      card(
        card_header("Series historicas", class = "bg-info text-white"),
        navset_tab(
          nav_panel("Grafico", plotlyOutput("plot_series", height = "360px")),
          nav_panel("Precipitacion", DTOutput("tbl_precip")),
          nav_panel("Caudal observado", DTOutput("tbl_caudal"))
        )
      )
    )
  ),

  # -- Modulo 2: Parametros de Cuenca -----------------------------------------
  nav_panel(
    title = "Parametros de Cuenca", icon = icon("sliders"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Geomorfologia y retencion", class = "bg-warning"),
        numericInput("area", "Area de la cuenca (km2):", value = 3631.19, min = 0.1),
        numericInput("k_agot", "Constante K (agotamiento):", value = 0.03, min = 0, step = 0.001),
        numericInput("reten", "Retencion R (mm):", value = 47, min = 0),
        helpText("El coeficiente de agotamiento (a) y la relacion de caudales a",
                 "30 dias (bo) se calculan automaticamente a partir del area y de K.")
      ),
      card(
        card_header("Generacion de la serie", class = "bg-secondary text-white"),
        numericInput("q0", "Caudal base q0 (m3/s):", value = 2.54, min = 0),
        selectInput("curva", "Curva PE para la generacion:",
                    c("PE II (fiel al Excel)" = "II", "PE mezclada (c1,c2)" = "mix",
                      "PE I" = "I", "PE III" = "III"))
      ),
      card(
        card_header("Parametros activos", class = "bg-success text-white"),
        DTOutput("tbl_params")
      )
    ),
    card(
      card_header("Precipitacion efectiva - tabla de calibracion (CALIBRACION B2:J18)",
                   class = "bg-primary text-white"),
      numericInput("coef_precip", "Coeficiente de precipitacion (D18):",
                   value = 0.25, min = 0, max = 1, step = 0.01),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Curvas PE I - PE II"),
          DTOutput("tbl_pe_izq")
        ),
        card(
          card_header("Curvas PE II - PE III"),
          DTOutput("tbl_pe_der")
        )
      )
    )
  ),

  # -- Modulo 3: Balance Hidrico (Ano promedio) -------------------------------
  nav_panel(
    title = "Balance Hidrico", icon = icon("tint"),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Ano promedio (modelo deterministico)", class = "bg-primary text-white"),
        DTOutput("tbl_ano")
      ),
      card(
        card_header("Caudal medio mensual", class = "bg-info text-white"),
        plotlyOutput("plot_ano", height = "360px")
      )
    )
  ),

  # -- Modulo 4: Serie Extendida ----------------------------------------------
  nav_panel(
    title = "Serie Extendida", icon = icon("chart-line"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Hidrograma: generado vs observado", class = "bg-primary text-white"),
        plotlyOutput("plot_gen", height = "380px")
      ),
      card(
        card_header("Calibracion y bondad de ajuste", class = "bg-info text-white"),
        checkboxInput("usar_excel", "Usar numeros aleatorios del Excel (solo Huancane)", TRUE),
        numericInput("seed", "Semilla (si no se usan los del Excel):", value = 42),
        verbatimTextOutput("print_calib"),
        DTOutput("tbl_bondad")
      )
    )
  )
)

# =============================================================================
#  SERVER
# =============================================================================
server <- function(input, output, session) {

  # -- Datos reactivos (solo por subida de CSV, sin default precargado) ------
  rv <- reactiveValues(precip = NULL, caudal = NULL)

  observeEvent(input$file_precip, {
    req(input$file_precip)
    rv$precip <- leer_serie_csv(input$file_precip$datapath)
  })
  observeEvent(input$file_caudal, {
    req(input$file_caudal)
    rv$caudal <- leer_serie_csv(input$file_caudal$datapath)
  })

  # -- Parametros reactivos ---------------------------------------------------
  # a y bo no se ingresan: se calculan igual que en la plantilla Excel
  # (CALIBRACION!F127 = -0.00252*LN(area)+K ; F128 = EXP(-a*30)).
  params <- reactive({
    a_calc  <- -0.00252 * log(input$area) + input$k_agot
    b0_calc <- exp(-a_calc * 30)
    lutz_params(area = input$area, retencion = input$reten,
                a = a_calc, b0 = b0_calc, q0 = input$q0)
  })

  # -- Cadena del modelo ------------------------------------------------------
  ano <- reactive({
    req(rv$precip)
    ano_promedio(rv$precip, params())
  })
  calib <- reactive(calibrar_regresion(ano()$caudal_m3s, ano()$PE))

  tabla_pe <- reactive({
    req(rv$precip)
    pe_calibracion(rv$precip, params(), coef_precip = input$coef_precip)
  })

  serie_gen <- reactive({
    req(rv$precip)
    pe <- pe_serie(rv$precip, params(), curva = input$curva)
    usar_excel <- isTRUE(input$usar_excel) &&
      nrow(as_matriz_mensual(rv$precip)) == nrow(aleatorios_huancane)
    if (usar_excel) {
      generar_serie(pe, calib(), params(), aleatorios = aleatorios_huancane)
    } else {
      generar_serie(pe, calib(), params(), seed = input$seed)
    }
  })

  # -- Modulo 1: exploracion --------------------------------------------------
  output$plot_series <- renderPlotly({
    req(rv$precip, rv$caudal)
    q <- a_serie_larga(rv$caudal); q$serie <- "Caudal obs. (m3/s)"
    p <- a_serie_larga(rv$precip); p$serie <- "Precipitacion (mm)"
    plot_ly() |>
      add_bars(data = p, x = ~fecha, y = ~valor, name = "Precipitacion (mm)",
                yaxis = "y2", marker = list(color = "#64A7DE")) |>
      add_lines(data = q, x = ~fecha, y = ~valor, name = "Caudal obs. (m3/s)",
                line = list(color = "#261F1F")) |>
      layout(
        yaxis  = list(title = "Caudal (m3/s)"),
        yaxis2 = list(title = "Precipitacion (mm)", overlaying = "y",
                      side = "right", autorange = "reversed"),
        legend = list(orientation = "h", y = 1.1), hovermode = "x unified"
      )
  })
  output$tbl_precip <- renderDT({
    req(rv$precip)
    datatable(rv$precip, options = list(pageLength = 12), rownames = FALSE)
  })
  output$tbl_caudal <- renderDT({
    req(rv$caudal)
    datatable(rv$caudal, options = list(pageLength = 12), rownames = FALSE)
  })

  # -- Modulo 2: parametros ---------------------------------------------------
  output$tbl_params <- renderDT({
    p <- params()
    tb <- data.frame(
      Parametro = c("Area de la cuenca (km2)",
                    "Coef. de agotamiento a (calculado)",
                    "Relacion de caudales 30d bo (calculado)",
                    "Retencion R (mm)",
                    "Caudal base q0 (m3/s)",
                    "Mezcla PE (c1 / c2)"),
      Valor = c(sprintf("%.3f", p$area),
                sprintf("%.6f", p$a),
                sprintf("%.4f", p$b0),
                sprintf("%.1f", p$retencion),
                sprintf("%.2f", p$q0),
                sprintf("%.4f / %.4f", p$c1, p$c2))
    )
    datatable(tb, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  output$tbl_pe_izq <- renderDT({
    tpe <- tabla_pe(); m <- tpe$mensual
    df <- data.frame(
      Mes = c(m$mes, "Total", "Coeficientes"),
      `Dias/mes` = c(m$dias, "", ""),
      `P Total (mm/mes)` = c(round(m$P, 2), tpe$totales$P, tpe$coef$d18),
      `PE I (mm/mes)` = c(round(m$PE_I, 2), tpe$totales$PE_I, round(tpe$coef$e18, 4)),
      `PE II (mm/mes)` = c(round(m$PE_II, 2), tpe$totales$PE_II, round(tpe$coef$f18, 4)),
      `PE (mm)` = c(round(m$PE_mix_I_II, 2), tpe$totales$PE_mix_I_II, round(tpe$coef$g18, 4)),
      check.names = FALSE
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  output$tbl_pe_der <- renderDT({
    tpe <- tabla_pe(); m <- tpe$mensual
    df <- data.frame(
      Mes = c(m$mes, "Total", "Coeficientes"),
      `PE II (mm/mes)` = c(round(m$PE_II, 2), tpe$totales$PE_II, round(tpe$coef$h18, 4)),
      `PE III (mm/mes)` = c(round(m$PE_III, 2), tpe$totales$PE_III, round(tpe$coef$i18, 4)),
      `PE (mm)` = c(round(m$PE_mix_II_III, 2), tpe$totales$PE_mix_II_III, round(tpe$coef$j18, 4)),
      check.names = FALSE
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  # -- Modulo 3: ano promedio -------------------------------------------------
  output$tbl_ano <- renderDT({
    df <- ano()
    df[ , -1] <- round(df[ , -1], 3)
    datatable(df, options = list(dom = "t", pageLength = 12), rownames = FALSE)
  })
  output$plot_ano <- renderPlotly({
    df <- ano()
    plot_ly(df, x = ~factor(mes, levels = MESES)) |>
      add_bars(y = ~caudal_m3s, name = "Caudal (m3/s)", marker = list(color = "#1F6FEB")) |>
      add_lines(y = ~PE, name = "PE (mm)", yaxis = "y2", line = list(color = "#F2994A")) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Caudal (m3/s)"),
        yaxis2 = list(title = "PE (mm)", overlaying = "y", side = "right"),
        legend = list(orientation = "h", y = 1.1)
      )
  })

  # -- Modulo 4: serie extendida ----------------------------------------------
  output$print_calib <- renderPrint(print(calib()))

  output$plot_gen <- renderPlotly({
    g <- a_serie_larga(serie_gen()); g$serie <- "Generado"
    o <- a_serie_larga(rv$caudal);   o$serie <- "Observado"
    plot_ly() |>
      add_lines(data = o, x = ~fecha, y = ~valor, name = "Observado",
                line = list(color = "#8892b0")) |>
      add_lines(data = g, x = ~fecha, y = ~valor, name = "Generado",
                line = list(color = "#1F6FEB")) |>
      layout(yaxis = list(title = "Caudal (m3/s)"),
             legend = list(orientation = "h", y = 1.1), hovermode = "x unified")
  })

  output$tbl_bondad <- renderDT({
    req(rv$caudal)
    # Alinea anios comunes entre observado y generado
    o <- as_matriz_mensual(rv$caudal); g <- as_matriz_mensual(serie_gen())
    comunes <- intersect(rownames(o), rownames(g))
    req(length(comunes) > 0)
    obs <- as.numeric(t(o[comunes, ])); sim <- as.numeric(t(g[comunes, ]))
    tb <- bondad_ajuste(obs, sim); tb$valor <- round(tb$valor, 4)
    datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })
}

shinyApp(ui, server)
