# LutzScholz2R

## Abaut Lutz Schols Hydrological Model

In the study Generation of monthly flow in the Peruvian Highlands in 1980 of the National Program of Small and Medium Irrigation Plan Meris II, it is argued that this hydrological model is combined because it has a deterministic structure for the calculation of monthly flows for an average year.

## Practical application of the model in a basin

## Estado del proyecto

- **Motor de cálculo (Fase 1):** implementado en `R/` y validado celda a celda
  contra la plantilla Excel de la cuenca Huancané (12 tests `testthat` en verde).
  Cubre precipitación efectiva USBR, retención, año promedio, calibración por
  regresión y generación estocástica (proceso markoviano).
- **Aplicativo Shiny (Fase 2):** `inst/Lutz-app/app.R` conectado al motor, con
  carga de datos (Huancané por defecto o CSV propio), parámetros reactivos,
  balance del año promedio, calibración y hidrograma generado vs observado.

Ver `DESIGN.md` para la arquitectura completa y las decisiones abiertas.

## Cómo ejecutar

```r
# Instalar dependencias (una vez)
install.packages(c("shiny", "bslib", "plotly", "DT", "readxl", "testthat"))

# Ejecutar la app (desde la raíz del repositorio)
shiny::runApp("inst/Lutz-app")
```

Formato de los CSV de entrada: una columna `anio` seguida de 12 columnas
mensuales (enero a diciembre).
