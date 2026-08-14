# Validacion del modelo contra la plantilla original INFORMACION - 3.xlsx
# (cuenca Huancane). Tolerancia ~1e-2 por el redondeo de los coeficientes
# polinomiales de las curvas USBR en la plantilla.

par    <- huancane_params()
P_prom <- c(130.24, 90.01, 102.13, 45.12, 12.47, 5.94,
            3.5, 11.06, 29.56, 46.68, 54.84, 96.18)

test_that("la precipitacion efectiva USBR reproduce el Excel", {
  pe <- pe_usbr(P_prom, par)
  exc_PE <- c(49.8142, 20.9353, 28.1322, 5.7198, 1.6291, 0.8327,
              0.5, 1.4664, 3.5065, 5.9997, 7.6955, 24.4239)
  expect_equal(pe$PE, exc_PE, tolerance = 1e-3)
})

test_that("el balance del ano promedio reproduce el caudal generado", {
  ap <- ano_promedio(P_prom, par)
  exc_m3s <- c(42.6841, 51.1766, 45.7861, 25.1523, 9.6575, 5.2119,
               3.8341, 3.1268, 2.5386, 3.6737, 5.5134, 12.0849)
  expect_equal(ap$caudal_m3s, exc_m3s, tolerance = 1e-2)
})

test_that("la calibracion reproduce los coeficientes de regresion del Excel", {
  ap  <- ano_promedio(P_prom, par)
  cal <- calibrar_regresion(ap$caudal_m3s, ap$PE)
  expect_equal(unname(cal$coef["b1"]), -2.4811, tolerance = 1e-3)
  expect_equal(unname(cal$coef["b2"]),  0.6130, tolerance = 1e-3)
  expect_equal(unname(cal$coef["b3"]),  0.7381, tolerance = 1e-3)
  expect_equal(cal$s,     5.4100, tolerance = 1e-3)
  expect_equal(cal$r2,    0.9311, tolerance = 1e-3)
  expect_equal(cal$r2_aj, 0.9157, tolerance = 1e-3)
})

test_that("la generacion estocastica reproduce la serie del Excel (1981)", {
  ap    <- ano_promedio(precip_media_huancane, par)
  cal   <- calibrar_regresion(ap$caudal_m3s, ap$PE)
  pe    <- pe_serie(precip_media_huancane, par, curva = "II")
  serie <- generar_serie(pe, cal, par, aleatorios = aleatorios_huancane)

  exc_1981 <- c(94.09, 69.565, 58.46, 39.019, 20.508, 10.569,
                2.618, 0.718, 0.556, 8.143, 7.282, 30.658)
  gen_1981 <- as.numeric(serie[serie$anio == 1981, meses_abrev()])
  expect_equal(gen_1981, exc_1981, tolerance = 1e-2)
})

test_that("pe_calibracion reproduce la tabla CALIBRACION!B2:J18 del Excel", {
  tpe <- pe_calibracion(P_prom, par, coef_precip = 0.25)
  expect_equal(tpe$totales$P, 627.73, tolerance = 1e-2)
  expect_equal(tpe$totales$PE_I, 46.77, tolerance = 1e-2)
  expect_equal(tpe$totales$PE_II, 136.6, tolerance = 1e-2)
  expect_equal(tpe$totales$PE_III, 226.65, tolerance = 1e-2)
  expect_equal(tpe$coef$e18, -0.226344205721919, tolerance = 1e-3)
  expect_equal(tpe$coef$f18,  1.226344205721920, tolerance = 1e-3)
  expect_equal(tpe$coef$h18,  0.774208772903942, tolerance = 1e-3)
  expect_equal(tpe$coef$i18,  0.225791227096058, tolerance = 1e-3)
})

test_that("las metricas de bondad de ajuste son coherentes", {
  expect_equal(nash_sutcliffe(1:10, 1:10), 1)
  expect_equal(r2(1:10, 2 * (1:10) + 3), 1)
  expect_equal(rmse(c(1, 2, 3), c(1, 2, 3)), 0)
})
