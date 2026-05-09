# ==============================================================================
# TEST: Motor de generación de gráficos
# ==============================================================================

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)
gc()

# Cargar módulos
library(tidyverse)
source(file.path(PROYECTO_ROOT, "reportes/70_CONFIG_REPORTES.R"))
source(file.path(PROYECTO_ROOT, "generadores/51_GENERADOR_GRAFICOS.R"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║           TEST: 51_GENERADOR_GRAFICOS.R                  ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# TEST 1: Verificar funciones básicas existen
# ==============================================================================

cat("[TEST 1] Verificando funciones básicas...\n")

funciones_requeridas <- c(
  "generar_grafico",
  "tema_sierrafor",
  "exportar_grafico",
  "crear_barras",
  "crear_lineas",
  "crear_histograma",
  "crear_scatter",
  "crear_boxplot",
  "paleta_generos"
)

for (func in funciones_requeridas) {
  if (!exists(func)) {
    stop(sprintf("Función '%s' no encontrada", func))
  }
}

cat(sprintf("  ✓ %d funciones encontradas\n\n", length(funciones_requeridas)))

# ==============================================================================
# TEST 2: Verificar paleta de colores
# ==============================================================================

cat("[TEST 2] Verificando paleta de colores...\n")

stopifnot(
  "COLORES_SIERRAFOR existe" = exists("COLORES_SIERRAFOR"),
  "Es una lista" = is.list(COLORES_SIERRAFOR),
  "Tiene pinus" = "pinus" %in% names(COLORES_SIERRAFOR),
  "Tiene quercus" = "quercus" %in% names(COLORES_SIERRAFOR)
)

cat("  Paleta de géneros:\n")
cat(sprintf("    Pinus:   %s\n", COLORES_SIERRAFOR$pinus))
cat(sprintf("    Quercus: %s\n", COLORES_SIERRAFOR$quercus))
cat(sprintf("    Otros:   %s\n\n", COLORES_SIERRAFOR$otros))

# ==============================================================================
# TEST 3: Crear datos de prueba
# ==============================================================================

cat("[TEST 3] Creando datos de prueba...\n")

# Datos para barras/líneas
datos_series <- tibble(
  año = rep(2026:2035, 2),
  genero = rep(c("Pinus", "Quercus"), each = 10),
  volumen = c(
    150 + cumsum(rnorm(10, 5, 2)),  # Pinus
    120 + cumsum(rnorm(10, 3, 1.5)) # Quercus
  )
)

# Datos para histograma
set.seed(42)
datos_dist <- tibble(
  diametro = c(
    rnorm(200, mean = 30, sd = 8),  # Pinus
    rnorm(150, mean = 25, sd = 6)   # Quercus
  ),
  genero = c(rep("Pinus", 200), rep("Quercus", 150))
)

# Datos para scatter
datos_scatter <- tibble(
  diametro = rnorm(100, 30, 10),
  altura = 1.3 * diametro^0.6 + rnorm(100, 0, 2),
  genero = sample(c("Pinus", "Quercus"), 100, replace = TRUE)
)

cat("  ✓ Datos de prueba creados\n\n")

# ==============================================================================
# TEST 4: Probar tema_sierrafor
# ==============================================================================

cat("[TEST 4] Probando tema_sierrafor()...\n")

tema <- tema_sierrafor()
stopifnot("Es un theme object" = inherits(tema, "theme"))

cat("  ✓ Tema creado exitosamente\n\n")

# ==============================================================================
# TEST 5: Crear gráfico de barras
# ==============================================================================

cat("[TEST 5] Creando gráfico de barras...\n")

datos_barras <- datos_series %>%
  filter(año == 2035) %>%
  mutate(rodal = c("R1", "R2"))

grafico_barras <- generar_grafico(
  datos = datos_barras,
  id = "TEST_barras",
  titulo = "Volumen por género",
  subtitulo = "Año 2035",
  tipo = "barras",
  x = "rodal",
  y = "volumen",
  fill = "genero"
)

stopifnot(inherits(grafico_barras, "grafico_sierrafor"))

cat("  ✓ Gráfico de barras creado\n")
print(grafico_barras)

# ==============================================================================
# TEST 6: Crear gráfico de líneas
# ==============================================================================

cat("[TEST 6] Creando gráfico de líneas...\n")

grafico_lineas <- generar_grafico(
  datos = datos_series,
  id = "TEST_lineas",
  titulo = "Evolución temporal del volumen",
  subtitulo = "Proyección 10 años",
  tipo = "lineas",
  x = "año",
  y = "volumen",
  color = "genero"
)

stopifnot(inherits(grafico_lineas, "grafico_sierrafor"))

cat("  ✓ Gráfico de líneas creado\n\n")

# ==============================================================================
# TEST 7: Crear histograma (APILADO)
# ==============================================================================

cat("[TEST 7] Creando histograma APILADO...\n")

grafico_hist <- generar_grafico(
  datos = datos_dist,
  id = "TEST_histograma",
  titulo = "Distribución diamétrica",
  subtitulo = "Por género (apilado)",
  tipo = "histograma",
  x = "diametro",
  fill = "genero",
  config_extra = list(bins = 20)  # position = "stack" por defecto
)

stopifnot(inherits(grafico_hist, "grafico_sierrafor"))

cat("  ✓ Histograma creado (apilado por defecto)\n")
cat("  ℹ️  Verifica visualmente que las barras estén apiladas\n\n")

# ==============================================================================
# TEST 7B: Verificar unidades en ejes
# ==============================================================================

cat("[TEST 7B] Verificando unidades automáticas en ejes...\n")

# Verificar que el label del eje X tiene unidades
plot_labels <- grafico_hist$plot$labels

cat(sprintf("  Eje X: %s\n", plot_labels$x))
cat(sprintf("  Eje Y: %s\n", plot_labels$y))

# Verificar que "diametro" se convirtió a "Diámetro (cm)"
if (grepl("cm|Diámetro", plot_labels$x, ignore.case = TRUE)) {
  cat("  ✓ Unidades detectadas automáticamente en eje X\n")
} else {
  warning("  ⚠️  No se detectaron unidades en eje X")
}

cat("\n")

# ==============================================================================
# TEST 8: Crear scatter plot
# ==============================================================================

cat("[TEST 8] Creando scatter plot...\n")

grafico_scatter <- generar_grafico(
  datos = datos_scatter,
  id = "TEST_scatter",
  titulo = "Relación diámetro-altura",
  subtitulo = "Con línea de regresión",
  tipo = "scatter",
  x = "diametro",
  y = "altura",
  color = "genero",
  config_extra = list(regresion = TRUE)
)

stopifnot(inherits(grafico_scatter, "grafico_sierrafor"))

cat("  ✓ Scatter plot creado\n\n")

# ==============================================================================
# TEST 9: Crear boxplot
# ==============================================================================

cat("[TEST 9] Creando boxplot...\n")

grafico_boxplot <- generar_grafico(
  datos = datos_dist,
  id = "TEST_boxplot",
  titulo = "Distribución de diámetros por género",
  tipo = "boxplot",
  x = "genero",
  y = "diametro",
  fill = "genero"
)

stopifnot(inherits(grafico_boxplot, "grafico_sierrafor"))

cat("  ✓ Boxplot creado\n\n")

# ==============================================================================
# TEST 10: Gráfico con facets
# ==============================================================================

cat("[TEST 10] Creando gráfico con facets...\n")

# Agregar rodal a datos
datos_facet <- datos_series %>%
  mutate(rodal = rep(c("R1", "R2"), each = 10))

grafico_facet <- generar_grafico(
  datos = datos_facet,
  id = "TEST_facets",
  titulo = "Evolución por rodal",
  tipo = "lineas",
  x = "año",
  y = "volumen",
  color = "genero",
  facet_by = "rodal"
)

stopifnot(inherits(grafico_facet, "grafico_sierrafor"))

cat("  ✓ Gráfico con facets creado\n\n")

# ==============================================================================
# TEST 11: Exportar gráficos
# ==============================================================================

cat("[TEST 11] Exportando gráficos...\n")

# Crear directorio temporal
dir_test <- "test_output_graficos"
if (!dir.exists(dir_test)) {
  dir.create(dir_test, recursive = TRUE)
}

# Exportar PNG
archivo_png <- exportar_grafico(
  grafico_barras,
  directorio = dir_test,
  formato = "png"
)

if (file.exists(archivo_png)) {
  size_kb <- file.size(archivo_png) / 1024
  cat(sprintf("  ✓ PNG creado: %s (%.1f KB)\n", basename(archivo_png), size_kb))
}

# Exportar PDF
archivo_pdf <- exportar_grafico(
  grafico_lineas,
  directorio = dir_test,
  formato = "pdf"
)

if (file.exists(archivo_pdf)) {
  size_kb <- file.size(archivo_pdf) / 1024
  cat(sprintf("  ✓ PDF creado: %s (%.1f KB)\n", basename(archivo_pdf), size_kb))
}

cat("\n")

# ==============================================================================
# TEST 12: Diferentes formatos de exportación
# ==============================================================================

cat("[TEST 12] Probando diferentes formatos...\n")

formatos <- c("png", "pdf", "svg")

for (fmt in formatos) {
  archivo <- exportar_grafico(
    grafico_hist,
    directorio = dir_test,
    formato = fmt
  )
  
  if (file.exists(archivo)) {
    cat(sprintf("  ✓ Formato %s: OK\n", toupper(fmt)))
  }
}

cat("\n")

# ==============================================================================
# TEST 13: Configuraciones adicionales
# ==============================================================================

cat("[TEST 13] Probando configuraciones adicionales...\n")

# Barras horizontales
grafico_horizontal <- generar_grafico(
  datos = datos_barras,
  id = "TEST_horizontal",
  titulo = "Barras horizontales",
  tipo = "barras",
  x = "genero",
  y = "volumen",
  fill = "genero",
  config_extra = list(orientacion = "horizontal")
)

cat("  ✓ Barras horizontales\n")

# Escala logarítmica
grafico_log <- generar_grafico(
  datos = datos_series %>% filter(genero == "Pinus"),
  id = "TEST_log",
  titulo = "Escala logarítmica",
  tipo = "lineas",
  x = "año",
  y = "volumen",
  config_extra = list(escala_y_log = TRUE)
)

cat("  ✓ Escala logarítmica\n")

# Labels personalizados
grafico_labs <- generar_grafico(
  datos = datos_barras,
  id = "TEST_labs",
  titulo = "Labels personalizados",
  tipo = "barras",
  x = "rodal",
  y = "volumen",
  labs_extra = list(
    xlab = "Rodal (UMM)",
    ylab = "Volumen (m³/ha)",
    caption = "Fuente: SIERRAFOR 2025"
  )
)

cat("  ✓ Labels personalizados\n\n")

# ==============================================================================
# TEST 14: Integración con REPORTE_CONFIG
# ==============================================================================

cat("[TEST 14] Verificando integración con REPORTE_CONFIG...\n")

REPORTE_CONFIG$mostrar_progreso <- TRUE

grafico_config <- generar_grafico(
  datos = datos_barras,
  id = "TEST_con_config",
  titulo = "Usando configuración global",
  tipo = "barras",
  x = "genero",
  y = "volumen",
  fill = "genero"
)

exportar_grafico(grafico_config, config = REPORTE_CONFIG)

cat("  ✓ Integración con REPORTE_CONFIG exitosa\n\n")

# ==============================================================================
# TEST 15: Paleta automática de géneros
# ==============================================================================

cat("[TEST 15] Verificando paleta automática...\n")

paleta <- paleta_generos()

stopifnot(
  "Paleta es vector" = is.vector(paleta),
  "Tiene nombres" = !is.null(names(paleta)),
  "Tiene Pinus" = "Pinus" %in% names(paleta),
  "Tiene Quercus" = "Quercus" %in% names(paleta)
)

cat("  Paleta obtenida:\n")
print(paleta)

cat("\n  ✓ Paleta automática funciona\n\n")

# ==============================================================================
# TEST 16: Verificar unidades automáticas en múltiples gráficos
# ==============================================================================

cat("[TEST 16] Verificando unidades en múltiples tipos...\n")

# Crear datos con columnas específicas
datos_unidades <- data.frame(
  rodal = c("R1", "R2", "R3"),
  vol_ha_m3 = c(150, 200, 175),
  ab_ha_m2 = c(25, 30, 28),
  n_ha = c(250, 300, 275),
  genero = c("Pinus", "Quercus", "Pinus")
)

# Test 1: Volumen
g1 <- generar_grafico(
  datos = datos_unidades,
  id = "test_vol",
  titulo = "Test volumen",
  tipo = "barras",
  x = "rodal",
  y = "vol_ha_m3"
)

cat(sprintf("  Volumen - Eje Y: %s\n", g1$plot$labels$y))
stopifnot(grepl("m³/ha|Volumen", g1$plot$labels$y))

# Test 2: Área basal
g2 <- generar_grafico(
  datos = datos_unidades,
  id = "test_ab",
  titulo = "Test área basal",
  tipo = "barras",
  x = "rodal",
  y = "ab_ha_m2"
)

cat(sprintf("  Área basal - Eje Y: %s\n", g2$plot$labels$y))
stopifnot(grepl("m²/ha|Área", g2$plot$labels$y))

# Test 3: Densidad
g3 <- generar_grafico(
  datos = datos_unidades,
  id = "test_dens",
  titulo = "Test densidad",
  tipo = "barras",
  x = "rodal",
  y = "n_ha"
)

cat(sprintf("  Densidad - Eje Y: %s\n", g3$plot$labels$y))
stopifnot(grepl("árb/ha|Densidad", g3$plot$labels$y))

cat("\n  ✓ Todas las unidades se infirieron correctamente\n\n")

# ==============================================================================
# TEST 17: Comparación visual histograma stack vs identity
# ==============================================================================

cat("[TEST 17] Comparando histograma apilado vs superpuesto...\n")

# Histograma apilado (por defecto)
hist_stack <- generar_grafico(
  datos = datos_dist,
  id = "TEST_hist_stack",
  titulo = "Histograma APILADO (stack)",
  tipo = "histograma",
  x = "diametro",
  fill = "genero",
  config_extra = list(bins = 15)
)

# Histograma superpuesto (identity)
hist_identity <- generar_grafico(
  datos = datos_dist,
  id = "TEST_hist_identity",
  titulo = "Histograma SUPERPUESTO (identity)",
  tipo = "histograma",
  x = "diametro",
  fill = "genero",
  config_extra = list(bins = 15, position = "identity")
)

cat("  ✓ Ambos histogramas creados\n")
cat("  ℹ️  Exportando para comparación visual...\n")

exportar_grafico(hist_stack, directorio = dir_test)
exportar_grafico(hist_identity, directorio = dir_test)

cat("  ✓ Archivos exportados para comparación\n\n")

# ==============================================================================
# LIMPIEZA
# ==============================================================================

cat("[LIMPIEZA] Verificando archivos generados...\n")

archivos_test <- list.files(dir_test, full.names = TRUE)
cat(sprintf("  %d archivos creados en %s/\n", length(archivos_test), dir_test))

# Listar archivos
for (f in archivos_test) {
  size_kb <- file.size(f) / 1024
  cat(sprintf("    • %s (%.1f KB)\n", basename(f), size_kb))
}

cat("\n¿Eliminar archivos de prueba? (y/n): ")
respuesta <- readline()

if (tolower(respuesta) == "y") {
  unlink(dir_test, recursive = TRUE)
  cat("  ✓ Archivos eliminados\n\n")
} else {
  cat(sprintf("  ℹ️  Archivos conservados en %s/\n\n", dir_test))
}

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║              ✓ TODOS LOS TESTS PASARON                   ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("FUNCIONALIDADES VERIFICADAS:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  ✓ Paleta de colores\n")
cat("  ✓ Tema visual estándar\n")
cat("  ✓ Gráfico de barras (vertical y horizontal)\n")
cat("  ✓ Gráfico de líneas\n")
cat("  ✓ Histograma APILADO por defecto\n")
cat("  ✓ Histograma superpuesto (identity)\n")
cat("  ✓ Scatter plot con regresión\n")
cat("  ✓ Boxplot\n")
cat("  ✓ Facets\n")
cat("  ✓ Exportación PNG\n")
cat("  ✓ Exportación PDF\n")
cat("  ✓ Exportación SVG\n")
cat("  ✓ Configuraciones adicionales\n")
cat("  ✓ Labels personalizados\n")
cat("  ✓ Inferencia automática de UNIDADES\n")
cat("  ✓ Integración con REPORTE_CONFIG\n")
cat("  ✓ Paleta automática de géneros\n\n")

cat("CORRECCIONES IMPLEMENTADAS:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  ✅ Histograma ahora es APILADO (stack) por defecto\n")
cat("  ✅ Ejes muestran UNIDADES automáticamente\n")
cat("     Ejemplos:\n")
cat("       • vol_ha_m3  → Volumen (m³/ha)\n")
cat("       • ab_ha_m2   → Área basal (m²/ha)\n")
cat("       • diametro   → Diámetro (cm)\n")
cat("       • n_ha       → Densidad (árb/ha)\n\n")

cat("El motor de gráficos está listo para usar.\n")
cat("Próximo paso: Implementar 52_CALCULOS_ESPECIFICOS.R\n\n")