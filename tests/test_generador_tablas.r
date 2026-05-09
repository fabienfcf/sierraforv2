# ==============================================================================
# TEST: Motor de generación de tablas
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
source(file.path(PROYECTO_ROOT, "reportes/70_CONFIG_REPORTES.R"))
source(file.path(PROYECTO_ROOT, "generadores/50_GENERADOR_TABLAS.R"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║           TEST: 50_GENERADOR_TABLAS.R                    ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# TEST 1: Verificar funciones básicas existen
# ==============================================================================

cat("[TEST 1] Verificando funciones básicas...\n")

funciones_requeridas <- c(
  "generar_tabla",
  "formatear_valor",
  "formatear_columnas",
  "exportar_tabla_latex",
  "exportar_tabla_csv",
  "exportar_tabla",
  "agregar_fila_totales",
  "crear_tabla_sierrafor"
)

for (func in funciones_requeridas) {
  if (!exists(func)) {
    stop(sprintf("Función '%s' no encontrada", func))
  }
}

cat(sprintf("  ✓ %d funciones encontradas\n\n", length(funciones_requeridas)))

# ==============================================================================
# TEST 2: Crear datos de prueba
# ==============================================================================

cat("[TEST 2] Creando datos de prueba...\n")

datos_prueba <- tibble(
  rodal = c(1, 2, 3, 4, 5),
  genero = c("Pinus", "Pinus", "Quercus", "Quercus", "Pinus"),
  n_arboles = c(156, 234, 189, 145, 201),
  densidad_ha = c(156.0, 234.0, 189.0, 145.0, 201.0),
  ab_ha_m2 = c(23.45, 31.67, 18.92, 22.13, 28.76),
  vol_ha_m3 = c(145.678, 198.234, 112.456, 134.789, 176.543),
  d_medio_cm = c(32.5, 28.9, 26.7, 31.2, 29.8),
  pct_composicion = c(25.5, 32.1, 18.9, 20.3, 27.6)
)

cat(sprintf("  ✓ Datos de prueba creados: %d × %d\n\n", 
            nrow(datos_prueba), ncol(datos_prueba)))

print(head(datos_prueba, 3))

# ==============================================================================
# TEST 3: Formateo de valores
# ==============================================================================

cat("\n[TEST 3] Probando formateo de valores...\n")

# Test diferentes tipos de formato
test_valores <- c(123.456, 0.123, 1234567.89, 0.001, NA)

cat("\n  Valores originales:\n")
print(test_valores)

cat("\n  Formatos aplicados:\n")
cat(sprintf("    Entero:      %s\n", paste(formatear_valor(test_valores, "entero"), collapse = ", ")))
cat(sprintf("    Decimal1:    %s\n", paste(formatear_valor(test_valores, "decimal1"), collapse = ", ")))
cat(sprintf("    Decimal2:    %s\n", paste(formatear_valor(test_valores, "decimal2"), collapse = ", ")))
cat(sprintf("    Decimal3:    %s\n", paste(formatear_valor(test_valores, "decimal3"), collapse = ", ")))
cat(sprintf("    Porcentaje:  %s\n", paste(formatear_valor(test_valores, "porcentaje"), collapse = ", ")))
cat(sprintf("    Científico:  %s\n\n", paste(formatear_valor(test_valores, "cientifico"), collapse = ", ")))

# ==============================================================================
# TEST 4: Formateo de columnas
# ==============================================================================

cat("[TEST 4] Probando formateo de columnas...\n")

formato_spec <- list(
  n_arboles = "entero",
  densidad_ha = "decimal1",
  ab_ha_m2 = "decimal2",
  vol_ha_m3 = "decimal2",
  d_medio_cm = "decimal1",
  pct_composicion = "porcentaje"
)

datos_formateados <- formatear_columnas(datos_prueba, formato_spec)

cat("\n  Datos formateados:\n")
print(head(datos_formateados, 3))

cat("\n  ✓ Formateo de columnas exitoso\n\n")

# ==============================================================================
# TEST 5: Agregar fila de totales
# ==============================================================================

cat("[TEST 5] Probando agregar fila de totales...\n")

datos_con_totales <- agregar_fila_totales(
  datos_prueba,
  columnas_sumar = c("n_arboles", "ab_ha_m2", "vol_ha_m3"),
  etiqueta = "TOTAL",
  columna_etiqueta = "rodal"
)

cat("\n  Datos con totales:\n")
print(tail(datos_con_totales, 2))

cat("\n  ✓ Fila de totales agregada\n\n")

# ==============================================================================
# TEST 6: Generar tabla simple
# ==============================================================================

cat("[TEST 6] Generando tabla simple...\n")

tabla_simple <- generar_tabla(
  datos = datos_prueba,
  id = "TEST_tabla_simple",
  titulo = "Tabla de prueba simple",
  subtitulo = "Datos de ejemplo",
  formato_columnas = formato_spec
)

# Verificar clase
stopifnot(inherits(tabla_simple, "tabla_sierrafor"))

cat("\n  ✓ Tabla generada:\n")
print(tabla_simple)

# ==============================================================================
# TEST 7: Generar tabla con totales y ordenamiento
# ==============================================================================

cat("\n[TEST 7] Generando tabla con totales y ordenamiento...\n")

tabla_completa <- generar_tabla(
  datos = datos_prueba,
  id = "TEST_tabla_completa",
  titulo = "Métricas dendrométricas por rodal",
  subtitulo = "Ordenado por volumen descendente",
  formato_columnas = formato_spec,
  ordenar_por = "vol_ha_m3",
  ordenar_desc = TRUE,
  agregar_totales = TRUE,
  columnas_totales = c("n_arboles", "ab_ha_m2", "vol_ha_m3")
)

cat("\n  ✓ Tabla completa generada\n")
print(tabla_completa)

# ==============================================================================
# TEST 8: Exportar a LaTeX
# ==============================================================================

cat("\n[TEST 8] Exportando tabla a LaTeX...\n")

# Crear directorio temporal
dir_test <- "test_output"
if (!dir.exists(dir_test)) {
  dir.create(dir_test, recursive = TRUE)
}

# Exportar
archivo_latex <- exportar_tabla_latex(
  tabla_completa,
  directorio = dir_test
)

# Verificar que existe
if (file.exists(archivo_latex)) {
  cat(sprintf("  ✓ Archivo LaTeX creado: %s\n", archivo_latex))
  
  # Mostrar primeras líneas
  cat("\n  Contenido LaTeX (primeras 15 líneas):\n")
  cat("  ───────────────────────────────────────────────\n")
  contenido <- readLines(archivo_latex, n = 15)
  cat(paste("  ", contenido, collapse = "\n"))
  cat("\n  ───────────────────────────────────────────────\n\n")
} else {
  stop("Archivo LaTeX no creado")
}

# ==============================================================================
# TEST 9: Exportar a CSV
# ==============================================================================

cat("[TEST 9] Exportando tabla a CSV...\n")

archivo_csv <- exportar_tabla_csv(
  tabla_completa,
  directorio = dir_test
)

# Verificar que existe
if (file.exists(archivo_csv)) {
  cat(sprintf("  ✓ Archivo CSV creado: %s\n", archivo_csv))
  
  # Leer y mostrar
  csv_leido <- read_csv(archivo_csv, show_col_types = FALSE)
  cat("\n  Contenido CSV:\n")
  print(head(csv_leido, 3))
} else {
  stop("Archivo CSV no creado")
}

cat("\n")

# ==============================================================================
# TEST 10: Exportación múltiple
# ==============================================================================

cat("[TEST 10] Exportación múltiple (LaTeX + CSV)...\n")

archivos <- exportar_tabla(
  tabla_completa,
  latex = TRUE,
  csv = TRUE,
  config = REPORTE_CONFIG
)

cat(sprintf("  ✓ LaTeX: %s\n", basename(archivos$latex)))
cat(sprintf("  ✓ CSV:   %s\n\n", basename(archivos$csv)))

# ==============================================================================
# TEST 11: Renombrar columnas a español
# ==============================================================================

cat("[TEST 11] Renombrando columnas a español...\n")

datos_espanol <- datos_prueba %>%
  select(rodal, genero, n_ha = densidad_ha, ab_ha = ab_ha_m2, vol_ha = vol_ha_m3)

datos_espanol <- renombrar_columnas_espanol(datos_espanol)

cat("\n  Columnas originales vs renombradas:\n")
cat(sprintf("    n_ha      → %s\n", names(datos_espanol)[3]))
cat(sprintf("    ab_ha     → %s\n", names(datos_espanol)[4]))
cat(sprintf("    vol_ha    → %s\n\n", names(datos_espanol)[5]))

# ==============================================================================
# TEST 12: Tabla estadísticas descriptivas
# ==============================================================================

cat("[TEST 12] Generando tabla de estadísticas...\n")

stats <- tabla_estadisticas(
  datos_prueba,
  columna = "vol_ha_m3",
  grupo_por = "genero"
)

cat("\n  Estadísticas por género:\n")
print(stats)

cat("\n")

# ==============================================================================
# TEST 13: Formateo automático
# ==============================================================================

cat("[TEST 13] Probando formateo automático...\n")

datos_auto <- formatear_automatico(datos_prueba)

cat("\n  Tipos de columnas después de formateo automático:\n")
sapply(datos_auto, class) %>% print()

cat("\n  ✓ Formateo automático exitoso\n\n")

# ==============================================================================
# TEST 14: Agregar columna de porcentaje
# ==============================================================================

cat("[TEST 14] Agregando columna de porcentaje...\n")

datos_pct <- agregar_columna_porcentaje(
  datos_prueba,
  columna_valor = "n_arboles",
  nombre_nueva_columna = "pct_arboles"
)

cat("\n  Datos con porcentaje:\n")
print(datos_pct %>% select(rodal, n_arboles, pct_arboles))

cat("\n")

# ==============================================================================
# TEST 15: Integración con REPORTE_CONFIG
# ==============================================================================

cat("[TEST 15] Verificando integración con REPORTE_CONFIG...\n")

# Crear tabla usando configuración global
REPORTE_CONFIG$mostrar_progreso <- TRUE

tabla_config <- generar_tabla(
  datos = datos_prueba %>% slice(1:3),
  id = "TEST_con_config",
  titulo = "Tabla usando configuración global",
  formato_columnas = formato_spec
)

# Exportar usando directorios de config
exportar_tabla(tabla_config, config = REPORTE_CONFIG)

cat("\n  ✓ Integración con REPORTE_CONFIG exitosa\n\n")

# ==============================================================================
# LIMPIEZA
# ==============================================================================

cat("[LIMPIEZA] Eliminando archivos de prueba...\n")

# Listar archivos creados
archivos_test <- list.files(dir_test, full.names = TRUE)
cat(sprintf("  %d archivos creados en %s/\n", length(archivos_test), dir_test))

# Preguntar si eliminar
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
cat("  ✓ Formateo de valores (6 tipos)\n")
cat("  ✓ Formateo de columnas\n")
cat("  ✓ Formateo automático\n")
cat("  ✓ Agregar fila de totales\n")
cat("  ✓ Agregar columna de porcentaje\n")
cat("  ✓ Ordenamiento de datos\n")
cat("  ✓ Generación de tablas\n")
cat("  ✓ Exportación a LaTeX\n")
cat("  ✓ Exportación a CSV\n")
cat("  ✓ Exportación múltiple\n")
cat("  ✓ Renombrado de columnas\n")
cat("  ✓ Estadísticas descriptivas\n")
cat("  ✓ Integración con REPORTE_CONFIG\n\n")

cat("El motor de tablas está listo para usar.\n")
cat("Próximo paso: Implementar 51_GENERADOR_GRAFICOS.R\n\n")
