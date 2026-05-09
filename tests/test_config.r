# ==============================================================================
# TEST: Verificar configuración de reportes
# ==============================================================================

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# Cargar configuración
source(file.path(PROYECTO_ROOT, "reportes/70_CONFIG_REPORTES.R"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║              TEST: 70_CONFIG_REPORTES.R                  ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# TEST 1: Verificar estructura básica
# ==============================================================================

cat("[TEST 1] Verificando estructura de REPORTE_CONFIG...\n")

stopifnot(
  "REPORTE_CONFIG existe" = exists("REPORTE_CONFIG"),
  "Es una lista" = is.list(REPORTE_CONFIG),
  "Tiene version" = "version" %in% names(REPORTE_CONFIG),
  "Tiene secciones" = any(grepl("^seccion_", names(REPORTE_CONFIG)))
)

cat("  ✓ Estructura básica correcta\n\n")

# ==============================================================================
# TEST 2: Verificar secciones
# ==============================================================================

cat("[TEST 2] Verificando secciones configuradas...\n")

secciones <- c("seccion_1_estado_inicial",
               "seccion_2_dinamica",
               "seccion_3_manejo",
               "seccion_5_sintesis")

for (sec in secciones) {
  stopifnot(sec %in% names(REPORTE_CONFIG))
  stopifnot("activar" %in% names(REPORTE_CONFIG[[sec]]))
  stopifnot("descripcion" %in% names(REPORTE_CONFIG[[sec]]))
  cat(sprintf("  ✓ %s\n", sec))
}

cat("\n")

# ==============================================================================
# TEST 3: Verificar funciones auxiliares
# ==============================================================================

cat("[TEST 3] Verificando funciones auxiliares...\n")

# Test tabla_activada()
test_T1_1 <- tabla_activada("seccion_1_estado_inicial", "T1_1_resumen_predio")
stopifnot("tabla_activada() funciona" = is.logical(test_T1_1))
cat(sprintf("  ✓ tabla_activada() = %s\n", test_T1_1))

# Test grafico_activado()
test_G1_2 <- grafico_activado("seccion_1_estado_inicial", "G1_2_abundancia_especies")
stopifnot("grafico_activado() funciona" = is.logical(test_G1_2))
cat(sprintf("  ✓ grafico_activado() = %s\n", test_G1_2))

# Test obtener_tablas_activadas()
tablas <- obtener_tablas_activadas()
stopifnot("obtener_tablas_activadas() retorna vector" = is.character(tablas))
cat(sprintf("  ✓ obtener_tablas_activadas() = %d tablas\n", length(tablas)))

# Test obtener_graficos_activados()
graficos <- obtener_graficos_activados()
stopifnot("obtener_graficos_activados() retorna vector" = is.character(graficos))
cat(sprintf("  ✓ obtener_graficos_activados() = %d gráficos\n", length(graficos)))

cat("\n")

# ==============================================================================
# TEST 4: Imprimir resumen
# ==============================================================================

cat("[TEST 4] Imprimiendo resumen de configuración...\n\n")

imprimir_resumen_config()

# ==============================================================================
# TEST 5: Verificar conteos
# ==============================================================================

cat("\n[TEST 5] Verificando conteos detallados...\n\n")

cat("SECCIÓN 1 - ESTADO INICIAL:\n")
cat("───────────────────────────────────────────────────────────\n")

sec1 <- REPORTE_CONFIG$seccion_1_estado_inicial

# Contar tablas sección 1
n_tablas_sec1 <- sum(unlist(sec1$tablas))
cat(sprintf("  Tablas:   %d/%d activadas\n", n_tablas_sec1, length(sec1$tablas)))

# Contar gráficos sección 1
n_graficos_sec1 <- sum(unlist(sec1$graficos))
cat(sprintf("  Gráficos: %d/%d activados\n\n", n_graficos_sec1, length(sec1$graficos)))

cat("SECCIÓN 2 - DINÁMICA:\n")
cat("───────────────────────────────────────────────────────────\n")

sec2 <- REPORTE_CONFIG$seccion_2_dinamica

n_tablas_sec2 <- sum(unlist(sec2$tablas))
cat(sprintf("  Tablas:   %d/%d activadas\n", n_tablas_sec2, length(sec2$tablas)))

n_graficos_sec2 <- sum(unlist(sec2$graficos))
cat(sprintf("  Gráficos: %d/%d activados\n\n", n_graficos_sec2, length(sec2$graficos)))

cat("SECCIÓN 3 - MANEJO:\n")
cat("───────────────────────────────────────────────────────────\n")

sec3 <- REPORTE_CONFIG$seccion_3_manejo

n_tablas_nom <- sum(unlist(sec3$tablas_nom152))
n_tablas_comp <- sum(unlist(sec3$tablas_complementarias))
n_tablas_imp <- sum(unlist(sec3$tablas_impacto))
n_tablas_sec3 <- n_tablas_nom + n_tablas_comp + n_tablas_imp

cat(sprintf("  Tablas NOM-152:        %d/%d activadas\n", n_tablas_nom, length(sec3$tablas_nom152)))
cat(sprintf("  Tablas complementarias: %d/%d activadas\n", n_tablas_comp, length(sec3$tablas_complementarias)))
cat(sprintf("  Tablas impacto:         %d/%d activadas\n", n_tablas_imp, length(sec3$tablas_impacto)))
cat(sprintf("  TOTAL tablas sección 3: %d\n", n_tablas_sec3))

n_graficos_sec3 <- sum(unlist(sec3$graficos))
cat(sprintf("  Gráficos: %d/%d activados\n\n", n_graficos_sec3, length(sec3$graficos)))

cat("SECCIÓN 5 - SÍNTESIS:\n")
cat("───────────────────────────────────────────────────────────\n")

sec5 <- REPORTE_CONFIG$seccion_5_sintesis

n_tablas_sec5 <- sum(unlist(sec5$tablas))
cat(sprintf("  Tablas:   %d/%d activadas\n", n_tablas_sec5, length(sec5$tablas)))

n_graficos_sec5 <- sum(unlist(sec5$graficos))
cat(sprintf("  Gráficos: %d/%d activados\n\n", n_graficos_sec5, length(sec5$graficos)))

# ==============================================================================
# TEST 6: Listar todas las tablas y gráficos activados
# ==============================================================================

cat("\n[TEST 6] Listado completo de elementos activados...\n\n")

cat("TABLAS ACTIVADAS:\n")
cat("═══════════════════════════════════════════════════════════\n")
for (i in seq_along(tablas)) {
  cat(sprintf("  %2d. %s\n", i, tablas[i]))
}

cat("\nGRÁFICOS ACTIVADOS:\n")
cat("═══════════════════════════════════════════════════════════\n")
for (i in seq_along(graficos)) {
  cat(sprintf("  %2d. %s\n", i, graficos[i]))
}

# ==============================================================================
# TEST 7: Verificar configuración avanzada
# ==============================================================================

cat("\n[TEST 7] Verificando configuración avanzada...\n")

stopifnot(
  "Tiene configuración avanzada" = "avanzado" %in% names(REPORTE_CONFIG),
  "Tiene umbrales" = "umbrales" %in% names(REPORTE_CONFIG$avanzado),
  "Tiene decimales" = "decimales" %in% names(REPORTE_CONFIG$avanzado)
)

cat(sprintf("  ✓ Tolerancia cero: %.2e\n", REPORTE_CONFIG$avanzado$tolerancia_cero))
cat(sprintf("  ✓ Umbral productividad alta: %.0f m³/ha\n", 
            REPORTE_CONFIG$avanzado$umbrales$productividad_alta))
cat(sprintf("  ✓ Umbral intensidad corta alta: %.0f%%\n", 
            REPORTE_CONFIG$avanzado$umbrales$intensidad_corta_alta))

cat("\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║              ✓ TODOS LOS TESTS PASARON                   ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("RESUMEN:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  Total tablas activadas:   %d\n", length(tablas)))
cat(sprintf("  Total gráficos activados: %d\n", length(graficos)))
cat(sprintf("  Total elementos:          %d\n\n", length(tablas) + length(graficos)))

cat("DISTRIBUCIÓN POR SECCIÓN:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  Sección 1: %2d tablas + %2d gráficos = %2d\n", 
            n_tablas_sec1, n_graficos_sec1, n_tablas_sec1 + n_graficos_sec1))
cat(sprintf("  Sección 2: %2d tablas + %2d gráficos = %2d\n", 
            n_tablas_sec2, n_graficos_sec2, n_tablas_sec2 + n_graficos_sec2))
cat(sprintf("  Sección 3: %2d tablas + %2d gráficos = %2d\n", 
            n_tablas_sec3, n_graficos_sec3, n_tablas_sec3 + n_graficos_sec3))
cat(sprintf("  Sección 5: %2d tablas + %2d gráficos = %2d\n\n", 
            n_tablas_sec5, n_graficos_sec5, n_tablas_sec5 + n_graficos_sec5))

cat("El archivo 70_CONFIG_REPORTES.R está listo para usar.\n")
cat("Próximo paso: Implementar 50_GENERADOR_TABLAS.R\n\n")

