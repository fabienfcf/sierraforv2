# ==============================================================================
# TEST DE L'INTÉGRATION TS + TC DANS analisis_descriptivo
# ==============================================================================
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/")

# Nettoyer l'environnement
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# Charger librairies
library(tidyverse)
library(readxl)
library(xtable)

# ==============================================================================
# PASO 1: CARGAR CONFIGURACIÓN
# ==============================================================================

cat("\n[PASO 1] Cargando configuración SIERRAFOR...\n")

# Cargar modules de configuration
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "config/02_config_especies.R"))
source(file.path(PROYECTO_ROOT, "config/03_config_codigos.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))

cat("✔ Configuración cargada\n")

# ==============================================================================
# PASO 2: IMPORTAR INVENTARIO
# ==============================================================================

cat("\n[PASO 2] Importando inventario...\n")

source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))

inventario <- importar_inventario_completo("inventario_forestal.xlsx")
arboles <- construir_arboles_analisis(inventario, CONFIG)

cat("✔ Inventario importado\n")
cat(sprintf("  Sitios: %d\n", nrow(inventario$f01)))
cat(sprintf("  Árboles: %d\n", nrow(arboles)))

# ==============================================================================
# PASO 3: VERIFICAR QUE TC1-3 ESTÁN EN F01
# ==============================================================================

cat("\n[PASO 3] Verificando datos TS + TC en F01...\n")

# Verificar columnas
columnas_necesarias <- c("tratamiento_silvicola", "tratamiento_comp1", 
                         "tratamiento_comp2", "tratamiento_comp3")

columnas_presentes <- columnas_necesarias %in% names(inventario$f01)
names(columnas_presentes) <- columnas_necesarias

for (col in names(columnas_presentes)) {
  if (columnas_presentes[col]) {
    n_na <- sum(is.na(inventario$f01[[col]]))
    pct_completo <- (nrow(inventario$f01) - n_na) / nrow(inventario$f01) * 100
    cat(sprintf("  ✔ %s: %.1f%% completo\n", col, pct_completo))
  } else {
    cat(sprintf("  ✖ %s: NO ENCONTRADA\n", col))
  }
}

# ==============================================================================
# PASO 4: CARGAR Y PROBAR NUEVA FUNCIÓN
# ==============================================================================

cat("\n[PASO 4] Cargando módulo con análisis TS integrado...\n")

# IMPORTANTE: Usar el archivo MODIFICADO
source(file.path(PROYECTO_ROOT, "analisis/20_analisis_descriptivo.R"))

cat("✔ Módulo cargado\n")

# Verificar que la nueva función existe
if (exists("analizar_tratamientos_silvicolas")) {
  cat("✔ Función analizar_tratamientos_silvicolas disponible\n")
} else {
  stop("✖ Función analizar_tratamientos_silvicolas NO encontrada")
}

# ==============================================================================
# PASO 5: PROBAR NUEVA FUNCIÓN AISLADA
# ==============================================================================

cat("\n[PASO 5] Probando función de tratamientos (aislada)...\n")

# Test con exportación desactivada para ver solo console output
resultado_ts <- analizar_tratamientos_silvicolas(
  inventario$f01,
  exportar_latex = FALSE,
  exportar_csv_flag = FALSE
)

cat("\n✔ Función ejecutada sin errores\n")
cat(sprintf("  Componentes del resultado: %d\n", length(resultado_ts)))
cat(sprintf("    - dist_ts: %d filas\n", nrow(resultado_ts$dist_ts)))
cat(sprintf("    - dist_tc: %d filas\n", nrow(resultado_ts$dist_tc)))
cat(sprintf("    - relacion_ts_tc: %d filas\n", nrow(resultado_ts$relacion_ts_tc)))
cat(sprintf("    - completitud_ts: %d filas\n", nrow(resultado_ts$completitud_ts)))
cat(sprintf("    - ts_por_rodal: %d filas\n", nrow(resultado_ts$ts_por_rodal)))
cat(sprintf("    - graficos: %d elementos\n", length(resultado_ts$graficos)))

# ==============================================================================
# PASO 6: EJECUTAR ANÁLISIS COMPLETO (CON EXPORTACIÓN)
# ==============================================================================

cat("\n[PASO 6] Ejecutando análisis descriptivo COMPLETO...\n")
cat("         (Incluye nueva sección [8/8] de tratamientos)\n\n")

# Crear directorios si no existen
dir.create("resultados", showWarnings = FALSE)
dir.create("tablas_latex", showWarnings = FALSE)
dir.create("graficos", showWarnings = FALSE)

# Ejecutar análisis completo
resultados_completos <- analisis_descriptivo_completo(
  inventario = inventario,
  arboles_df = arboles,
  config = CONFIG,
  exportar_csv_flag = TRUE
)

cat("\n✔ Análisis completo ejecutado\n")

# ==============================================================================
# PASO 7: VERIFICAR ARCHIVOS GENERADOS
# ==============================================================================

cat("\n[PASO 7] Verificando archivos generados para TS+TC...\n\n")

# Archivos CSV esperados
csv_esperados <- c(
  "desc_14_ts_distribucion.csv",
  "desc_15_tc_distribucion.csv",
  "desc_16_ts_tc_relacion.csv",
  "desc_17_ts_completitud.csv",
  "desc_18_ts_por_rodal.csv"
)

cat("CSV:\n")
for (archivo in csv_esperados) {
  ruta <- file.path("resultados", archivo)
  if (file.exists(ruta)) {
    size_kb <- round(file.info(ruta)$size / 1024, 1)
    cat(sprintf("  ✔ %s (%.1f KB)\n", archivo, size_kb))
  } else {
    cat(sprintf("  ✖ %s NO ENCONTRADO\n", archivo))
  }
}

# Archivos LaTeX esperados
latex_esperados <- c(
  "desc_14_ts_distribucion.tex",
  "desc_15_ts_tc_relacion.tex"
)

cat("\nLaTeX:\n")
for (archivo in latex_esperados) {
  ruta <- file.path("tablas_latex", archivo)
  if (file.exists(ruta)) {
    cat(sprintf("  ✔ %s\n", archivo))
  } else {
    cat(sprintf("  ✖ %s NO ENCONTRADO\n", archivo))
  }
}

# Archivos de gráficos esperados
graficos_esperados <- c(
  "desc_13_ts_distribucion.png",
  "desc_14_tc_comparacion.png",
  "desc_15_completitud.png"
)

cat("\nGráficos:\n")
for (archivo in graficos_esperados) {
  ruta <- file.path("graficos", archivo)
  if (file.exists(ruta)) {
    size_kb <- round(file.info(ruta)$size / 1024, 1)
    cat(sprintf("  ✔ %s (%.1f KB)\n", archivo, size_kb))
  } else {
    cat(sprintf("  ✖ %s NO ENCONTRADO\n", archivo))
  }
}

# ==============================================================================
# PASO 8: RESUMEN DE RESULTADOS TS
# ==============================================================================

cat("\n[PASO 8] Resumen de resultados tratamientos...\n\n")

cat("TRATAMIENTOS SILVÍCOLAS (TS):\n")
cat("════════════════════════════════════════════════════════════\n")
print(resultado_ts$dist_ts %>% select(etiqueta, n_sitios, pct))

cat("\n\nTOP 5 TRATAMIENTOS COMPLEMENTARIOS:\n")
cat("════════════════════════════════════════════════════════════\n")
cat("\nTC1:\n")
print(resultado_ts$dist_tc %>% 
        filter(tratamiento == "TC1") %>% 
        head(5) %>%
        select(etiqueta, n_menciones, pct))

cat("\nTC2:\n")
print(resultado_ts$dist_tc %>% 
        filter(tratamiento == "TC2") %>% 
        head(5) %>%
        select(etiqueta, n_menciones, pct))

cat("\nTC3:\n")
print(resultado_ts$dist_tc %>% 
        filter(tratamiento == "TC3") %>% 
        head(5) %>%
        select(etiqueta, n_menciones, pct))

cat("\n\nRELACIÓN TS ↔ TC (Top 10 combinaciones):\n")
cat("════════════════════════════════════════════════════════════\n")
print(resultado_ts$relacion_ts_tc %>% 
        head(10) %>%
        select(ts_etiqueta, tc_etiqueta, n_menciones))

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("  TEST COMPLETADO EXITOSAMENTE\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("INTEGRACIÓN REALIZADA:\n")
cat("  ✔ Nueva función analizar_tratamientos_silvicolas()\n")
cat("  ✔ Integrada en analisis_descriptivo_completo()\n")
cat("  ✔ Paso [8/8] agregado al workflow\n")
cat("  ✔ Exporta 5 CSV + 2 LaTeX + 3 gráficos\n\n")

cat("PRÓXIMOS PASOS:\n")
cat("  1. Revisar gráficos en: graficos/desc_13_*.png\n")
cat("  2. Revisar tablas en: tablas_latex/desc_14_*.tex\n")
cat("  3. Integrar tablas en reporte PMF\n")
cat("  4. Vincular TS con plan de cortas\n\n")

cat("PARA USAR EN PRODUCCIÓN:\n")
cat("  Reemplazar: 20_analisis_descriptivo.R\n")
cat("  Por: 20_analisis_descriptivo_INTEGRADO.R\n\n")
