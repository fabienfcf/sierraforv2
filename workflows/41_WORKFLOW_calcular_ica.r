# ==============================================================================
# SCRIPT DE EJECUCIÓN: CÁLCULO DE ICA SIN OPERACIÓN FORESTAL
# ==============================================================================
#
# Este script ejecuta el cálculo del ICA sobre 10 años sin cortes,
# generando todas las tablas necesarias para el PMF según sección 11.1.4
#
# IMPORTANTE: Este cálculo debe ejecutarse ANTES del optimizador de cortas
# para tener los valores de ICA precisos derivados del modelo poblacional.
#
# ==============================================================================

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)


# Librerías
library(tidyverse)
library(xtable)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║      CÁLCULO DE ICA - SIMULACIÓN SIN OPERACIÓN           ║\n")
cat("║           Derivado del Modelo Poblacional                ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# PASO 1: CARGAR MÓDULOS NECESARIOS
# ==============================================================================

cat("[1/5] Cargando módulos del modelo...\n\n")

source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))

# Cargar el nuevo módulo de ICA
source(file.path(PROYECTO_ROOT, "core/16_calcular_ica.R"))

cat("✓ Módulos cargados\n\n")

# ==============================================================================
# PASO 2: CARGAR DATOS INICIALES
# ==============================================================================

cat("[2/5] Cargando población inicial...\n\n")

# Verificar que existan los datos procesados
if (!file.exists("datos_intermedios/arboles_analisis.rds")) {
  cat("❌ ERROR: No se encontró arboles_analisis.rds\n")
  cat("   Ejecuta primero: source(file.path(PROYECTO_ROOT, 'workflows/40_WORKFLOW_COMPLETO.R'))\n\n")
  stop("Datos no encontrados")
}

arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("✓ Población cargada: %d árboles\n", nrow(arboles_inicial)))
cat(sprintf("  • Pinus:   %d árboles\n", 
            sum(arboles_inicial$genero_grupo == "Pinus")))
cat(sprintf("  • Quercus: %d árboles\n", 
            sum(arboles_inicial$genero_grupo == "Quercus")))
cat(sprintf("  • Rodales: %d\n\n", n_distinct(arboles_inicial$rodal)))

# ==============================================================================
# PASO 3: EJECUTAR SIMULACIÓN Y CALCULAR ICA
# ==============================================================================

cat("[3/5] Ejecutando simulación de 10 años sin cortes...\n\n")

# NOTA IMPORTANTE:
# La simulación usa ÚNICAMENTE procesos naturales:
#   - Crecimiento diamétrico anual
#   - Mortalidad natural
#   - Reclutamiento de nuevos individuos
# NO incluye ninguna operación forestal (cortas)

resultado_ica <- calcular_ica_sin_cortes(
  arboles_inicial = arboles_inicial,
  config = CONFIG,
  años = 10
)

cat("\n✓ Simulación completada\n\n")

# ==============================================================================
# PASO 4: EXPORTAR TABLAS LaTeX
# ==============================================================================

cat("[4/5] Generando tablas LaTeX para el PMF...\n\n")

exportar_tablas_latex_ica(resultado_ica, directorio = "tablas_latex")

cat("\n✓ Tablas LaTeX generadas\n\n")

# ==============================================================================
# PASO 5: GUARDAR RESULTADOS
# ==============================================================================

cat("[5/5] Guardando resultados...\n\n")

guardar_resultados_ica(resultado_ica, directorio = "resultados")

# ==============================================================================
# VISUALIZACIÓN DE RESULTADOS
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                  RESULTADOS PRINCIPALES                   ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Mostrar resumen por rodal
cat("ICA POR RODAL (primeros 5):\n")
cat("─────────────────────────────────────────────────────────────\n")
print(head(resultado_ica$ica_por_rodal, 5))
cat("\n")

# Mostrar resumen por género
cat("ICA POR GÉNERO (primeros 5):\n")
cat("─────────────────────────────────────────────────────────────\n")
print(head(resultado_ica$ica_por_genero_rodal, 5))
cat("\n")

# Mostrar resumen del predio
cat("RESUMEN GENERAL DEL PREDIO:\n")
cat("─────────────────────────────────────────────────────────────\n")
print(resultado_ica$resumen_predio)
cat("\n")

# ==============================================================================
# INFORMACIÓN PARA USO EN OPTIMIZADOR
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║          USO DE ESTOS RESULTADOS EN OPTIMIZADOR          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Los archivos CSV generados pueden ser cargados en el\n")
cat("optimizador de cortas (14_optimizador_cortas.R) para:\n\n")

cat("1. Definir el VOLUMEN OBJETIVO basado en ICA real:\n")
cat("   ica_data <- read.csv('resultados/31_ica_por_rodal.csv')\n")
cat("   vol_objetivo <- ica_data$VC_rodal_m3[rodal == X]\n\n")

cat("2. Comparar intensidad de corta propuesta vs. IC calculado:\n")
cat("   IntCor_rel_IC (de este cálculo) vs. intensidad aplicada\n\n")

cat("3. Validar sostenibilidad:\n")
cat("   Nunca cortar más del ICA calculado para cada rodal\n\n")

# ==============================================================================
# ESTADÍSTICAS FINALES
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              ESTADÍSTICAS DE LA SIMULACIÓN                ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Cambios poblacionales
n_inicial <- nrow(resultado_ica$poblacion_inicial)
n_final <- nrow(resultado_ica$poblacion_final)
cambio_n <- n_final - n_inicial
cambio_pct <- (cambio_n / n_inicial) * 100

cat(sprintf("Población inicial:      %d árboles\n", n_inicial))
cat(sprintf("Población final:        %d árboles\n", n_final))
cat(sprintf("Cambio neto:            %+d árboles (%+.1f%%)\n\n", 
            cambio_n, cambio_pct))

# Cambios volumétricos totales
vol_inicial_total <- sum(resultado_ica$poblacion_inicial$volumen_m3, na.rm = TRUE)
vol_final_total <- sum(resultado_ica$poblacion_final$volumen_m3, na.rm = TRUE)
delta_vol <- vol_final_total - vol_inicial_total
ica_promedio <- delta_vol / 10

cat(sprintf("Volumen inicial:        %.2f m³\n", vol_inicial_total))
cat(sprintf("Volumen final:          %.2f m³\n", vol_final_total))
cat(sprintf("Incremento total:       %.2f m³\n", delta_vol))
cat(sprintf("ICA promedio anual:     %.2f m³/año\n\n", ica_promedio))

# ==============================================================================
# COMPARACIÓN CON FÓRMULA TRADICIONAL
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║     VENTAJAS DEL MÉTODO POBLACIONAL VS. TRADICIONAL      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("✓ MÉTODO POBLACIONAL (este cálculo):\n")
cat("  • Incluye crecimiento individual por árbol\n")
cat("  • Considera mortalidad diferencial por tamaño\n")
cat("  • Incorpora reclutamiento de nuevos individuos\n")
cat("  • Más preciso para masas irregulares\n")
cat("  • Refleja dinámica real del bosque\n\n")

cat("  MÉTODO TRADICIONAL (fórmula interés compuesto):\n")
cat("  • Asume tasa de crecimiento constante\n")
cat("  • No considera estructura poblacional\n")
cat("  • Puede sobre/subestimar en masas irregulares\n")
cat("  • Útil para validación rápida\n\n")

# ==============================================================================
# RECOMENDACIONES
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                    RECOMENDACIONES                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("1. VALIDACIÓN:\n")
cat("   • Comparar ICA calculado con inventarios previos\n")
cat("   • Verificar congruencia con índice de sitio\n")
cat("   • Contrastar con datos de anillos de crecimiento\n\n")

cat("2. USO EN PMF:\n")
cat("   • Incluir tablas LaTeX en sección 11.1.4\n")
cat("   • Mencionar que ICA es derivado de modelo poblacional\n")
cat("   • Explicar que es más conservador que fórmula tradicional\n\n")

cat("3. AJUSTES POSIBLES:\n")
cat("   • Si ICA parece bajo: revisar tasas de crecimiento\n")
cat("   • Si ICA parece alto: revisar mortalidad_base\n")
cat("   • Sensibilidad: ejecutar con diferentes parámetros\n\n")

# ==============================================================================
# PRÓXIMOS PASOS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                    PRÓXIMOS PASOS                         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("1. Revisar tablas generadas en 'tablas_latex/'\n")
cat("2. Integrar tablas en documento LaTeX del PMF\n")
cat("3. Modificar 14_optimizador_cortas.R para usar estos ICAs\n")
cat("4. Ejecutar simulación con cortas (30_SIMULACION_10AÑOS_COMPLETA.R)\n")
cat("5. Comparar escenario con cortas vs. sin cortas\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              ✓ PROCESO COMPLETADO CON ÉXITO              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")