# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 03_CONFIG_SIMULACION.R
# Parámetros del modelo de simulación forestal
# ==============================================================================

library(tidyverse)

cat("\n[3/4] Cargando configuración de simulación...\n")

# ==============================================================================
# 1. PARÁMETROS TEMPORALES
# ==============================================================================

PERIODO_SIMULACION <- 10  # años del ciclo de corta
INICIO_SIMULACION <- 2026

# ==============================================================================
# 2. GÉNEROS OBJETIVO PARA SIMULACIÓN
# ==============================================================================

GENEROS_OBJETIVO <- c("Pinus", "Quercus")

# ==============================================================================
# 3. MORTALIDAD
# ==============================================================================

MORTALIDAD_BASE <- 0.005  # 0.5% anual para árboles dominantes/codominantes
SEMILLA_MORTALIDAD <- 42  # Para reproducibilidad

# Nota: Los modificadores por dominancia están en CODIGOS_DOMINANCIA
# (factor_mortalidad: suprimidos mueren 3× más que dominantes)

# ==============================================================================
# 4. RECLUTAMIENTO
# ==============================================================================

TASA_RECLUTAMIENTO <- 0.01  # 1% de la densidad actual por año

# Rango de clases de ingreso (diámetro en cm)
RECLUTAMIENTO_D_MIN <- 7.5
RECLUTAMIENTO_D_MAX <- 8.5

# Dominancia inicial de nuevos árboles
RECLUTAMIENTO_DOMINANCIA <- 3  # Suprimido (código 3)

# Altura inicial aproximada (m) por género
RECLUTAMIENTO_ALTURA <- list(
  "Pinus" = 3.5,
  "Quercus" = 2.5
)

# ==============================================================================
# 5. CLASES DIAMÉTRICAS (PARA ANÁLISIS Y REPORTES)
# ==============================================================================

CLASES_DIAMETRICAS <- seq(5, 100, by = 5)

# ==============================================================================
# 6. PARÁMETROS DE EXPANSIÓN Y MUESTREO
# ==============================================================================

AREA_PARCELA_HA <- 0.1  # 1000 m² por sitio de muestreo
AREA_REGENERACION_M2<-9 # 9m2 son las parcelas de regeneración

# ==============================================================================
# 7. CONFIGURACIÓN COMPLETA (LISTA MAESTRA)
# ==============================================================================

crear_configuracion_simulacion <- function() {
  
  # Cargar dependencias (si no están ya cargadas)
  if (!exists("ESPECIES")) {
    source(file.path(PROYECTO_ROOT, "config/02_config_especies.R"))
  }
  if (!exists("CODIGOS_DOMINANCIA")) {
    source(file.path(PROYECTO_ROOT, "config/03_config_codigos.R"))
  }
  
  config <- list(
    # ══════════════════════════════════════════════════════════════
    # TEMPORAL
    # ══════════════════════════════════════════════════════════════
    periodo = PERIODO_SIMULACION,
    inicio = INICIO_SIMULACION,
    
    # ══════════════════════════════════════════════════════════════
    # ESPECIES
    # ══════════════════════════════════════════════════════════════
    especies = ESPECIES,
    ecuaciones_volumen = ECUACIONES_VOLUMEN,
    generos = GENEROS_OBJETIVO,
    crecimiento_base = setNames(
      CRECIMIENTO_DIAMETRICO$tasa_base_cm_año,
      CRECIMIENTO_DIAMETRICO$genero
    ),
    crecimiento_altura = setNames(
      CRECIMIENTO_DIAMETRICO$tasa_altura_m_año,
      CRECIMIENTO_DIAMETRICO$genero
    ),
    altura_maxima = setNames(
      CRECIMIENTO_DIAMETRICO$altura_max_m,
      CRECIMIENTO_DIAMETRICO$genero
    ),
    crecimiento_d_default = CRECIMIENTO_D_DEFAULT,
    crecimiento_h_default = CRECIMIENTO_H_DEFAULT,
    
    # ══════════════════════════════════════════════════════════════
    # DOMINANCIA
    # ══════════════════════════════════════════════════════════════
    modificadores_dominancia = CODIGOS_DOMINANCIA,
    
    # ══════════════════════════════════════════════════════════════
    # MORTALIDAD
    # ══════════════════════════════════════════════════════════════
    mortalidad_base = MORTALIDAD_BASE,
    semilla_mortalidad = SEMILLA_MORTALIDAD,
    
    # ══════════════════════════════════════════════════════════════
    # RECLUTAMIENTO
    # ══════════════════════════════════════════════════════════════
    tasa_reclutamiento = TASA_RECLUTAMIENTO,
    reclut_d_min = RECLUTAMIENTO_D_MIN,
    reclut_d_max = RECLUTAMIENTO_D_MAX,
    reclut_dominancia = RECLUTAMIENTO_DOMINANCIA,
    reclut_altura = RECLUTAMIENTO_ALTURA,
    
    # ══════════════════════════════════════════════════════════════
    # CLASIFICACIÓN
    # ══════════════════════════════════════════════════════════════
    clases_d = CLASES_DIAMETRICAS,
    
    # ══════════════════════════════════════════════════════════════
    # MUESTREO
    # ══════════════════════════════════════════════════════════════
    area_parcela_ha = AREA_PARCELA_HA,
    area_parcela_regeneracion_ha = AREA_REGENERACION_M2/10000,

    # ══════════════════════════════════════════════════════════════
    # UMBRALES DE VALIDACIÓN BIOLÓGICA
    # ══════════════════════════════════════════════════════════════
    umbral_delta_d_max = 1.0,  # cm/año — warning si incremento diamétrico supera esto
    umbral_delta_h_max = 0.8   # m/año  — warning si incremento en altura supera esto
   )
   
   return(config)
 }

# ==============================================================================
# 8. VALIDACIÓN
# ==============================================================================

validar_configuracion <- function(config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        VALIDACIÓN DE CONFIGURACIÓN DEL SISTEMA            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  errores <- c()
  
  # Periodo
  if (config$periodo <= 0) {
    errores <- c(errores, "✗ Periodo de simulación debe ser > 0")
  } else {
    cat(sprintf("✓ Periodo simulación: %d años (%d-%d)\n", 
                config$periodo, config$inicio, config$inicio + config$periodo - 1))
  }
  
  # Géneros
  if (length(config$generos) == 0) {
    errores <- c(errores, "✗ Debe haber al menos un género objetivo")
  } else {
    cat(sprintf("✓ Géneros objetivo: %s\n", paste(config$generos, collapse=", ")))
  }
  
  # Tasas de crecimiento
  if (any(unlist(config$crecimiento_base) <= 0)) {
    errores <- c(errores, "✗ Tasas de crecimiento deben ser > 0")
  } else {
    cat("✓ Tasas crecimiento diamétrico:\n")
    for (g in names(config$crecimiento_base)) {
      cat(sprintf("  • %s: %.2f cm/año\n", g, config$crecimiento_base[[g]]))
    }
  }
  
  # Mortalidad
  if (config$mortalidad_base < 0 || config$mortalidad_base > 1) {
    errores <- c(errores, "✗ Mortalidad base debe estar entre 0 y 1")
  } else {
    cat(sprintf("✓ Mortalidad base: %.2f%% anual\n", config$mortalidad_base * 100))
  }
  
  # Reclutamiento
  if (config$tasa_reclutamiento < 0 || config$tasa_reclutamiento > 1) {
    errores <- c(errores, "✗ Tasa reclutamiento debe estar entre 0 y 1")
  } else {
    cat(sprintf("✓ Reclutamiento: %.1f%% de densidad actual\n", 
                config$tasa_reclutamiento * 100))
    cat(sprintf("  • Rango ingreso: %.1f-%.1f cm\n", 
                config$reclut_d_min, config$reclut_d_max))
  }
  
  # # Parámetros altura
  # if (is.null(config$parametros_altura) || nrow(config$parametros_altura) == 0) {
  #   errores <- c(errores, "✗ No se cargaron parámetros altura-diámetro (CRÍTICO)")
  # } else {
  #   cat(sprintf("✓ Modelos altura-diámetro: %d especies\n", 
  #               nrow(config$parametros_altura)))
  # }
  
  # Resultado final
  cat("\n")
  if (length(errores) > 0) {
    cat("══════════════════════════════════════════════════════════\n")
    cat("❌ ERRORES ENCONTRADOS:\n")
    cat("══════════════════════════════════════════════════════════\n")
    for (e in errores) cat(sprintf("  %s\n", e))
    stop("\nConfiguración inválida. Corrige los errores antes de continuar.")
  } else {
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║           ✓ CONFIGURACIÓN VÁLIDA - SISTEMA LISTO          ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n\n")
  }
  
  return(TRUE)
}

# ==============================================================================
# RESUMEN DE CARGA
# ==============================================================================

cat(sprintf("  ✓ Periodo: %d años\n", PERIODO_SIMULACION))
cat(sprintf("  ✓ Mortalidad base: %.2f%%\n", MORTALIDAD_BASE * 100))
cat(sprintf("  ✓ Reclutamiento: %.1f%%\n", TASA_RECLUTAMIENTO * 100))
cat(sprintf("  ✓ Clases diamétricas: %d (%.0f-%.0f cm)\n",
            length(CLASES_DIAMETRICAS) - 1,
            min(CLASES_DIAMETRICAS),
            max(CLASES_DIAMETRICAS)))