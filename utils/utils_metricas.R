# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO DE UTILIDADES: FUNCIONES DE CÁLCULO DE MÉTRICAS COMPARTIDAS
# Centraliza el cálculo de métricas para evitar duplicación
# VERSIÓN CORREGIDA: Método de densidad global con n_sitios FIJO
# ==============================================================================

library(tidyverse)

# Cargar core_calculos si no está disponible
if (!exists("filtrar_arboles_vivos")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# ==============================================================================
# MÉTODO CORRECTO: DENSIDAD GLOBAL POR UMM CON N_SITIOS FIJO
# ==============================================================================
#
# CORRECCIÓN CRÍTICA:
#   n_sitios debe venir de num_muestreos_realizados (FIJO desde inventario)
#   NO de n_distinct(muestreo) que cambia si todos los árboles de un sitio mueren
#
# EJEMPLO:
#   UMM 1: 16 sitios inventoriados (num_muestreos_realizados = 16)
#   Año 0: 500 árboles repartidos en 16 sitios → densidad = 500 / 1.6 ha
#   Año 5: Sitio 3 pierde todos sus árboles → SIGUE siendo 16 sitios
#   Año 5: 480 árboles → densidad = 480 / 1.6 ha
#
# ==============================================================================

#' @title Calcular métricas dasométricas con n_sitios FIJO
#' @description Función genérica que usa num_muestreos_realizados (constante)
#' @param arboles_df Data frame de árboles
#' @param agrupar_por Vector de columnas para agrupar (ej: c("rodal", "genero_grupo"))
#' @param config Configuración del sistema
#' @return Data frame con métricas calculadas correctamente
calcular_metricas <- function(arboles_df, agrupar_por = NULL, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # PASO 1: Obtener n_sitios FIJOS por rodal desde num_muestreos_realizados
  # ========================================================================
  # CRÍTICO: Usar first() sobre TODOS los árboles (vivos + muertos)
  # para garantizar que n_sitios no cambie con el tiempo
  
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(
      n_sitios_fijo = first(num_muestreos_realizados),
      .groups = "drop"
    )
  
  # Verificar que existe num_muestreos_realizados
  if (!"num_muestreos_realizados" %in% names(arboles_df)) {
    stop("❌ Columna 'num_muestreos_realizados' no encontrada en arboles_df")
  }
  
  # PASO 2: Si no hay agrupación, calcular global
  # ==============================================
  if (is.null(agrupar_por) || length(agrupar_por) == 0) {
    n_sitios_global <- sum(sitios_por_rodal$n_sitios_fijo)
    
    metricas <- vivos %>%
      summarise(
        n_sitios = n_sitios_global,
        n_arboles = n(),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # PASO 3: Calcular por grupo
    # ===========================
    metricas <- vivos %>%
      group_by(across(all_of(agrupar_por))) %>%
      summarise(
        n_arboles = n(),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        .groups = "drop"
      )
    
    # PASO 4: Unir con n_sitios_fijo del rodal
    # =========================================
    if ("rodal" %in% agrupar_por) {
      metricas <- metricas %>%
        left_join(sitios_por_rodal, by = "rodal") %>%
        rename(n_sitios = n_sitios_fijo)
    } else {
      # Si no agrupa por rodal, usar total global
      metricas$n_sitios <- sum(sitios_por_rodal$n_sitios_fijo)
    }
  }
  
  # PASO 5: Calcular métricas /ha con n_sitios FIJO
  # ================================================
  metricas <- metricas %>%
    mutate(
      area_total_ha = n_sitios * config$area_parcela_ha,
      
      # DENSIDAD GLOBAL: n_arboles / (n_sitios_FIJOS × 0.1)
      densidad_ha = n_arboles / area_total_ha,
      
      # AB GLOBAL: suma_AB / (n_sitios_FIJOS × 0.1)
      ab_ha_m2 = ab_total_m2 / area_total_ha,
      
      # VOL GLOBAL: suma_Vol / (n_sitios_FIJOS × 0.1)
      vol_ha_m3 = vol_total_m3 / area_total_ha,
      
      # Columnas adicionales para compatibilidad
      n_vivos = n_arboles,
      vol_muestreado_m3 = vol_total_m3,
      ab_muestreada_m2 = ab_total_m2
    )
  
  return(metricas)
}

# ==============================================================================
# WRAPPERS ESPECÍFICOS (para compatibilidad con código existente)
# ==============================================================================

#' @title Calcular métricas de estado por rodal
#' @description Métricas globales por UMM (n_sitios FIJO desde inventario)
calcular_metricas_estado <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = "rodal", config = config)
}

#' @title Calcular métricas por género
#' @description Métricas por género y UMM (n_sitios FIJO desde inventario)
calcular_metricas_por_genero <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo"), config = config)
}

#' @title Calcular métricas por especie
#' @description Métricas por especie y UMM (n_sitios FIJO desde inventario)
calcular_metricas_por_especie <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo", "nombre_cientifico"), config = config)
}

#' @title Calcular métricas globales del predio
#' @description Métricas a nivel de todo el predio
calcular_metricas_predio <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = NULL, config = config)
}

# ==============================================================================
# VALIDACIÓN DE MÉTODO
# ==============================================================================

#' Validar que n_sitios permanece constante en el tiempo
#'
#' @param arboles_df Data frame de árboles
#' @param config Configuración
#' @return Logical indicando si la validación pasó
validar_n_sitios_constante <- function(arboles_df, config = CONFIG) {
  
  cat("\n[VALIDACIÓN] n_sitios debe ser constante...\n")
  
  # Verificar que num_muestreos_realizados existe
  if (!"num_muestreos_realizados" %in% names(arboles_df)) {
    cat("  ✗ ERROR: Columna num_muestreos_realizados no encontrada\n")
    return(FALSE)
  }
  
  # Verificar que es constante por rodal
  test <- arboles_df %>%
    group_by(rodal) %>%
    summarise(
      n_valores_unicos = n_distinct(num_muestreos_realizados),
      valor = first(num_muestreos_realizados),
      .groups = "drop"
    )
  
  if (any(test$n_valores_unicos != 1)) {
    cat("  ✗ ERROR: num_muestreos_realizados NO es constante por rodal\n")
    print(test)
    return(FALSE)
  }
  
  cat("  ✓ num_muestreos_realizados es constante por rodal\n")
  
  # Mostrar valores
  cat("\n  Valores por rodal:\n")
  for (i in 1:nrow(test)) {
    cat(sprintf("    Rodal %d: %d sitios\n", test$rodal[i], test$valor[i]))
  }
  
  return(TRUE)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de métricas compartidas cargado (N_SITIOS FIJO)\n")
cat("══════════════════════════════════════════════════════════\n")
cat("  MÉTODO CORRECTO:\n")
cat("    • n_sitios = num_muestreos_realizados (FIJO desde inventario)\n")
cat("    • densidad_ha = n_arboles / (n_sitios_FIJOS × 0.1 ha)\n")
cat("    • NO cambia si un sitio pierde todos sus árboles\n\n")
cat("  Funciones disponibles:\n")
cat("    - calcular_metricas(arboles_df, agrupar_por, config)\n")
cat("    - calcular_metricas_estado(arboles_df, config)\n")
cat("    - calcular_metricas_por_genero(arboles_df, config)\n")
cat("    - calcular_metricas_por_especie(arboles_df, config)\n")
cat("    - calcular_metricas_predio(arboles_df, config)\n")
cat("    - validar_n_sitios_constante(arboles_df, config)\n")
cat("══════════════════════════════════════════════════════════\n\n")