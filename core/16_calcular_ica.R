# Establecer directorio raÃ­z del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# CÃLCULO DE ICA Y VARIABLES SECCIÃ“N 11.1.4 DEL MANUAL PMF
# ==============================================================================
# 
# Este módulo calcula el ICA sobre 10 años SIN operaciÃ³n forestal para obtener
# valores de crecimiento reales derivados del modelo poblacional.
#
# Calcula todas las variables mencionadas en la secciÃ³n 11.1.4 del Manual PMF:
#   - Sup (ha): Superficie por rodal
#   - IS (m): Ãndice de Sitio  
#   - ER (mÂ³/ha): Existencias Reales al inicio
#   - ICA (mÂ³/ha): Incremento Corriente Anual
#   - ICA Rel (i): Incremento relativo = ICA/ER
#   - IntCor Rel (IC): Intensidad de corta relativa = 1 - 1/(1+i)^cc
#   - VC/ha (mÂ³): Volumen de corta por ha = IC * ER
#   - ER/rodal (mÂ³): Existencias reales por rodal
#   - VC/rodal (mÂ³): Volumen de corta por rodal
#
# Estos cálculos se realizan por:
#   - Especie (géneros Pinus y Quercus)
#   - Género 
#   - Rodal (UMM)
#   - Total del predio
#
# ==============================================================================

library(tidyverse)
library(xtable)

# Cargar utilidades compartidas si no estÃ¡n disponibles
if (!exists("calcular_metricas_estado")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_metricas.R"))
}

# ==============================================================================
# FUNCIÃ“N PRINCIPAL: SIMULAR SIN CORTES Y CALCULAR ICA
# ==============================================================================

calcular_ica_sin_cortes <- function(arboles_inicial, config = CONFIG, años = 10) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘         CÃLCULO DE ICA - SIMULACIÃ“N SIN CORTES           â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  cat(sprintf("â€¢ PerÃ­odo de simulación: %d años\n", años))
  cat(sprintf("â€¢ PoblaciÃ³n inicial: %d árboles\n", nrow(arboles_inicial)))
  cat("â€¢ MÃ©todos: crecimiento + mortalidad + reclutamiento\n")
  cat("â€¢ Sin operaciones forestales\n\n")
  
  # Verificar que los módulos estÃ©n cargados
  if (!exists("aplicar_crecimiento_poblacion")) {
    stop("âŒ MÃ³dulos de simulación no cargados. Ejecuta 40_WORKFLOW_COMPLETO.R primero")
  }
  
  # ===========================================================================
  # 1. SIMULACIÃ“N SIN CORTES
  # ===========================================================================
  
  cat("[PASO 1/4] Simulando crecimiento sin cortes...\n\n")
  
  arboles_actual <- arboles_inicial
  historial_completo <- list()
  
  # Guardar estado inicial
  historial_completo[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
  
  # Simular año por año (solo procesos naturales)
  for (año in 1:años) {
    
    cat(sprintf("  â””â”€ AÃ±o %d/%d\n", año, años))
    
    # Crecimiento
    arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, config, año)
    
    # Mortalidad natural
    arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, config, año)
    
    # Reclutamiento
    arboles_actual <- aplicar_reclutamiento(arboles_actual, config, año)
    
    # Guardar estado
    historial_completo[[año + 1]] <- arboles_actual %>% 
      mutate(año_simulacion = año)
  }
  
  cat("\nâœ“ SimulaciÃ³n completada\n\n")
  
  # ===========================================================================
  # 2. CALCULAR MÃ‰TRICAS INICIALES Y FINALES
  # ===========================================================================
  
  cat("[PASO 2/4] Calculando métricas por rodal, género y especie...\n\n")
  
  # MÃ©tricas iniciales (año 0)
  metricas_inicial <- calcular_metricas_detalladas(arboles_inicial, config)
  
  # MÃ©tricas finales (año 10)
  metricas_final <- calcular_metricas_detalladas(arboles_actual, config)
  
  
  # Calcular tiempo de paso
  tiempo_paso <- calcular_tiempo_paso(metricas_inicial, metricas_final, años)
  # ===========================================================================
  # 3. CALCULAR VARIABLES DEL 11.1.4
  # ===========================================================================
  
  cat("[PASO 3/4] Calculando variables según Manual PMF 11.1.4...\n\n")
  
  # Calcular por rodal
  ica_por_rodal <- calcular_variables_114_rodal(
    metricas_inicial, 
    metricas_final, 
    años, 
    config,
    arboles_inicial  # Para obtener superficies reales
  )
  
  # Calcular por género y rodal
  ica_por_genero_rodal <- calcular_variables_114_genero_rodal(
    metricas_inicial,
    metricas_final,
    años,
    config,
    arboles_inicial  # Para obtener superficies reales
  )
  
  # Calcular por especie y rodal (solo Pinus y Quercus)
  ica_por_especie_rodal <- calcular_variables_114_especie_rodal(
    arboles_inicial,
    arboles_actual,
    años,
    config,
    metricas_inicial,  # Agregar métricas
    metricas_final     # Agregar métricas
  )
  
  # Resumen general del predio
  resumen_predio <- calcular_resumen_predio(
    ica_por_rodal,
    años,
    config
  )
  
  cat("âœ“ Cálculos completados\n\n")
  
  # ===========================================================================
  # 4. RETORNAR RESULTADOS
  # ===========================================================================
  
  return(list(
    # Datos de simulación
    poblacion_inicial = arboles_inicial,
    poblacion_final = arboles_actual,
    historial = bind_rows(historial_completo),
    años_simulados = años,
    
    # Tablas ICA calculadas
    ica_por_rodal = ica_por_rodal,
    ica_por_genero_rodal = ica_por_genero_rodal,
    ica_por_especie_rodal = ica_por_especie_rodal,
    resumen_predio = resumen_predio,
    
    # Tiempo de paso
    tiempo_paso_por_genero_rodal = tiempo_paso$por_genero_rodal,
    tiempo_paso_por_rodal = tiempo_paso$por_rodal
  ))
}

# ==============================================================================
# MÃ‰TRICAS DETALLADAS POR RODAL, GÃ‰NERO Y ESPECIE
# ==============================================================================

calcular_metricas_detalladas <- function(arboles_df, config) {
  
  # Usar funciones compartidas de utils_metricas.R (evita duplicaciÃ³n)
  por_rodal <- calcular_metricas_estado(arboles_df, config) %>%
    select(rodal, n_arboles = n_vivos, vol_muestreado_m3, vol_ha_m3,
           area_basal_m2 = ab_muestreada_m2, d_medio_cm, h_media_m)
  
  por_genero <- calcular_metricas_por_genero(arboles_df, config) %>%
    select(rodal, genero_grupo, n_arboles = n_vivos, vol_muestreado_m3,
           vol_ha_m3, area_basal_m2 = ab_muestreada_m2)
  
  # Por especie (filtrar solo Pinus y Quercus)
  por_especie <- calcular_metricas_por_especie(arboles_df, config) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    select(rodal, genero_grupo, nombre_cientifico, n_arboles = n_vivos,
           vol_muestreado_m3, vol_ha_m3, area_basal_m2 = ab_muestreada_m2)
  
  return(list(
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# CALCULAR TIEMPO DE PASO (AÑOS PARA INCREMENTAR 5 CM)
# ==============================================================================

calcular_tiempo_paso <- function(metricas_inicial, metricas_final, años, 
                                 delta_d_objetivo = 5) {
  
  cat("\n[CÁLCULO] Tiempo de paso por UMM y género...\n")
  
  # ============================================================================
  # Función auxiliar: convertir área basal/ha y n_arboles/ha a diámetro medio
  # ============================================================================
  calcular_diametro_cuadratico <- function(area_basal_m2, n_arboles) {
    # área basal ya es m²/ha, n_arboles es árboles/ha (implícito en las métricas)
    # d_g = sqrt(AB/n × 40000/π)
    # donde AB/n es el área basal promedio por árbol en m²
    
    ab_promedio_m2 <- ifelse(n_arboles > 0 & !is.na(n_arboles),
                             area_basal_m2 / n_arboles,
                             NA_real_)
    d_cm <- sqrt(ab_promedio_m2 * 40000 / pi)
    
    return(d_cm)
  }
  
  # ============================================================================
  # TIEMPO DE PASO POR GÉNERO Y RODAL
  # ============================================================================
  
  # Extraer datos iniciales
  inicial_genero <- metricas_inicial$por_genero %>%
    rename(genero = genero_grupo) %>%
    mutate(
      d_medio_ini_cm = calcular_diametro_cuadratico(area_basal_m2, n_arboles)
    ) %>%
    select(rodal, genero, n_arboles_ini = n_arboles, 
           area_basal_ini_m2 = area_basal_m2, d_medio_ini_cm)
  
  # Extraer datos finales
  final_genero <- metricas_final$por_genero %>%
    rename(genero = genero_grupo) %>%
    mutate(
      d_medio_fin_cm = calcular_diametro_cuadratico(area_basal_m2, n_arboles)
    ) %>%
    select(rodal, genero, n_arboles_fin = n_arboles,
           area_basal_fin_m2 = area_basal_m2, d_medio_fin_cm)
  
  # Combinar y calcular tiempo de paso
  tiempo_paso_genero <- inicial_genero %>%
    left_join(final_genero, by = c("rodal", "genero")) %>%
    mutate(
      # Incremento diamétrico observado en el período
      delta_d_cm = d_medio_fin_cm - d_medio_ini_cm,
      
      # Incremento anual
      incremento_anual_cm = delta_d_cm / años,
      
      # Tiempo de paso (años para incrementar 5 cm)
      tiempo_paso_años = ifelse(incremento_anual_cm > 0,
                                delta_d_objetivo / incremento_anual_cm,
                                NA_real_)
    ) %>%
    select(rodal, genero, 
           n_arboles_ini, n_arboles_fin,
           d_medio_ini_cm, d_medio_fin_cm, 
           delta_d_cm, incremento_anual_cm, tiempo_paso_años)
  
  # ============================================================================
  # TIEMPO DE PASO POR RODAL (PROMEDIO PONDERADO DE GÉNEROS)
  # ============================================================================
  
  tiempo_paso_rodal <- tiempo_paso_genero %>%
    filter(!is.na(tiempo_paso_años), tiempo_paso_años > 0) %>%
    group_by(rodal) %>%
    summarise(
      # Promedio ponderado por número de árboles inicial
      tiempo_paso_promedio_años = weighted.mean(
        tiempo_paso_años,
        w = n_arboles_ini,
        na.rm = TRUE
      ),
      # Incremento promedio ponderado
      incremento_anual_promedio_cm = weighted.mean(
        incremento_anual_cm,
        w = n_arboles_ini,
        na.rm = TRUE
      ),
      n_arboles_total_ini = sum(n_arboles_ini),
      n_arboles_total_fin = sum(n_arboles_fin),
      .groups = "drop"
    )
  
  # Mostrar resumen
  cat(sprintf("\n  ✓ Tiempo de paso calculado para %d UMM\n", 
              nrow(tiempo_paso_rodal)))
  cat(sprintf("  • Rango: %.1f - %.1f años\n",
              min(tiempo_paso_rodal$tiempo_paso_promedio_años, na.rm = TRUE),
              max(tiempo_paso_rodal$tiempo_paso_promedio_años, na.rm = TRUE)))
  cat(sprintf("  • Media: %.1f años\n\n",
              mean(tiempo_paso_rodal$tiempo_paso_promedio_años, na.rm = TRUE)))
  
  return(list(
    por_genero_rodal = tiempo_paso_genero,
    por_rodal = tiempo_paso_rodal
  ))
}

# ==============================================================================
# VARIABLES 11.1.4 POR RODAL (UMM) - CON SUPERFICIE_CORTA
# ==============================================================================

calcular_variables_114_rodal <- function(inicial, final, años, config, arboles_inicial) {
  
  # ============================================================================
  # âœ… EXTRAER AMBAS SUPERFICIES: total y aprovechable
  # ============================================================================
  
  superficie_por_rodal <- arboles_inicial %>%
    group_by(rodal) %>%
    summarise(
      superficie_total_ha = first(superficie_total_ha),
      superficie_corta_ha = first(superficie_corta_ha),  # âœ… Aprovechable
      .groups = "drop"
    )
  
  # Verificar si hay NAs
  if (any(is.na(superficie_por_rodal$superficie_total_ha))) {
    warning("âš ï¸ Algunos rodales tienen superficie_total_ha NA")
    sup_total_promedio <- mean(superficie_por_rodal$superficie_total_ha, na.rm = TRUE)
    superficie_por_rodal <- superficie_por_rodal %>%
      mutate(superficie_total_ha = ifelse(is.na(superficie_total_ha), 
                                          sup_total_promedio, 
                                          superficie_total_ha))
  }
  
  if (any(is.na(superficie_por_rodal$superficie_corta_ha))) {
    warning("âš ï¸ Algunos rodales tienen superficie_corta_ha NA. Usando superficie_total_ha.")
    superficie_por_rodal <- superficie_por_rodal %>%
      mutate(superficie_corta_ha = ifelse(is.na(superficie_corta_ha), 
                                          superficie_total_ha, 
                                          superficie_corta_ha))
  }
  
  # ============================================================================
  # CALCULAR ICA y VOLÃšMENES DE CORTA
  # ============================================================================
  
  comparacion <- inicial$por_rodal %>%
    select(rodal,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final$por_rodal %>%
        select(rodal,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = "rodal"
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # Existencias reales (mÂ³/ha)
      ER_m3_ha = vol_ha_ini,
      
      # ICA (mÂ³/ha/año)
      ICA_m3_ha = (vol_ha_fin - ER_m3_ha) / años,
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      
      # Ciclo de corta
      ciclo_corta = config$periodo,
      
      # Intensidad de corta relativa
      IntCor_rel_IC = ifelse(ICA_rel_i > 0, 
                             1 - 1/(1 + ICA_rel_i)^ciclo_corta, 
                             0),
      
      # âœ… VOLUMEN DE CORTA: usar superficie_corta_ha (NO total)
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,  # mÂ³/ha
      VC_rodal_m3 = VC_ha_m3 * superficie_corta_ha,  # âœ… Escalar por sup. aprovechable
      
      # ER usa superficie total; VC usa superficie aprovechable (sin ribereñas)
      ER_rodal_m3 = ER_m3_ha * superficie_total_ha,
      ER_rodal_total_m3 = ER_rodal_m3
    ) %>%
    select(rodal,
           Sup_ha = superficie_corta_ha,
           superficie_total_ha,
           ER_m3_ha, ICA_m3_ha, ICA_rel_i,
           ciclo_corta, IntCor_rel_IC, VC_ha_m3,
           ER_rodal_m3,
           ER_rodal_total_m3,
           VC_rodal_m3)
  
  return(comparacion)
}

# ==============================================================================
# VARIABLES 11.1.4 POR GÃ‰NERO Y RODAL
# ==============================================================================

calcular_variables_114_genero_rodal <- function(inicial, final, años, config, arboles_inicial) {
  
  # âœ… Extraer superficie APROVECHABLE (corta_ha), no total
  superficie_por_rodal <- arboles_inicial %>%
    group_by(rodal) %>%
    summarise(
      superficie_corta_ha = first(superficie_corta_ha),
      superficie_total_ha = first(superficie_total_ha),  # Para referencia
      .groups = "drop"
    )
  
  # Verificar NAs y usar fallback
  if (any(is.na(superficie_por_rodal$superficie_corta_ha))) {
    warning("âš ï¸ Algunos rodales tienen superficie_corta_ha NA. Usando superficie_total_ha.")
    superficie_por_rodal <- superficie_por_rodal %>%
      mutate(superficie_corta_ha = ifelse(is.na(superficie_corta_ha), 
                                          superficie_total_ha, 
                                          superficie_corta_ha))
  }
  
  comparacion <- inicial$por_genero %>%
    select(rodal, genero = genero_grupo,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final$por_genero %>%
        select(rodal, genero = genero_grupo,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = c("rodal", "genero")
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # ER y volumen final ya estÃ¡n calculados correctamente como mÂ³/ha
      ER_m3_ha = vol_ha_ini,
      vol_fin_m3_ha = vol_ha_fin,
      ICA_m3_ha = (vol_fin_m3_ha - ER_m3_ha) / años,
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      ciclo_corta = config$periodo,
      IntCor_rel_IC = ifelse(ICA_rel_i > -1, 
                             1 - 1/((1 + ICA_rel_i)^ciclo_corta),
                             0),
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,
      # ER usa superficie total; VC usa superficie aprovechable (sin ribereñas)
      ER_rodal_m3 = ER_m3_ha * superficie_total_ha,
      VC_rodal_m3 = VC_ha_m3 * superficie_corta_ha
    ) %>%
    select(rodal, genero,
           Sup_ha = superficie_corta_ha,
           superficie_total_ha,
           ER_m3_ha, ICA_m3_ha, ICA_rel_i,
           ciclo_corta, IntCor_rel_IC, VC_ha_m3,
           ER_rodal_m3, VC_rodal_m3)
  
  return(comparacion)
}

# ==============================================================================
# VARIABLES 11.1.4 POR ESPECIE Y RODAL (PINUS Y QUERCUS)
# ==============================================================================

calcular_variables_114_especie_rodal <- function(arboles_inicial, arboles_final, 
                                                 años, config, 
                                                 metricas_inicial, metricas_final) {
  
  # Usar las métricas ya calculadas que incluyen vol_ha_m3
  inicial_especies <- metricas_inicial$por_especie %>%
    rename(genero = genero_grupo, especie = nombre_cientifico)
  
  final_especies <- metricas_final$por_especie %>%
    rename(genero = genero_grupo, especie = nombre_cientifico)
  
  # âœ… Extraer superficie APROVECHABLE, no total
  tiene_superficie <- "superficie_corta_ha" %in% names(arboles_inicial)
  
  if (tiene_superficie) {
    superficie_por_rodal <- arboles_inicial %>%
      group_by(rodal) %>%
      summarise(
        superficie_corta_ha = first(na.omit(superficie_corta_ha)),
        superficie_total_ha = first(na.omit(superficie_total_ha)),
        .groups = "drop"
      )
    
    if (all(is.na(superficie_por_rodal$superficie_corta_ha)) || 
        nrow(superficie_por_rodal) == 0 ||
        any(superficie_por_rodal$superficie_corta_ha <= 0, na.rm = TRUE)) {
      # Fallback a superficie total si corta_ha no estÃ¡ disponible
      warning("âš ï¸ superficie_corta_ha no disponible. Usando superficie_total_ha.")
      superficie_por_rodal <- superficie_por_rodal %>%
        mutate(superficie_corta_ha = superficie_total_ha)
      tiene_superficie <- !all(is.na(superficie_por_rodal$superficie_corta_ha))
    }
  }
  
  # Fallback si no hay datos de superficie
  if (!tiene_superficie) {
    warning("âš ï¸ No se encontraron superficies por rodal. Usando distribución equitativa.")
    superficie_total_ha <- 100
    n_rodales <- n_distinct(inicial_especies$rodal)
    superficie_por_rodal <- tibble(
      rodal = unique(inicial_especies$rodal),
      superficie_corta_ha = superficie_total_ha / n_rodales,
      superficie_total_ha = superficie_total_ha / n_rodales
    )
  }
  
  # Asegurar que todos los rodales tengan superficie
  rodales_sin_superficie <- inicial_especies %>%
    distinct(rodal) %>%
    anti_join(superficie_por_rodal, by = "rodal")
  
  if (nrow(rodales_sin_superficie) > 0) {
    sup_promedio <- mean(superficie_por_rodal$superficie_corta_ha, na.rm = TRUE)
    superficie_faltante <- tibble(
      rodal = rodales_sin_superficie$rodal,
      superficie_corta_ha = sup_promedio,
      superficie_total_ha = sup_promedio
    )
    superficie_por_rodal <- bind_rows(superficie_por_rodal, superficie_faltante)
  }
  
  comparacion <- inicial_especies %>%
    rename(vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final_especies %>%
        rename(vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = c("rodal", "genero", "especie")
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # ER y volumen final ya estÃ¡n calculados correctamente como mÂ³/ha
      ER_m3_ha = vol_ha_ini,
      vol_fin_m3_ha = vol_ha_fin,
      ICA_m3_ha = (vol_fin_m3_ha - ER_m3_ha) / años,
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      ciclo_corta = config$periodo,
      IntCor_rel_IC = ifelse(ICA_rel_i > -1,
                             1 - 1/((1 + ICA_rel_i)^ciclo_corta),
                             0),
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,
      # ER usa superficie total; VC usa superficie aprovechable (sin ribereñas)
      ER_rodal_m3 = ER_m3_ha * superficie_total_ha,
      VC_rodal_m3 = VC_ha_m3 * superficie_corta_ha
    ) %>%
    select(rodal, genero, especie,
           Sup_ha = superficie_corta_ha,
           superficie_total_ha,
           ER_m3_ha, ICA_m3_ha, ICA_rel_i,
           ciclo_corta, IntCor_rel_IC, VC_ha_m3,
           ER_rodal_m3, VC_rodal_m3)
  
  return(comparacion)
}
# ==============================================================================
# RESUMEN GENERAL DEL PREDIO
# ==============================================================================

calcular_resumen_predio <- function(ica_por_rodal, años, config) {
  
  resumen <- ica_por_rodal %>%
    summarise(
      superficie_total_ha = sum(Sup_ha),
      ER_total_m3 = sum(ER_rodal_m3),
      ciclo_corta = first(ciclo_corta),
      VC_total_ciclo_m3 = sum(VC_rodal_m3),
      VC_anual_m3 = VC_total_ciclo_m3 / ciclo_corta,
      area_corta_anual_ha = superficie_total_ha / ciclo_corta,
      .groups = "drop"
    )
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘            RESUMEN GENERAL DEL PREDIO                     â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  cat(sprintf("Superficie de la UM:                  %.3f hectáreas\n", 
              resumen$superficie_total_ha))
  cat(sprintf("Existencias reales en la UM:          %.3f mÂ³\n", 
              resumen$ER_total_m3))
  cat(sprintf("Ciclo de corta:                       %d años\n", 
              resumen$ciclo_corta))
  cat(sprintf("Vol. de corta durante el ciclo:       %.3f mÂ³\n", 
              resumen$VC_total_ciclo_m3))
  cat(sprintf("Posibilidad anual:                    %.3f mÂ³\n", 
              resumen$VC_anual_m3))
  cat(sprintf("Ãrea de corta anual:                  %.3f ha\n\n", 
              resumen$area_corta_anual_ha))
  
  return(resumen)
}

# ==============================================================================
# EXPORTAR TABLAS LaTeX
# ==============================================================================

exportar_tablas_latex_ica <- function(resultados_ica, directorio = "tablas_latex") {
  
  cat("\n[PASO 4/4] Exportando tablas LaTeX...\n\n")
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # --------------------------------------------------
  # TABLA 1: ICA POR RODAL
  # --------------------------------------------------
  
  tabla1 <- resultados_ica$ica_por_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab1 <- xtable(tabla1,
                  caption = "Variables de manejo forestal por Rodal (UMM) según secciÃ³n 11.1.4 del Manual PMF",
                  label = "tab:ica_rodal",
                  digits = 3)
  
  print(xtab1,
        file = file.path(directorio, "31_ica_por_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  âœ“ %s\n", "31_ica_por_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 2: ICA POR GÃ‰NERO Y RODAL
  # --------------------------------------------------
  
  tabla2 <- resultados_ica$ica_por_genero_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab2 <- xtable(tabla2,
                  caption = "Variables de manejo forestal por Género y Rodal",
                  label = "tab:ica_genero_rodal",
                  digits = 3)
  
  print(xtab2,
        file = file.path(directorio, "31_ica_por_genero_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  âœ“ %s\n", "31_ica_por_genero_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 3: ICA POR ESPECIE Y RODAL (PINUS/QUERCUS)
  # --------------------------------------------------
  
  tabla3 <- resultados_ica$ica_por_especie_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab3 <- xtable(tabla3,
                  caption = "Variables de manejo forestal por Especie (Pinus y Quercus) y Rodal",
                  label = "tab:ica_especie_rodal",
                  digits = 3)
  
  print(xtab3,
        file = file.path(directorio, "31_ica_por_especie_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  âœ“ %s\n", "31_ica_por_especie_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 4: RESUMEN PREDIO
  # --------------------------------------------------
  
  tabla4 <- resultados_ica$resumen_predio %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor")
  
  xtab4 <- xtable(tabla4,
                  caption = "Resumen general del predio - Posibilidad de corta",
                  label = "tab:resumen_predio",
                  digits = 3)
  
  print(xtab4,
        file = file.path(directorio, "31_resumen_predio.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  âœ“ %s\n", "31_resumen_predio.tex"))
  
  # --------------------------------------------------
  # TABLA 5: TIEMPO DE PASO POR RODAL
  # --------------------------------------------------
  
  if (!is.null(resultados_ica$tiempo_paso_por_rodal)) {
    tabla5 <- resultados_ica$tiempo_paso_por_rodal %>%
      mutate(across(where(is.numeric), ~round(., 3)))
    
    xtab5 <- xtable(tabla5,
                    caption = "Tiempo de paso (años para incrementar 5 cm) por Rodal (UMM)",
                    label = "tab:tiempo_paso_rodal",
                    digits = 3)
    
    print(xtab5,
          file = file.path(directorio, "31_tiempo_paso_rodal.tex"),
          include.rownames = FALSE,
          caption.placement = "top",
          booktabs = TRUE,
          sanitize.text.function = identity)
    
    cat(sprintf("  ✓ %s\n", "31_tiempo_paso_rodal.tex"))
  }
  
  # --------------------------------------------------
  # TABLA 6: TIEMPO DE PASO POR GÉNERO Y RODAL
  # --------------------------------------------------
  
  if (!is.null(resultados_ica$tiempo_paso_por_genero_rodal)) {
    tabla6 <- resultados_ica$tiempo_paso_por_genero_rodal %>%
      mutate(across(where(is.numeric), ~round(., 3)))
    
    xtab6 <- xtable(tabla6,
                    caption = "Tiempo de paso (años para incrementar 5 cm) por Género y Rodal",
                    label = "tab:tiempo_paso_genero_rodal",
                    digits = 3)
    
    print(xtab6,
          file = file.path(directorio, "31_tiempo_paso_genero_rodal.tex"),
          include.rownames = FALSE,
          caption.placement = "top",
          booktabs = TRUE,
          sanitize.text.function = identity)
    
    cat(sprintf("  ✓ %s\n", "31_tiempo_paso_genero_rodal.tex"))
  }
  cat("\nâœ“ Tablas LaTeX exportadas exitosamente\n\n")
}

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

guardar_resultados_ica <- function(resultados_ica, directorio = "resultados") {
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Guardar RDS completo
  saveRDS(resultados_ica, file.path(directorio, "31_resultados_ica.rds"))
  
  # Guardar CSVs individuales para uso en optimizador
  write.csv(resultados_ica$ica_por_rodal,
            file.path(directorio, "31_ica_por_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$ica_por_genero_rodal,
            file.path(directorio, "31_ica_por_genero_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$ica_por_especie_rodal,
            file.path(directorio, "31_ica_por_especie_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$resumen_predio,
            file.path(directorio, "31_resumen_predio.csv"),
            row.names = FALSE)
  
  # Guardar tiempo de paso si está disponible
  if (!is.null(resultados_ica$tiempo_paso_por_rodal)) {
    write.csv(resultados_ica$tiempo_paso_por_rodal,
              file.path(directorio, "31_tiempo_paso_rodal.csv"),
              row.names = FALSE)
  }
  
  if (!is.null(resultados_ica$tiempo_paso_por_genero_rodal)) {
    write.csv(resultados_ica$tiempo_paso_por_genero_rodal,
              file.path(directorio, "31_tiempo_paso_genero_rodal.csv"),
              row.names = FALSE)
  }
  
  cat("\n✓ Resultados guardados en directorio resultados/\n")
  cat("  • 31_resultados_ica.rds (objeto completo)\n")
  cat("  • 31_ica_por_rodal.csv\n")
  cat("  • 31_ica_por_genero_rodal.csv\n")
  cat("  • 31_ica_por_especie_rodal.csv\n")
  cat("  • 31_resumen_predio.csv\n")
  if (!is.null(resultados_ica$tiempo_paso_por_rodal)) {
    cat("  • 31_tiempo_paso_rodal.csv\n")
    cat("  • 31_tiempo_paso_genero_rodal.csv\n")
  }
  cat("\n")
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================


cat("\n✓ Módulo de cálculo de ICA cargado (sin cortes)\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Funciones disponibles:\n")
cat("  • calcular_ica_sin_cortes(arboles, config, años=10)\n")
cat("  • calcular_tiempo_paso(metricas_inicial, metricas_final, años)\n")
cat("  • exportar_tablas_latex_ica(resultados)\n")
cat("  • guardar_resultados_ica(resultados)\n\n")
cat("Este módulo calcula:\n")
cat("  - ICA derivado del modelo poblacional\n")
cat("  - Tiempo de paso (años para incrementar 5 cm de diámetro)\n")
cat("  - Variables requeridas por NOM-152 (sección 11.1.4)\n")
cat("══════════════════════════════════════════════════════════════\n\n")