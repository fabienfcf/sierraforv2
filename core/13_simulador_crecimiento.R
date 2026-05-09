# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 13_simulador_crecimiento.R
# ==============================================================================


library(tidyverse)

# Cargar funciones puras
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "utils/utils_validacion.R"))
source(file.path(PROYECTO_ROOT, "utils/utils_metricas.R"))

# ==============================================================================
# 1-3. CALCULAR MÉTRICAS (usa funciones compartidas)
# ==============================================================================

# Las funciones de cálculo de métricas ahora están en utils_metricas.R
# para evitar duplicación de código:
#   - calcular_metricas_estado()
#   - calcular_metricas_por_genero()
#   - calcular_metricas_por_especie()

# ==============================================================================
# 4. ACTUALIZAR VOLÚMENES
# ==============================================================================

actualizar_volumenes <- function(arboles_df) {
  
  cat("\n[VOLUMEN] Recalculando con ecuaciones alométricas...\n")
  
  arboles_actualizado <- arboles_df %>%
    mutate(area_basal = calcular_area_basal(diametro_normal))
  
  arboles_actualizado <- calcular_volumenes_vectorizado(arboles_actualizado)
  
  arboles_actualizado <- arboles_actualizado %>%
    mutate(volumen_m3 = if_else(!es_arbol_vivo(dominancia), 0, volumen_m3))
  
  vol_total <- sum(arboles_actualizado$volumen_m3, na.rm = TRUE)
  vivos <- filtrar_arboles_vivos(arboles_actualizado)
  vol_vivos <- sum(vivos$volumen_m3, na.rm = TRUE)
  
  cat(sprintf("  Volumen total: %.2f m³\n", vol_total))
  cat(sprintf("  Volumen vivos: %.2f m³\n", vol_vivos))
  
  return(arboles_actualizado)
}

# ==============================================================================
# 5. SIMULADOR PRINCIPAL
# ==============================================================================

simular_crecimiento_rodal <- function(arboles_inicial, config, años = NULL) {
  
  if (is.null(años)) {
    años <- config$periodo
  }
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat(sprintf("║     SIMULACIÓN DE CRECIMIENTO - %d AÑOS                   ║\n", años))
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  arboles_actual <- arboles_inicial
  historial <- list()
  historial_metricas <- list()
  
  historial[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
  historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual) %>% 
    mutate(año_simulacion = 0)
  
  for (año in 1:años) {
    
    cat(sprintf("\n════════════════ AÑO %d ════════════════\n", año))
    
    arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, config, año)
    arboles_actual <- actualizar_volumenes(arboles_actual)
    arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, config, año)
    arboles_actual <- aplicar_reclutamiento(arboles_actual, config, año)
    
    historial[[año + 1]] <- arboles_actual %>% mutate(año_simulacion = año)
    historial_metricas[[año + 1]] <- calcular_metricas_estado(arboles_actual) %>%
      mutate(año_simulacion = año)
  }
  
  historial_completo <- bind_rows(historial)
  historial_metricas_completo <- bind_rows(historial_metricas)
  
  cat("\n✓ Simulación completada\n")
  
  return(list(
    poblacion_inicial = arboles_inicial,
    poblacion_final = arboles_actual,
    historial = historial_completo,
    historial_metricas = historial_metricas_completo,
    años_simulados = años
  ))
}

# ==============================================================================
# 6. COMPARAR ESTADOS (NOMENCLATURA ACTUALIZADA)
# ==============================================================================

comparar_estados <- function(resultado_simulacion) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         COMPARACIÓN ESTADO INICIAL vs FINAL               ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  inicial <- resultado_simulacion$poblacion_inicial
  final <- resultado_simulacion$poblacion_final
  años <- resultado_simulacion$años_simulados
  
  metricas_inicial <- calcular_metricas_estado(inicial)
  metricas_final <- calcular_metricas_estado(final)
  
  comparacion <- metricas_inicial %>%
    select(rodal, 
           n_vivos_ini = n_vivos,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           d_medio_ini = d_medio_cm,
           h_media_ini = h_media_m) %>%
    left_join(
      metricas_final %>%
        select(rodal,
               n_vivos_fin = n_vivos,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               d_medio_fin = d_medio_cm,
               h_media_fin = h_media_m),
      by = "rodal"
    ) %>%
    mutate(
      delta_n = n_vivos_fin - n_vivos_ini,
      delta_vol_muestreado = vol_muestreado_fin - vol_muestreado_ini,
      delta_vol_ha = vol_ha_fin - vol_ha_ini,
      delta_d = d_medio_fin - d_medio_ini,
      delta_h = h_media_fin - h_media_ini,
      
      cambio_n_pct = (delta_n / n_vivos_ini) * 100,
      cambio_vol_pct = (delta_vol_muestreado / vol_muestreado_ini) * 100,
      
      ima_vol_muestreado = delta_vol_muestreado / años,
      ima_vol_ha = delta_vol_ha / años,
      ima_d = delta_d / años,
      ima_h = delta_h / años
    )
  
  # Reporte
  cat("[POBLACIÓN]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %d → %d árboles (%+d, %+.1f%%)\n",
                r$rodal, r$n_vivos_ini, r$n_vivos_fin, 
                r$delta_n, r$cambio_n_pct))
  }
  
  cat("\n[VOLUMEN MEDIDO EN PARCELAS]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %.2f → %.2f m³ (%+.2f, %+.1f%%) | IMA: %.2f m³/año\n",
                r$rodal, r$vol_muestreado_ini, r$vol_muestreado_fin,
                r$delta_vol_muestreado, r$cambio_vol_pct, r$ima_vol_muestreado))
  }
  
  cat("\n[VOLUMEN POR HECTÁREA]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %.2f → %.2f m³/ha | IMA: %.2f m³/ha/año\n",
                r$rodal, r$vol_ha_ini, r$vol_ha_fin, r$ima_vol_ha))
  }
  
  cat("\n[DIMENSIONES]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: Ø %.1f → %.1f cm (%+.1f) | h %.1f → %.1f m (%+.1f)\n",
                r$rodal, r$d_medio_ini, r$d_medio_fin, r$delta_d,
                r$h_media_ini, r$h_media_fin, r$delta_h))
  }
  
  cat("\n")
  
  return(comparacion)
}

# ==============================================================================
# 7. COMPARAR POR GÉNERO
# ==============================================================================

comparar_estados_por_genero <- function(resultado_simulacion) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         COMPARACIÓN POR GÉNERO - INICIAL vs FINAL         ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  inicial <- resultado_simulacion$poblacion_inicial
  final <- resultado_simulacion$poblacion_final
  años <- resultado_simulacion$años_simulados
  
  metricas_inicial <- calcular_metricas_por_genero(inicial)
  metricas_final <- calcular_metricas_por_genero(final)
  
  comparacion <- metricas_inicial %>%
    select(rodal, genero_grupo,
           n_vivos_ini = n_vivos,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           d_medio_ini = d_medio_cm) %>%
    left_join(
      metricas_final %>%
        select(rodal, genero_grupo,
               n_vivos_fin = n_vivos,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               d_medio_fin = d_medio_cm),
      by = c("rodal", "genero_grupo")
    ) %>%
    mutate(
      delta_n = n_vivos_fin - n_vivos_ini,
      delta_vol_muestreado = vol_muestreado_fin - vol_muestreado_ini,
      delta_vol_ha = vol_ha_fin - vol_ha_ini,
      delta_d = d_medio_fin - d_medio_ini,
      
      cambio_n_pct = (delta_n / n_vivos_ini) * 100,
      cambio_vol_pct = (delta_vol_muestreado / vol_muestreado_ini) * 100,
      
      ima_vol_muestreado = delta_vol_muestreado / años,
      ima_vol_ha = delta_vol_ha / años
    )
  
  # Reporte por rodal y género
  for (rodal_id in unique(comparacion$rodal)) {
    cat(sprintf("\n═══ RODAL %d ═══\n\n", rodal_id))
    
    datos_rodal <- comparacion %>% filter(rodal == rodal_id)
    
    for (gen in unique(datos_rodal$genero_grupo)) {
      r <- datos_rodal %>% filter(genero_grupo == gen)
      
      cat(sprintf("[%s]\n", gen))
      cat(sprintf("  Población:      %d → %d árboles (%+d, %+.1f%%)\n",
                  r$n_vivos_ini, r$n_vivos_fin, r$delta_n, r$cambio_n_pct))
      cat(sprintf("  Vol. muestreado: %.2f → %.2f m³ (%+.2f, %+.1f%%) | IMA: %.2f m³/año\n",
                  r$vol_muestreado_ini, r$vol_muestreado_fin, 
                  r$delta_vol_muestreado, r$cambio_vol_pct, r$ima_vol_muestreado))
      cat(sprintf("  Vol/ha:         %.2f → %.2f m³/ha | IMA: %.2f m³/ha/año\n",
                  r$vol_ha_ini, r$vol_ha_fin, r$ima_vol_ha))
      cat(sprintf("  Ø medio:        %.1f → %.1f cm (%+.1f)\n\n",
                  r$d_medio_ini, r$d_medio_fin, r$delta_d))
    }
  }
  
  # Resumen general
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMEN GENERAL POR GÉNERO                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  resumen_genero <- comparacion %>%
    group_by(genero_grupo) %>%
    summarise(
      n_rodales = n(),
      vol_muestreado_ini = sum(vol_muestreado_ini),
      vol_muestreado_fin = sum(vol_muestreado_fin),
      vol_ha_medio_ini = mean(vol_ha_ini),
      vol_ha_medio_fin = mean(vol_ha_fin),
      .groups = "drop"
    ) %>%
    mutate(
      cambio_vol = vol_muestreado_fin - vol_muestreado_ini,
      cambio_pct = (cambio_vol / vol_muestreado_ini) * 100
    )
  
  print(resumen_genero)
  
  cat("\n")
  
  return(comparacion)
}

# ==============================================================================
# 8. VISUALIZACIÓN
# ==============================================================================

graficar_evolucion_temporal <- function(resultado_simulacion) {
  
  if (!"historial_metricas" %in% names(resultado_simulacion)) {
    warning("No hay historial de métricas disponible")
    return(NULL)
  }
  
  resumen_anual <- resultado_simulacion$historial_metricas %>%
    group_by(año_simulacion) %>%
    summarise(
      n_vivos_total = sum(n_vivos),
      vol_muestreado_m3 = sum(vol_muestreado_m3),
      vol_medio_ha = mean(vol_ha_m3),
      d_medio_cm = mean(d_medio_cm, na.rm = TRUE),
      h_media_m = mean(h_media_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  library(patchwork)
  
  p1 <- ggplot(resumen_anual, aes(x = año_simulacion, y = vol_medio_ha)) +
    geom_area(fill = "#06A77D", alpha = 0.3) +
    geom_line(color = "#06A77D", linewidth = 1.2) +
    geom_point(color = "#06A77D", size = 2.5) +
    labs(
      title = "Volumen Promedio por Hectárea",
      x = "Año de Simulación",
      y = "Volumen (m³/ha)"
    ) +
    theme_minimal(base_size = 11)
  
  p2 <- ggplot(resumen_anual, aes(x = año_simulacion, y = n_vivos_total)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 2.5) +
    labs(
      title = "Población Total",
      x = "Año de Simulación",
      y = "Número de Árboles Vivos"
    ) +
    theme_minimal(base_size = 11)
  
  p3 <- ggplot(resumen_anual, aes(x = año_simulacion, y = d_medio_cm)) +
    geom_line(color = "#F18F01", linewidth = 1.2) +
    geom_point(color = "#F18F01", size = 2.5) +
    labs(
      title = "Diámetro Medio",
      x = "Año de Simulación",
      y = "Diámetro (cm)"
    ) +
    theme_minimal(base_size = 11)
  
  p_combined <- (p1 / p2 / p3) +
    plot_annotation(
      title = "Simulación de Crecimiento Forestal",
      subtitle = sprintf("%d años de proyección", max(resumen_anual$año_simulacion))
    )
  
  return(p_combined)
}

# ==============================================================================
# 9. VALIDACIÓN (usa función compartida)
# ==============================================================================

# La función validar_crecimiento() ahora está en utils_validacion.R
# para evitar duplicación de código

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo simulador de crecimiento cargado\n")
cat("  Funciones disponibles:\n")
cat("    - simular_crecimiento_rodal(arboles, config, años)\n")
cat("    - calcular_metricas_estado(arboles_df) [en utils_metricas.R]\n")
cat("    - calcular_metricas_por_genero(arboles_df) [en utils_metricas.R]\n")
cat("    - calcular_metricas_por_especie(arboles_df) [en utils_metricas.R]\n")
cat("    - comparar_estados(resultado)\n")
cat("    - comparar_estados_por_genero(resultado)\n")
cat("    - graficar_evolucion_temporal(resultado)\n\n")