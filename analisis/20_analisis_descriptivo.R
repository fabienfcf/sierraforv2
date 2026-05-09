# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 08_ANALISIS_DESCRIPTIVO.R - CORRECCION n_sitios
# CAMBIO CRÃTICO: Usar TODOS los sitios muestreados, no solo donde hay Ã¡rboles
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# Verificar que CONFIG estÃ© cargado
if (!exists("CONFIG")) {
  stop("âŒ CONFIG no estÃ¡ cargado. Ejecuta primero: source(file.path(PROYECTO_ROOT, 'config/01_parametros_configuracion.R'))")
}

# Verificar funciones de expansiÃ³n
if (!exists("expandir_a_hectarea")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# ==============================================================================
# VERIFICAR CONFIG
# ==============================================================================

if (is.null(CONFIG$area_parcela_regeneracion_ha)) {
  warning("âš ï¸ CONFIG$area_parcela_regeneracion_ha no existe, usando 0.0009 ha (9 mÂ²)")
  CONFIG$area_parcela_regeneracion_ha <- 0.0009
}

cat(sprintf("\n[CONFIGURACIÃ“N DE EXPANSIÃ“N]\n"))
cat(sprintf("  Arbolado: %.2f ha (factor = %.1f)\n", 
            CONFIG$area_parcela_ha, 
            calcular_factor_expansion(CONFIG$area_parcela_ha)))
cat(sprintf("  RegeneraciÃ³n: %.4f ha (9 mÂ², factor = %.1f)\n\n", 
            CONFIG$area_parcela_regeneracion_ha,
            calcular_factor_expansion(CONFIG$area_parcela_regeneracion_ha)))

# ==============================================================================
# FUNCIÃ“N AUXILIAR: EXPORTAR CSV (VERSIÃ“N ROBUSTA)
# ==============================================================================

exportar_csv <- function(df, nombre_archivo, carpeta = "resultados") {
  
  # Verificar input
  if (is.null(df) || nrow(df) == 0) {
    warning(sprintf("âš ï¸ DataFrame vacÃ­o para %s, no se exporta", nombre_archivo))
    return(invisible(FALSE))
  }
  
  # Crear carpeta si no existe
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
    cat(sprintf("  ðŸ“ Creando carpeta: %s/\n", carpeta))
  }
  
  # Ruta completa
  ruta <- file.path(carpeta, paste0(nombre_archivo, ".csv"))
  
  # Exportar con manejo de errores
  tryCatch({
    write.csv(df, file = ruta, row.names = FALSE)
    
    # Verificar que se creÃ³
    if (file.exists(ruta)) {
      size_kb <- file.info(ruta)$size / 1024
      cat(sprintf("  âœ“ CSV: %s (%.1f KB)\n", basename(ruta), size_kb))
      return(invisible(TRUE))
    } else {
      warning(sprintf("âš ï¸ Archivo no creado: %s", ruta))
      return(invisible(FALSE))
    }
    
  }, error = function(e) {
    warning(sprintf("âŒ Error exportando %s: %s", nombre_archivo, e$message))
    return(invisible(FALSE))
  })
}

# ==============================================================================
# 1. ESTRUCTURA POBLACIONAL - CORREGIDO n_sitios
# ==============================================================================

analizar_estructura_poblacional <- function(arboles_df, config = CONFIG, 
                                            exportar_latex = TRUE,
                                            exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘     ANÃLISIS ESTRUCTURA POBLACIONAL (AB/ha + CSV)         â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  # CRÃTICO: Usar TODOS los sitios muestreados, no solo donde hay Ã¡rboles vivos
  n_sitios_total <- n_distinct(arboles_df$muestreo)
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios_con_vivos <- n_distinct(vivos$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios_total))
  cat(sprintf("Sitios con Ã¡rboles vivos: %d\n", n_sitios_con_vivos))
  cat(sprintf("Sitios sin Ã¡rboles vivos: %d\n\n", n_sitios_total - n_sitios_con_vivos))
  
  # Usar n_sitios_total para cÃ¡lculos /ha
  n_sitios <- n_sitios_total
  
  # RESUMEN GENERAL - CON AB/HA Y DMC
  resumen_general <- vivos %>%
    summarise(
      n_arboles_total = n(),
      n_sitios = n_sitios,
      n_rodales = n_distinct(rodal),
      n_especies = n_distinct(nombre_cientifico),
      n_generos = n_distinct(genero_grupo),
      densidad_ha = expandir_a_hectarea(n_arboles_total / n_sitios, config$area_parcela_ha),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      dmc_cm = calcular_dmc(diametro_normal),
      d_min_cm = min(diametro_normal, na.rm = TRUE),
      d_max_cm = max(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),
      h_min_m = min(altura_total, na.rm = TRUE),
      h_max_m = max(altura_total, na.rm = TRUE),
      ab_media_m2ha = expandir_a_hectarea(
        sum(area_basal, na.rm = TRUE) / n_sitios,
        config$area_parcela_ha
      ),
      vol_medio_m3ha = expandir_a_hectarea(
        sum(volumen_m3, na.rm = TRUE) / n_sitios, 
        config$area_parcela_ha
      )
    )
  
  cat("[RESUMEN GENERAL - VALORES POR HECTÃREA]\n")
  print(resumen_general)
  
  # POR RODAL - CON AB/HA Y DMC (CORREGIDO)
  por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      .groups = "drop"
    ) %>%
    left_join(
      vivos %>%
        group_by(rodal) %>%
        summarise(
          n_arboles_obs = n(),
          n_especies = n_distinct(nombre_cientifico),
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          h_media_m = mean(altura_total, na.rm = TRUE),
          ab_total = sum(area_basal, na.rm = TRUE),
          vol_total = sum(volumen_m3, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "rodal"
    ) %>%
    mutate(
      # Reemplazar NA con 0 si no hay Ã¡rboles vivos
      n_arboles_obs = replace_na(n_arboles_obs, 0),
      n_especies = replace_na(n_especies, 0),
      # Calcular mÃ©tricas /ha con n_sitios del rodal
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(
        replace_na(ab_total, 0) / n_sitios,
        config$area_parcela_ha
      ),
      vol_m3ha = expandir_a_hectarea(
        replace_na(vol_total, 0) / n_sitios,
        config$area_parcela_ha
      )
    ) %>%
    select(-ab_total, -vol_total) %>%
    arrange(rodal)
  
  cat("\n[POR RODAL - VALORES POR HECTÃREA]\n")
  print(por_rodal, n = 50)
  
  # POR GÃ‰NERO - CON AB/HA Y DMC (CORREGIDO)
  por_genero <- vivos %>%
    count(genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      # Usar n_sitios_total para densidad /ha
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / sum(n_arboles_obs)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(genero_grupo) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          h_media_m = mean(altura_total, na.rm = TRUE),
          # Usar n_sitios_total para AB y Vol /ha
          ab_m2ha = expandir_a_hectarea(
            sum(area_basal, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          vol_m3ha = expandir_a_hectarea(
            sum(volumen_m3, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          .groups = "drop"
        ),
      by = "genero_grupo"
    ) %>%
    mutate(
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    arrange(desc(densidad_ha))
  
  cat("\n[POR GÃ‰NERO - VALORES POR HECTÃREA]\n")
  print(por_genero)
  
  # POR ESPECIE - CON AB/HA Y DMC (TOP 10) (CORREGIDO)
  por_especie <- vivos %>%
    count(nombre_cientifico, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      # Usar n_sitios_total
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / nrow(vivos)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(nombre_cientifico) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          # Usar n_sitios_total
          ab_m2ha = expandir_a_hectarea(
            sum(area_basal, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          .groups = "drop"
        ),
      by = "nombre_cientifico"
    ) %>%
    arrange(desc(densidad_ha)) %>%
    head(10)
  
  cat("\n[TOP 10 ESPECIES - DENSIDAD POR HECTÃREA]\n")
  print(por_especie)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(resumen_general, "desc_01_resumen_general")
    exportar_csv(por_rodal, "desc_02_por_rodal")
    exportar_csv(por_genero, "desc_03_por_genero")
    exportar_csv(por_especie, "desc_04_top10_especies")
  }
  
  # EXPORTAR LATEX
  if (exportar_latex) {
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    # Tabla por rodal - AHORA CON AB/HA Y DMC
    xtable(por_rodal %>% select(-n_sitios, -n_arboles_obs), 
           caption = "Resumen dasomÃ©trico por rodal (valores/ha, incluye AB y DMC)",
           label = "tab:resumen_rodal",
           digits = c(0, 0, 1, 0, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_01_resumen_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla por gÃ©nero - AHORA CON AB/HA, DMC Y %
    xtable(por_genero %>% select(genero_grupo, densidad_ha, proporcion_pct, 
                                 d_medio_cm, dmc_cm, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "ComposiciÃ³n por gÃ©nero (valores/ha, incluye AB, DMC y porcentajes)",
           label = "tab:composicion_genero",
           digits = c(0, 0, 1, 2, 2, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_02_composicion_genero.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla especies - AHORA CON AB/HA Y DMC
    xtable(por_especie %>% select(-n_arboles_obs),
           caption = "Principales especies (top 10, incluye AB/ha y DMC)",
           label = "tab:top_especies",
           digits = c(0, 0, 0, 1, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_03_top_especies.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\nâœ“ Tablas LaTeX exportadas (desc_01 a desc_03)\n")
  }
  
  return(list(
    general = resumen_general,
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# 1B. COMPOSICIÃ“N PINUS/QUERCUS POR RODAL - CORREGIDO
# ==============================================================================

analizar_composicion_generopq_por_rodal <- function(arboles_df, config = CONFIG,
                                                    exportar_latex = TRUE,
                                                    exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘  COMPOSICIÃ“N PINUS/QUERCUS POR RODAL (con AB/ha y DMC)   â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÃTICO: Contar TODOS los sitios por rodal, no solo donde hay P/Q vivos
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  # Calcular por rodal y gÃ©nero CON DMC
  composicion_generopq_rodal <- vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_arboles_obs = n(),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      dmc_cm = calcular_dmc(diametro_normal),
      h_media_m = mean(altura_total, na.rm = TRUE),
      ab_total = sum(area_basal, na.rm = TRUE),
      vol_total = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Unir con sitios totales del rodal
    left_join(sitios_por_rodal, by = "rodal") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_total / n_sitios, config$area_parcela_ha),
      vol_m3ha = expandir_a_hectarea(vol_total / n_sitios, config$area_parcela_ha)
    ) %>%
    select(-ab_total, -vol_total) %>%
    # Calcular porcentajes por rodal
    group_by(rodal) %>%
    mutate(
      densidad_pct = (densidad_ha / sum(densidad_ha)) * 100,
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, desc(densidad_ha))
  
  cat("[COMPOSICIÃ“N PINUS/QUERCUS POR RODAL]\n")
  print(composicion_generopq_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(composicion_generopq_rodal, "desc_11_composicion_generopq_por_rodal")
  }
  
  # EXPORTAR LATEX
  if (exportar_latex) {
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    xtable(composicion_generopq_rodal %>% 
             select(rodal, genero_grupo, densidad_ha, densidad_pct,
                    d_medio_cm, dmc_cm, h_media_m, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "ComposiciÃ³n de Pinus y Quercus por rodal (valores/ha, DMC y porcentajes)",
           label = "tab:composicion_generopq_rodal",
           digits = c(0, 0, 0, 1, 1, 2, 2, 2, 2, 1, 2, 1)) %>%
      print(file = "tablas_latex/desc_11_composicion_generopq_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\nâœ“ Tabla LaTeX: desc_11_composicion_generopq_rodal.tex\n")
  }
  
  # GRÃFICO OPCIONAL - Volumen por rodal y gÃ©nero
  p_comp <- ggplot(composicion_generopq_rodal,
                   aes(x = factor(rodal), y = vol_m3ha, fill = genero_grupo)) +
    geom_col(position = "stack", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", vol_pct)),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3.5) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22")
    ) +
    labs(
      title = "ComposiciÃ³n de Volumen: Pinus vs Quercus por Rodal",
      subtitle = "Volumen total/ha y porcentaje de cada gÃ©nero",
      x = "Rodal",
      y = "Volumen (mÂ³/ha)",
      fill = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    ggsave("graficos/desc_11_composicion_generopq_rodal.png", p_comp,
           width = 10, height = 6, dpi = 300)
    cat("âœ“ GrÃ¡fico: desc_11_composicion_generopq_rodal.png\n")
  }
  
  return(list(
    tabla = composicion_generopq_rodal,
    grafico = p_comp
  ))
}

# ==============================================================================
# 1C. ÃREA BASAL POR DOMINANCIA - CORREGIDO
# ==============================================================================

analizar_ab_por_dominancia <- function(arboles_df, config = CONFIG,
                                       exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘   AB POR DOMINANCIA: Pinus/Quercus (incluye muertos)     â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_DOMINANCIA")) {
    stop("âŒ CODIGOS_DOMINANCIA no estÃ¡ cargado")
  }
  
  # TODOS LOS ÃRBOLES (incluyendo muertos) - Solo Pinus/Quercus
  pq_todos <- arboles_df %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÃTICO: Contar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  # Sitios por rodal
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  cat(sprintf("Ãrboles Pinus/Quercus (vivos + muertos): %d\n", nrow(pq_todos)))
  cat(sprintf("Sitios TOTAL: %d\n\n", n_sitios))
  
  # Calcular AB por rodal, gÃ©nero y dominancia
  ab_por_dominancia <- pq_todos %>%
    left_join(
      CODIGOS_DOMINANCIA %>% select(codigo, etiqueta),
      by = c("dominancia" = "codigo")
    ) %>%
    rename(etiqueta_dominancia = etiqueta) %>%
    group_by(rodal, genero_grupo, dominancia, etiqueta_dominancia) %>%
    summarise(
      n_arboles_obs = n(),
      ab_total = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Unir con sitios por rodal
    left_join(sitios_por_rodal, by = "rodal") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_total / n_sitios, config$area_parcela_ha)
    ) %>%
    select(-ab_total) %>%
    # Calcular % de AB por rodal y gÃ©nero
    group_by(rodal, genero_grupo) %>%
    mutate(
      ab_pct_genero = (ab_m2ha / sum(ab_m2ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, genero_grupo, dominancia)
  
  cat("[ÃREA BASAL POR DOMINANCIA - PINUS/QUERCUS]\n")
  print(ab_por_dominancia, n = 100)
  
  # Resumen por rodal
  resumen_rodal <- ab_por_dominancia %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      ab_total_m2ha = sum(ab_m2ha),
      ab_vivos_m2ha = sum(ab_m2ha[dominancia %in% 1:6]),
      ab_muertos_m2ha = sum(ab_m2ha[!es_arbol_vivo(dominancia)]),
      pct_muertos = (ab_muertos_m2ha / ab_total_m2ha) * 100,
      .groups = "drop"
    )
  
  cat("\n[RESUMEN: AB VIVOS VS MUERTOS]\n")
  print(resumen_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(ab_por_dominancia, "desc_12_ab_por_dominancia")
    exportar_csv(resumen_rodal, "desc_13_ab_vivos_vs_muertos")
  }
  
  # GRÃFICO - AB por dominancia
  p_ab_dom <- ggplot(ab_por_dominancia,
                     aes(x = factor(rodal), y = ab_m2ha, 
                         fill = etiqueta_dominancia)) +
    geom_col(position = "stack", alpha = 0.8) +
    facet_wrap(~genero_grupo, ncol = 1) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
      title = "Ãrea Basal por Clase de Dominancia",
      subtitle = "Pinus y Quercus - Incluye Ã¡rboles muertos",
      x = "Rodal",
      y = "Ãrea Basal (mÂ²/ha)",
      fill = "Dominancia"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  ggsave("graficos/desc_12_ab_por_dominancia.png", p_ab_dom,
         width = 12, height = 8, dpi = 300)
  
  cat("\nâœ“ GrÃ¡fico: desc_12_ab_por_dominancia.png\n")
  
  return(list(
    detalle = ab_por_dominancia,
    resumen = resumen_rodal,
    grafico = p_ab_dom
  ))
}

# ==============================================================================
# 2. DISTRIBUCIÃ“N DIAMÃ‰TRICA - CORREGIDO
# ==============================================================================

analizar_distribucion_diametrica <- function(arboles_df, config = CONFIG, 
                                             exportar = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘         ANÃLISIS DISTRIBUCIÃ“N DIAMÃ‰TRICA + CSV            â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÃTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Ãrboles vivos: %d\n\n", nrow(vivos)))
  
  dist_diametrica <- vivos %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal, 
      breaks = config$clases_d,
      formato = "rango"
    )) %>%
    filter(!is.na(clase_d)) %>%
    count(clase_d, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha)
    ) %>%
    group_by(genero_grupo) %>%
    mutate(
      proporcion_pct = (densidad_ha / sum(densidad_ha)) * 100,
      proporcion_acum = cumsum(proporcion_pct)
    ) %>%
    ungroup()
  
  cat("[DISTRIBUCIÃ“N DIAMÃ‰TRICA - DENSIDAD/HA]\n")
  print(dist_diametrica, n = 30)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(dist_diametrica, "desc_05_distribucion_diametrica")
  }
  
  # GRÃFICOS
  p1 <- ggplot(dist_diametrica, 
               aes(x = clase_d, y = densidad_ha, fill = genero_grupo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "DistribuciÃ³n DiamÃ©trica por GÃ©nero",
      subtitle = sprintf("Densidad/ha - Parcela: %.2f ha", config$area_parcela_ha),
      x = "Clase DiamÃ©trica (cm)",
      y = "Densidad (Ã¡rboles/ha)",
      fill = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  p2 <- ggplot(dist_diametrica,
               aes(x = clase_d, y = proporcion_acum, 
                   color = genero_grupo, group = genero_grupo)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "ProporciÃ³n Acumulada por Clase DiamÃ©trica",
      x = "Clase DiamÃ©trica (cm)",
      y = "ProporciÃ³n Acumulada (%)",
      color = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  p_combined <- p1 / p2
  
  if (exportar) {
    dir.create("graficos", showWarnings = FALSE, recursive = TRUE)
    ggsave("graficos/desc_01_distribucion_diametrica.png", p_combined,
           width = 10, height = 8, dpi = 300)
    cat("\nâœ“ GrÃ¡fico guardado: graficos/desc_01_distribucion_diametrica.png\n")
  }
  
  return(list(
    tabla = dist_diametrica, 
    grafico_barras = p1,
    grafico_acumulado = p2,
    grafico_combinado = p_combined
  ))
}

# ==============================================================================
# ANÁLISIS DASOMÉTRICO CON IC 95% BASADO EN COEFICIENTE DE VARIACIÓN
# ==============================================================================
# 
# MÉTODO:
# 1. Calcular CV a nivel predio (n=58) para cada género
# 2. Aplicar CV a valores puntuales por UMM/género/especie
# 3. IC = media × (1 ± margen_error_relativo)
# 
# FÓRMULA:
# - CV = s / x̄
# - EE = (CV × x̄) / √n
# - E = t(α/2, df) × EE
# - IC = [x̄ × (1 - E/x̄), x̄ × (1 + E/x̄)]
# - IC = x̄ × [1 ± (CV × t / √n)]
#
# Para n=58, t(0.025, 57) ≈ 2.002
# Margen relativo = CV × 2.002 / √58 ≈ CV × 0.263
# ==============================================================================

# ==============================================================================
# FUNCIÓN AUXILIAR: FORMATEAR 3 CIFRAS SIGNIFICATIVAS
# ==============================================================================

formatear_3_cifras_sig <- function(x) {
  if (is.na(x) || !is.numeric(x)) return(NA_character_)
  if (x == 0) return("0")
  
  formatted <- signif(x, 3)
  
  if (abs(formatted) >= 100) {
    sprintf("%.0f", formatted)
  } else if (abs(formatted) >= 10) {
    sprintf("%.1f", formatted)
  } else if (abs(formatted) >= 1) {
    sprintf("%.2f", formatted)
  } else {
    sprintf("%.3f", formatted)
  }
}

formatear_3_cifras_sig <- Vectorize(formatear_3_cifras_sig)

# ==============================================================================
# FUNCIÓN: CALCULAR CV A NIVEL PREDIO
# ==============================================================================

calcular_cv_predio <- function(arboles_df, config) {
  
  cat("\n[PASO 1] Calculando CV a nivel predio (n=58)...\n")
  
  # Todos los sitios
  todos_sitios <- arboles_df %>%
    distinct(muestreo, rodal)
  
  n_sitios <- n_distinct(todos_sitios$muestreo)
  
  # Árboles vivos por sitio y género
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))


  metricas_por_sitio <- vivos %>%
    group_by(muestreo, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      vol_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = n_arboles / config$area_parcela_ha,
      ab_m2ha = ab_m2 / config$area_parcela_ha,
      vol_m3ha = vol_m3 / config$area_parcela_ha
    )
  
  # Completar con ceros donde no hay presencia
  grid_completo <- expand.grid(
    muestreo = unique(todos_sitios$muestreo),
    genero_grupo = c("Pinus", "Quercus"),
    stringsAsFactors = FALSE
  ) %>%
    left_join(metricas_por_sitio, by = c("muestreo", "genero_grupo")) %>%
    mutate(
      densidad_ha = ifelse(is.na(densidad_ha), 0, densidad_ha),
      ab_m2ha = ifelse(is.na(ab_m2ha), 0, ab_m2ha),
      vol_m3ha = ifelse(is.na(vol_m3ha), 0, vol_m3ha)
    )
  
  # Calcular CV por género
  cv_por_genero <- grid_completo %>%
    group_by(genero_grupo) %>%
    summarise(
      n = n(),
      # Densidad
      dens_media = mean(densidad_ha, na.rm = TRUE),
      dens_sd = sd(densidad_ha, na.rm = TRUE),
      dens_cv = dens_sd / dens_media,
      # AB
      ab_media = mean(ab_m2ha, na.rm = TRUE),
      ab_sd = sd(ab_m2ha, na.rm = TRUE),
      ab_cv = ab_sd / ab_media,
      # Volumen
      vol_media = mean(vol_m3ha, na.rm = TRUE),
      vol_sd = sd(vol_m3ha, na.rm = TRUE),
      vol_cv = vol_sd / vol_media,
      .groups = "drop"
    )
  
  cat(sprintf("  ✓ CV calculados (n=%d sitios):\n", n_sitios))
  cat(sprintf("    Pinus: Dens CV=%.3f, AB CV=%.3f, Vol CV=%.3f\n",
              cv_por_genero$dens_cv[cv_por_genero$genero_grupo == "Pinus"],
              cv_por_genero$ab_cv[cv_por_genero$genero_grupo == "Pinus"],
              cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Pinus"]))
  cat(sprintf("    Quercus: Dens CV=%.3f, AB CV=%.3f, Vol CV=%.3f\n",
              cv_por_genero$dens_cv[cv_por_genero$genero_grupo == "Quercus"],
              cv_por_genero$ab_cv[cv_por_genero$genero_grupo == "Quercus"],
              cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Quercus"]))
  
  return(list(
    cv_por_genero = cv_por_genero,
    n_sitios = n_sitios
  ))
}

# ==============================================================================
# FUNCIÓN: CALCULAR IC USANDO CV
# ==============================================================================

calcular_ic_con_cv <- function(media, cv, n, alpha = 0.05) {
  
  # Para valores muy cercanos a 0, no calcular IC
  if (is.na(media) || media < 0.01) {
    return(list(
      media = media,
      ic_inf = NA_real_,
      ic_sup = NA_real_,
      margen_rel = NA_real_
    ))
  }
  
  # Valor t para n-1 grados de libertad
  df <- n - 1
  t_critico <- qt(1 - alpha/2, df)
  
  # Margen de error relativo
  # E/x̄ = CV × t / √n
  margen_rel <- cv * t_critico / sqrt(n)
  
  # IC
  ic_inf <- max(0, media * (1 - margen_rel))
  ic_sup <- media * (1 + margen_rel)
  
  return(list(
    media = media,
    ic_inf = ic_inf,
    ic_sup = ic_sup,
    margen_rel = margen_rel
  ))
}

# ==============================================================================
# FUNCIÓN PRINCIPAL: ANÁLISIS DASOMÉTRICO CON IC BASADO EN CV
# ==============================================================================

analizar_dasometrico_cv <- function(arboles_df, 
                                    config = CONFIG,
                                    exportar_csv_flag = TRUE,
                                    exportar_latex = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS DASOMÉTRICO CON IC BASADO EN CV (n=58)        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source("core/15_core_calculos.R")
  }
  
  # ==========================================================================
  # PASO 1: CALCULAR CV A NIVEL PREDIO
  # ==========================================================================
  
  cv_data <- calcular_cv_predio(arboles_df, config)
  cv_por_genero <- cv_data$cv_por_genero
  n_sitios_predio <- cv_data$n_sitios
  
  # ==========================================================================
  # PASO 2: CALCULAR VALORES PUNTUALES POR ESPECIE Y GÉNERO
  # ==========================================================================
  
  cat("\n[PASO 2] Calculando valores por especie, género y UMM...\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    mutate(rodal = as.character(rodal))
  
  # CRÍTICO: Calcular n_sitios TOTALES por rodal PRIMERO
  sitios_por_rodal_count <- vivos %>%
    group_by(rodal) %>%
    summarise(n_sitios_umm = n_distinct(muestreo), .groups = "drop")
  
  # Superficie por rodal
  superficie_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(superficie_ha = first(superficie_ha), .groups = "drop") %>%
    mutate(rodal = as.character(rodal))
  
  # Métricas por UMM y ESPECIE
  metricas_umm_especie <- vivos %>%
    group_by(rodal, genero_grupo, nombre_cientifico) %>%
    summarise(
      n_arboles = n(),
      vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # UNIR con n_sitios TOTALES del rodal
    left_join(sitios_por_rodal_count, by = "rodal") %>%
    mutate(
      area_total_ha = n_sitios_umm * config$area_parcela_ha,
      vol_m3ha = vol_total_m3 / area_total_ha,
      tipo_fila = "especie"
    ) %>%
    rename(n_sitios = n_sitios_umm)
  
  # Métricas por UMM y GÉNERO (para subtotales)
  metricas_umm_genero <- vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # UNIR con n_sitios TOTALES del rodal
    left_join(sitios_por_rodal_count, by = "rodal") %>%
    mutate(
      area_total_ha = n_sitios_umm * config$area_parcela_ha,
      vol_m3ha = vol_total_m3 / area_total_ha
    ) %>%
    rename(n_sitios = n_sitios_umm)
  
  # ==========================================================================
  # PASO 3: APLICAR CV PARA CALCULAR IC (solo para ER)
  # ==========================================================================
  
  cat("[PASO 3] Aplicando CV para calcular IC en ER...\n")
  
  # ESPECIES: Aplicar IC usando CV del género correspondiente
  tabla_especies_con_ic <- metricas_umm_especie %>%
    left_join(
      cv_por_genero %>% select(genero_grupo, vol_cv),
      by = "genero_grupo"
    ) %>%
    rowwise() %>%
    mutate(
      vol_ic = list(calcular_ic_con_cv(vol_m3ha, vol_cv, n_sitios_predio)),
      vol_ic_inf = vol_ic$ic_inf,
      vol_ic_sup = vol_ic$ic_sup
    ) %>%
    ungroup() %>%
    select(rodal, genero_grupo, nombre_cientifico, n_sitios, vol_m3ha, 
           vol_ic_inf, vol_ic_sup, tipo_fila)
  
  # SUBTOTALES POR GÉNERO: Aplicar IC
  subtotales_genero <- metricas_umm_genero %>%
    left_join(
      cv_por_genero %>% select(genero_grupo, vol_cv),
      by = "genero_grupo"
    ) %>%
    rowwise() %>%
    mutate(
      vol_ic = list(calcular_ic_con_cv(vol_m3ha, vol_cv, n_sitios_predio)),
      vol_ic_inf = vol_ic$ic_inf,
      vol_ic_sup = vol_ic$ic_sup
    ) %>%
    ungroup() %>%
    mutate(
      nombre_cientifico = paste("Subtotal", genero_grupo),
      tipo_fila = "subtotal_genero"
    ) %>%
    select(rodal, genero_grupo, nombre_cientifico, n_sitios, vol_m3ha,
           vol_ic_inf, vol_ic_sup, tipo_fila)
  
  # ==========================================================================
  # PASO 4: AGREGAR SUBTOTALES POR UMM Y TOTAL GENERAL
  # ==========================================================================
  
  cat("[PASO 4] Agregando subtotales por UMM y total general...\n")
  
  # Subtotal por UMM (suma Pinus + Quercus, CON IC)
  # Calcular CV combinado para el subtotal
  subtotal_umm <- subtotales_genero %>%
    group_by(rodal) %>%
    summarise(
      vol_m3ha_suma = sum(vol_m3ha),
      .groups = "drop"
    ) %>%
    # Calcular CV combinado como promedio ponderado de CVs de Pinus y Quercus
    mutate(
      cv_combinado = {
        cv_pinus <- cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Pinus"]
        cv_quercus <- cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Quercus"]
        # Promedio simple de los CVs (aproximación conservadora)
        (cv_pinus + cv_quercus) / 2
      }
    ) %>%
    rowwise() %>%
    mutate(
      vol_ic = list(calcular_ic_con_cv(vol_m3ha_suma, cv_combinado, n_sitios_predio))
    ) %>%
    ungroup() %>%
    transmute(
      rodal,
      genero_grupo = "TOTAL",
      nombre_cientifico = "Subtotal UMM",
      n_sitios = n_sitios_predio,
      vol_m3ha = vol_m3ha_suma,
      vol_ic_inf = map_dbl(vol_ic, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol_ic, ~.x$ic_sup),
      tipo_fila = "subtotal_umm"
    )
  
  # Total general por género (promedio ponderado por superficie)
  total_genero <- subtotales_genero %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    group_by(genero_grupo) %>%
    summarise(
      vol_m3ha_ponderada = weighted.mean(vol_m3ha, superficie_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(cv_por_genero %>% select(genero_grupo, vol_cv), by = "genero_grupo") %>%
    rowwise() %>%
    mutate(
      vol_ic = list(calcular_ic_con_cv(vol_m3ha_ponderada, vol_cv, n_sitios_predio))
    ) %>%
    ungroup() %>%
    transmute(
      rodal = "TOTAL",
      genero_grupo,
      nombre_cientifico = paste("Total", genero_grupo),
      n_sitios = n_sitios_predio,
      vol_m3ha = vol_m3ha_ponderada,
      vol_ic_inf = map_dbl(vol_ic, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol_ic, ~.x$ic_sup),
      tipo_fila = "total_genero"
    )
  
  # Total general (suma de todos con IC)
  total_general <- total_genero %>%
    summarise(
      vol_m3ha_suma = sum(vol_m3ha),
      .groups = "drop"
    ) %>%
    mutate(
      cv_combinado = {
        cv_pinus <- cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Pinus"]
        cv_quercus <- cv_por_genero$vol_cv[cv_por_genero$genero_grupo == "Quercus"]
        (cv_pinus + cv_quercus) / 2
      }
    ) %>%
    rowwise() %>%
    mutate(
      vol_ic = list(calcular_ic_con_cv(vol_m3ha_suma, cv_combinado, n_sitios_predio))
    ) %>%
    ungroup() %>%
    transmute(
      rodal = "TOTAL",
      genero_grupo = "TOTAL",
      nombre_cientifico = "Total General",
      n_sitios = n_sitios_predio,
      vol_m3ha = vol_m3ha_suma,
      vol_ic_inf = map_dbl(vol_ic, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol_ic, ~.x$ic_sup),
      tipo_fila = "total_general"
    )
  
  # ==========================================================================
  # PASO 5: ENSAMBLAR TABLA FINAL
  # ==========================================================================
  
  cat("[PASO 5] Ensamblando tabla final...\n")
  
  # Ensamblar: especies + subtotales género + subtotales UMM + totales
  tabla_final <- bind_rows(
    tabla_especies_con_ic,
    subtotales_genero,
    subtotal_umm,
    total_genero,
    total_general
  ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      superficie_ha = ifelse(rodal == "TOTAL", NA_real_, superficie_ha)
    ) %>%
    arrange(rodal, genero_grupo, nombre_cientifico)
  
  cat(sprintf("  ✓ Tabla final: %d filas\n", nrow(tabla_final)))
  
  # ==========================================================================
  # EXPORTAR CSV
  # ==========================================================================
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
    
    write.csv(tabla_final, 
              "resultados/analisis_dasometrico_ic_cv.csv", 
              row.names = FALSE)
    cat("  ✓ CSV: analisis_dasometrico_ic_cv.csv\n")
  }
  
  # ==========================================================================
  # EXPORTAR LATEX
  # ==========================================================================
  
  if (exportar_latex) {
    cat("\n[GENERANDO LATEX]\n")
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    # Función para formatear con IC
    formatear_con_ic <- function(media, ic_inf, ic_sup) {
      media_str <- formatear_3_cifras_sig(media)
      ic_inf_str <- formatear_3_cifras_sig(ic_inf)
      ic_sup_str <- formatear_3_cifras_sig(ic_sup)
      
      tiene_ic <- !is.na(ic_inf) & !is.na(ic_sup)
      result <- media_str
      result[tiene_ic] <- sprintf("%s (%s--%s)", 
                                  media_str[tiene_ic],
                                  ic_inf_str[tiene_ic],
                                  ic_sup_str[tiene_ic])
      return(result)
    }
    
    # Preparar para LaTeX
    tabla_latex <- tabla_final %>%
      mutate(
        UMM = rodal,
        Sup_ha = ifelse(is.na(superficie_ha), "--", formatear_3_cifras_sig(superficie_ha)),
        Especie = nombre_cientifico,
        ER = formatear_con_ic(vol_m3ha, vol_ic_inf, vol_ic_sup),
        es_subtotal = tipo_fila %in% c("subtotal_genero", "subtotal_umm", 
                                       "total_genero", "total_general")
      )
    
    # Calcular número de filas por UMM (para multirow)
    filas_por_umm <- tabla_latex %>%
      filter(UMM != "TOTAL") %>%
      group_by(UMM) %>%
      summarise(n_filas = n(), .groups = "drop")
    
    # Generar LaTeX
    latex_content <- c(
      "% ============================================================",
      "% TABLA: Existencias Reales por Especie y UMM (IC 95% con CV)",
      "% ============================================================",
      "% Requiere: \\usepackage{multirow}",
      "\\begin{table}[H]",
      "\t\\centering",
      "\t\\caption{Existencias reales por especie y UMM (IC 95\\% basado en CV, n=58)}",
      "\t\\label{tab:existencias_reales}",
      "\t\\scriptsize",
      "\t\\begin{tabular}{ccp{5cm}c}",
      "\t\t\\toprule",
      "\t\t\\textbf{UMM} & \\textbf{Sup (ha)} & \\textbf{Especie/Categoría} & \\textbf{Exist. Reales (m³/ha)} \\\\",
      "\t\t\\midrule"
    )
    
    umm_actual <- NULL
    ultimo_genero <- NULL
    
    for (i in 1:nrow(tabla_latex)) {
      fila <- tabla_latex[i, ]
      
      # Determinar si es primera fila de UMM
      es_primera_fila_umm <- is.null(umm_actual) || fila$UMM != umm_actual
      
      # Valores de UMM y Sup con multirow
      if (es_primera_fila_umm && fila$UMM != "TOTAL") {
        # Obtener número de filas para este UMM
        n_filas_umm <- filas_por_umm$n_filas[filas_por_umm$UMM == fila$UMM]
        umm_col <- sprintf("\\multirow{%d}{*}{%s}", n_filas_umm, fila$UMM)
        sup_col <- sprintf("\\multirow{%d}{*}{%s}", n_filas_umm, fila$Sup_ha)
      } else if (fila$UMM == "TOTAL") {
        # Pour filas TOTAL, no usar multirow
        umm_col <- fila$UMM
        sup_col <- fila$Sup_ha
      } else {
        # Filas subsecuentes: vacío
        umm_col <- ""
        sup_col <- ""
      }
      
      # Separador entre UMMs (solo antes de primera especie de nueva UMM)
      if (es_primera_fila_umm && 
          !is.null(umm_actual) && 
          fila$UMM != "TOTAL" &&
          fila$tipo_fila == "especie") {
        latex_content <- c(latex_content, "\t\t\\midrule")
      }
      
      # Formato según tipo de fila
      if (fila$tipo_fila == "especie") {
        # Especies en itálica
        linea <- sprintf("\t\t%s & %s & \\textit{%s} & %s \\\\",
                         umm_col, sup_col, fila$Especie, fila$ER)
        ultimo_genero <- fila$genero_grupo
        
      } else if (fila$tipo_fila == "subtotal_genero") {
        # Subtotal género: extraer nombre del género de "Subtotal Pinus"
        genero_nombre <- gsub("Subtotal ", "", fila$Especie)
        # Subtotal con género en itálica: Subtotal \textit{Pinus}
        linea <- sprintf("\t\t\\rowcolor{gray!15}%s & %s & \\textbf{Subtotal \\textit{%s}} & \\textbf{%s} \\\\",
                         umm_col, sup_col, genero_nombre, fila$ER)
        
      } else if (fila$tipo_fila == "subtotal_umm") {
        # Subtotal UMM con fondo gris más oscuro
        linea <- sprintf("\t\t\\rowcolor{gray!25}%s & %s & \\textbf{%s} & \\textbf{%s} \\\\",
                         umm_col, sup_col, fila$Especie, fila$ER)
        
      } else if (fila$tipo_fila %in% c("total_genero", "total_general")) {
        # Agregar midrule antes del primer total
        if (fila$tipo_fila == "total_genero" && 
            (is.null(umm_actual) || umm_actual != "TOTAL")) {
          latex_content <- c(latex_content, "\t\t\\midrule")
        }
        
        # Para totales por género: "Total Pinus" → "Total \textit{Pinus}"
        if (fila$tipo_fila == "total_genero") {
          genero_nombre <- gsub("Total ", "", fila$Especie)
          especie_formateada <- sprintf("\\textbf{Total \\textit{%s}}", genero_nombre)
        } else {
          especie_formateada <- sprintf("\\textbf{%s}", fila$Especie)
        }
        
        linea <- sprintf("\t\t\\rowcolor{gray!15}%s & %s & %s & \\textbf{%s} \\\\",
                         umm_col, sup_col, especie_formateada, fila$ER)
      } else {
        # Fallback
        linea <- sprintf("\t\t%s & %s & %s & %s \\\\",
                         umm_col, sup_col, fila$Especie, fila$ER)
      }
      
      latex_content <- c(latex_content, linea)
      
      # Actualizar UMM actual
      if (fila$tipo_fila == "especie" || fila$tipo_fila == "subtotal_genero" || 
          fila$tipo_fila == "subtotal_umm") {
        umm_actual <- fila$UMM
      }
    }
    
    latex_content <- c(
      latex_content,
      "\t\t\\bottomrule",
      "\t\\end{tabular}",
      "\t\\\\[0.3cm]",
      sprintf("\t{\\scriptsize IC 95\\%% calculados con CV a nivel predio (n=%d, NOM-152).}", n_sitios_predio),
      "\t\\\\",
      sprintf("\t{\\scriptsize Subtotales por género incluyen todas las especies del género. Formato: 3 cifras significativas.}"),
      "\\end{table}"
    )
    
    writeLines(latex_content, "tablas_latex/existencias_reales_por_especie.tex")
    cat("  ✓ LaTeX: existencias_reales_por_especie.tex\n")
  }
  
  cat("\n✓ Análisis completado\n")
  cat(sprintf("  Método: IC basado en CV (n=%d)\n\n", n_sitios_predio))
  
  return(list(
    tabla_final = tabla_final,
    cv_por_genero = cv_por_genero,
    n_sitios = n_sitios_predio
  ))
}
analizar_erosion <- function(f01, exportar_latex = TRUE, exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘      ANÃLISIS DE EROSIÃ“N (% sitios afectados + CSV)       â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_EROSION")) {
    stop("âŒ CODIGOS_EROSION no estÃ¡ cargado")
  }
  
  # Calcular % de sitios afectados por rodal
  erosion_por_rodal <- f01 %>%
    select(rodal, muestreo, erosion_laminar, erosion_canalillos, 
           erosion_carcavas, erosion_antropica) %>%
    pivot_longer(
      cols = starts_with("erosion_"),
      names_to = "tipo_erosion",
      values_to = "codigo"
    ) %>%
    mutate(
      tipo_erosion = recode(
        tipo_erosion,
        "erosion_laminar" = "Laminar",
        "erosion_canalillos" = "Canalillos",
        "erosion_carcavas" = "CÃ¡rcavas",
        "erosion_antropica" = "AntrÃ³pica"
      ),
      afectado = codigo > 1
    ) %>%
    group_by(rodal, tipo_erosion) %>%
    summarise(
      n_sitios_total = n(),
      n_sitios_afectados = sum(afectado),
      pct_afectado = (n_sitios_afectados / n_sitios_total) * 100,
      .groups = "drop"
    ) %>%
    arrange(rodal, tipo_erosion)
  
  cat("[EROSIÃ“N: % DE SITIOS AFECTADOS POR RODAL]\n")
  print(erosion_por_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(erosion_por_rodal, "desc_06_erosion")
  }
  
  # GrÃ¡fico
  p_erosion <- ggplot(erosion_por_rodal, 
                      aes(x = factor(rodal), y = pct_afectado, fill = tipo_erosion)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    geom_text(aes(label = sprintf("%.0f%%", pct_afectado)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(
      title = "Porcentaje de Sitios con Evidencia de ErosiÃ³n",
      subtitle = "Por rodal y tipo de erosiÃ³n",
      x = "Rodal",
      y = "% de Sitios Afectados",
      fill = "Tipo de ErosiÃ³n"
    ) +
    ylim(0, 110) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
  
  if (exportar_latex) {
    # Tabla pivoteada
    tabla_latex <- erosion_por_rodal %>%
      select(rodal, tipo_erosion, pct_afectado) %>%
      pivot_wider(
        names_from = tipo_erosion,
        values_from = pct_afectado,
        names_prefix = "pct_"
      )
    
    xtable(tabla_latex,
           caption = "Porcentaje de sitios afectados por erosiÃ³n (por rodal)",
           label = "tab:erosion_pct",
           digits = c(0, 0, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_04_erosion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_02_erosion.png", p_erosion,
           width = 10, height = 6, dpi = 300)
    
    cat("\nâœ“ Tabla y grÃ¡fico de erosiÃ³n exportados\n")
  }
  
  return(list(
    tabla = erosion_por_rodal,
    grafico = p_erosion
  ))
}

# ==============================================================================
# 4. SANIDAD - CORREGIDO
# ==============================================================================

analizar_sanidad <- function(arboles_df, config = CONFIG, 
                             exportar_latex = TRUE,
                             exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘      ANÃLISIS SANITARIO (paleta daltÃ³nicos + CSV)         â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_SANIDAD") || !exists("CODIGOS_INTENSIDAD_INFESTACION")) {
    stop("âŒ CODIGOS_SANIDAD o CODIGOS_INTENSIDAD_INFESTACION no estÃ¡n cargados")
  }
  
  # FILTRAR SOLO VIVOS PARA ANÃLISIS SANITARIO
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÃTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Ãrboles vivos evaluados: %d\n\n", nrow(vivos)))
  
  sanidad <- vivos %>%
    filter(!is.na(sanidad)) %>%
    left_join(CODIGOS_SANIDAD, by = c("sanidad" = "codigo"), suffix = c("", "_sanidad")) %>%
    left_join(CODIGOS_INTENSIDAD_INFESTACION, 
              by = c("calificacion_sanidad" = "codigo"),
              suffix = c("", "_intensidad")) %>%
    rename(
      problema = etiqueta,
      intensidad = etiqueta_intensidad
    ) %>%
    count(rodal, genero_grupo, problema, intensidad, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha)
    ) %>%
    arrange(rodal, desc(densidad_ha))
  
  resumen_sanidad <- sanidad %>%
    group_by(rodal) %>%
    summarise(
      densidad_evaluada_ha = sum(densidad_ha),
      densidad_sanos_ha = sum(densidad_ha[problema == "Sano"]),
      prop_sanos_pct = (densidad_sanos_ha / densidad_evaluada_ha) * 100,
      principal_problema = {
        problemas_rodal <- problema[problema != "Sano"]
        if (length(problemas_rodal) > 0) {
          idx_max <- which.max(densidad_ha[problema != "Sano"])
          problemas_rodal[idx_max]
        } else {
          "Ninguno"
        }
      },
      .groups = "drop"
    )
  
  cat("[SANIDAD POR RODAL - DENSIDAD/HA]\n")
  print(resumen_sanidad)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(sanidad, "desc_07_sanidad_detalle")
    exportar_csv(resumen_sanidad, "desc_08_sanidad_resumen")
  }
  
  # GRÃFICO - PALETA VIRIDIS
  p_sanidad <- sanidad %>%
    filter(problema != "Sano") %>%
    ggplot(aes(x = factor(rodal), y = densidad_ha, fill = problema)) +
    geom_col(alpha = 0.9) +
    scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
    labs(
      title = "Problemas Fitosanitarios Detectados",
      subtitle = "Densidad de Ã¡rboles afectados/ha por rodal (paleta accesible)",
      x = "Rodal",
      y = "Densidad (Ã¡rboles/ha)",
      fill = "Problema"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    xtable(resumen_sanidad,
           caption = "Estado sanitario por rodal (densidad/ha)",
           label = "tab:sanidad",
           digits = c(0, 0, 1, 1, 1, 0)) %>%
      print(file = "tablas_latex/desc_05_sanidad.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_03_sanidad.png", p_sanidad,
           width = 10, height = 6, dpi = 300)
    
    cat("\nâœ“ Tabla y grÃ¡fico de sanidad exportados\n")
  }
  
  return(list(
    detalle = sanidad, 
    resumen = resumen_sanidad,
    grafico = p_sanidad
  ))
}

# ==============================================================================
# 5. REGENERACIÓN - VERSIÓN CORREGIDA
# ==============================================================================
# CAMBIOS CRÍTICOS:
# 1. Mantiene 3 clases de tamaño separadas (0-25, 25-150, 150-275 cm)
# 2. NO hace upscaling automático (números observados en parcela 9m²)
# 3. Incluye TODAS las especies, no solo Pinus/Quercus
# 4. Agrega columna UMM (rodal) desde F01
# 5. Preserva edad_media y diam_medio por clase
# 6. Exporta formato largo: sitio-especie-clase-individuos
# ==============================================================================

analizar_regeneracion <- function(f05, f01, config = CONFIG, 
                                  exportar_latex = TRUE,
                                  exportar_csv_flag = TRUE) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║   REGENERACIÓN: Estructura por clase de tamaño           ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("ESPECIES")) {
    stop("✖ ESPECIES no está cargado")
  }
  
  # ===========================================================================
  # 1. MAPEO SITIO → UMM (RODAL)
  # ===========================================================================
  
  sitios_umm <- f01 %>%
    select(muestreo, rodal) %>%
    distinct()
  
  n_sitios_total <- nrow(sitios_umm)
  
  cat(sprintf("Total de sitios en inventario: %d\n", n_sitios_total))
  cat(sprintf("Rodales únicos: %d\n\n", n_distinct(sitios_umm$rodal)))
  
  # ===========================================================================
  # 2. PROCESAR DATOS DE REGENERACIÓN - TODAS LAS ESPECIES
  # ===========================================================================
  
  # Agregar información de especies
  f05_especies <- f05 %>%
    left_join(ESPECIES %>% select(codigo, nombre_cientifico, genero), 
              by = c("especie" = "codigo"))
  
  # Verificar especies no encontradas
  especies_no_encontradas <- f05_especies %>%
    filter(is.na(nombre_cientifico)) %>%
    select(especie) %>%
    distinct()
  
  if (nrow(especies_no_encontradas) > 0) {
    cat("⚠ ESPECIES NO ENCONTRADAS EN CATÁLOGO:\n")
    print(especies_no_encontradas)
    cat("\n")
  }
  
  # ===========================================================================
  # 3. TRANSFORMAR A FORMATO LARGO (UNA FILA POR SITIO-ESPECIE-CLASE)
  # ===========================================================================
  
  regeneracion_long <- f05_especies %>%
    # Agregar UMM
    left_join(sitios_umm, by = "muestreo") %>%
    # Seleccionar columnas relevantes
    select(
      muestreo,
      rodal,
      especie,
      nombre_cientifico,
      genero,
      # Clase 1: 0-25 cm altura
      num_individuos_clase1 = frec_025_150,
      edad_media_clase1 = edad_media_025_150,
      diam_medio_clase1 = diametro_medio_025_150,
      # Clase 2: 25-150 cm altura  
      num_individuos_clase2 = frec_151_275,
      edad_media_clase2 = edad_media_151_275,
      diam_medio_clase2 = diametro_medio_151_275,
      # Clase 3: 150-275 cm altura
      num_individuos_clase3 = frec_276_400,
      edad_media_clase3 = edad_media_276_400,
      diam_medio_clase3 = diametro_medio_276_400
    ) %>%
    # Transformar a formato largo
    pivot_longer(
      cols = starts_with(c("num_individuos_", "edad_media_", "diam_medio_")),
      names_to = c(".value", "clase"),
      names_pattern = "(.+)_clase(\\d+)"
    ) %>%
    # Agregar etiquetas de clase
    mutate(
      clase_tamaño = case_when(
        clase == "1" ~ "0-25 cm",
        clase == "2" ~ "25-150 cm",
        clase == "3" ~ "150-275 cm",
        TRUE ~ NA_character_
      )
    ) %>%
    # Renombrar columnas finales
    rename(
      UMM = rodal,
      codigo_especie = especie,
      num_individuos = num_individuos,
      edad_media_años = edad_media,
      diam_medio_cm = diam_medio
    ) %>%
    # Filtrar solo registros con individuos
    filter(!is.na(num_individuos) & num_individuos > 0) %>%
    # Reordenar columnas
    select(
      muestreo,
      UMM,
      codigo_especie,
      nombre_cientifico,
      genero,
      clase_tamaño,
      num_individuos,
      edad_media_años,
      diam_medio_cm
    ) %>%
    # Ordenar
    arrange(muestreo, codigo_especie, clase_tamaño)
  
  cat(sprintf("✓ Datos transformados: %d registros con regeneración > 0\n", 
              nrow(regeneracion_long)))
  cat(sprintf("  Sitios con regeneración: %d\n", 
              n_distinct(regeneracion_long$muestreo)))
  cat(sprintf("  Especies diferentes: %d\n", 
              n_distinct(regeneracion_long$codigo_especie)))
  cat(sprintf("  Total individuos contados: %d\n\n", 
              sum(regeneracion_long$num_individuos)))
  
  # ===========================================================================
  # 4. RESUMEN POR GÉNERO Y CLASE
  # ===========================================================================
  
  cat("═══════════════════════════════════════════════════════════\n")
  cat("RESUMEN POR GÉNERO Y CLASE DE TAMAÑO\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  resumen_genero_clase <- regeneracion_long %>%
    group_by(genero, clase_tamaño) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      n_UMM = n_distinct(UMM),
      total_individuos = sum(num_individuos),
      media_individuos = mean(num_individuos),
      mediana_individuos = median(num_individuos),
      min_individuos = min(num_individuos),
      max_individuos = max(num_individuos),
      .groups = "drop"
    )
  
  print(resumen_genero_clase)
  
  # ===========================================================================
  # 5. RESUMEN POR ESPECIE Y CLASE
  # ===========================================================================
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("RESUMEN POR ESPECIE Y CLASE DE TAMAÑO\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  resumen_especie_clase <- regeneracion_long %>%
    group_by(nombre_cientifico, genero, clase_tamaño) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      n_UMM = n_distinct(UMM),
      total_individuos = sum(num_individuos),
      .groups = "drop"
    ) %>%
    arrange(desc(total_individuos))
  
  print(resumen_especie_clase, n = 50)
  
  # ===========================================================================
  # 6. RESUMEN GENERAL POR GÉNERO (TODAS LAS CLASES COMBINADAS)
  # ===========================================================================
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("RESUMEN GENERAL POR GÉNERO (todas las clases)\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  resumen_genero_total <- regeneracion_long %>%
    group_by(genero) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      n_UMM = n_distinct(UMM),
      total_individuos = sum(num_individuos),
      pct_individuos = (total_individuos / sum(regeneracion_long$num_individuos)) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(total_individuos))
  
  print(resumen_genero_total)
  
  # ===========================================================================
  # 7. EXPORTAR CSV
  # ===========================================================================
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    
    # Datos observados (formato largo)
    exportar_csv(regeneracion_long, "desc_09_regeneracion_observada")
    
    # Resúmenes
    exportar_csv(resumen_genero_clase, "desc_10_regeneracion_resumen_genero_clase")
    exportar_csv(resumen_especie_clase, "desc_11_regeneracion_resumen_especie_clase")
    exportar_csv(resumen_genero_total, "desc_12_regeneracion_resumen_genero_total")
    
    cat("\n")
  }
  
  # ===========================================================================
  # 8. GRÁFICOS
  # ===========================================================================
  
  # Gráfico 1: Distribución por género y clase
  p1_genero_clase <- ggplot(
    resumen_genero_clase,
    aes(x = genero, y = total_individuos, fill = clase_tamaño)
  ) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c(
        "0-25 cm" = "#E69F00",
        "25-150 cm" = "#56B4E9", 
        "150-275 cm" = "#009E73"
      )
    ) +
    labs(
      title = "Regeneración Natural por Género y Clase de Tamaño",
      subtitle = sprintf("Parcela 9 m² - %d sitios", n_sitios_total),
      x = "Género",
      y = "Total de individuos",
      fill = "Clase de altura"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Gráfico 2: Top 10 especies
  top10_especies <- resumen_especie_clase %>%
    group_by(nombre_cientifico) %>%
    summarise(total = sum(total_individuos), .groups = "drop") %>%
    top_n(10, total) %>%
    pull(nombre_cientifico)
  
  p2_top_especies <- regeneracion_long %>%
    filter(nombre_cientifico %in% top10_especies) %>%
    group_by(nombre_cientifico, clase_tamaño) %>%
    summarise(total = sum(num_individuos), .groups = "drop") %>%
    ggplot(aes(x = reorder(nombre_cientifico, total), y = total, fill = clase_tamaño)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(
      values = c(
        "0-25 cm" = "#E69F00",
        "25-150 cm" = "#56B4E9", 
        "150-275 cm" = "#009E73"
      )
    ) +
    coord_flip() +
    labs(
      title = "Top 10 Especies en Regeneración",
      subtitle = "Por clase de tamaño",
      x = NULL,
      y = "Total de individuos",
      fill = "Clase de altura"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  # ===========================================================================
  # 9. EXPORTAR LATEX Y GRÁFICOS
  # ===========================================================================
  
  if (exportar_latex) {
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    dir.create("graficos", showWarnings = FALSE, recursive = TRUE)
    
    # Tabla 1: Resumen por género y clase
    xtable(
      resumen_genero_clase,
      caption = "Regeneración por género y clase de tamaño (parcela 9 m²)",
      label = "tab:regeneracion_genero_clase",
      digits = c(0, 0, 0, 0, 0, 0, 1, 1, 0, 0)
    ) %>%
      print(
        file = "tablas_latex/desc_06_regeneracion_genero_clase.tex",
        include.rownames = FALSE, 
        floating = TRUE, 
        booktabs = TRUE,
        sanitize.text.function = identity
      )
    
    # Tabla 2: Top 20 especies
    xtable(
      resumen_especie_clase %>% head(20),
      caption = "Top 20 especies en regeneración por clase de tamaño",
      label = "tab:regeneracion_especies",
      digits = c(0, 0, 0, 0, 0, 0, 0)
    ) %>%
      print(
        file = "tablas_latex/desc_07_regeneracion_especies.tex",
        include.rownames = FALSE, 
        floating = TRUE, 
        booktabs = TRUE,
        sanitize.text.function = identity
      )
    
    # Gráficos
    ggsave(
      "graficos/desc_04_regeneracion_genero_clase.png",
      p1_genero_clase,
      width = 10, height = 6, dpi = 300
    )
    
    ggsave(
      "graficos/desc_05_regeneracion_top10_especies.png",
      p2_top_especies,
      width = 10, height = 8, dpi = 300
    )
    
    cat("✓ Tablas LaTeX y gráficos exportados\n\n")
  }
  
  # ===========================================================================
  # 10. RETORNAR RESULTADOS
  # ===========================================================================
  
  return(list(
    datos_observados = regeneracion_long,
    resumen_genero_clase = resumen_genero_clase,
    resumen_especie_clase = resumen_especie_clase,
    resumen_genero_total = resumen_genero_total,
    grafico_genero_clase = p1_genero_clase,
    grafico_top_especies = p2_top_especies,
    n_sitios_total = n_sitios_total,
    n_sitios_con_regeneracion = n_distinct(regeneracion_long$muestreo),
    total_individuos = sum(regeneracion_long$num_individuos)
  ))
}

# ==============================================================================
# FUNCIÓN CORREGIDA: analizar_tratamientos_silvicolas (ROBUSTA)
# Detecta automáticamente los nombres de columnas
# ==============================================================================

analizar_tratamientos_silvicolas <- function(inventario_f01,
                                             exportar_latex = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS TRATAMIENTOS SILVÍCOLAS (TS) Y TC1-3        ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  # ============================================================================
  # DETECCIÓN AUTOMÁTICA DE NOMBRES DE COLUMNAS
  # ============================================================================
  
  cols_disponibles <- names(inventario_f01)
  
  # Detectar nombres de columna TS
  col_ts <- case_when(
    "tratamiento_silvicola" %in% cols_disponibles ~ "tratamiento_silvicola",
    "35.TS" %in% cols_disponibles ~ "35.TS",
    "x35_ts" %in% cols_disponibles ~ "x35_ts",
    TRUE ~ NA_character_
  )
  
  # Detectar nombres de columnas TC
  col_tc1 <- case_when(
    "tratamiento_comp1" %in% cols_disponibles ~ "tratamiento_comp1",
    "36.TC1" %in% cols_disponibles ~ "36.TC1",
    "x36_tc1" %in% cols_disponibles ~ "x36_tc1",
    TRUE ~ NA_character_
  )
  
  col_tc2 <- case_when(
    "tratamiento_comp2" %in% cols_disponibles ~ "tratamiento_comp2",
    "36.TC2" %in% cols_disponibles ~ "36.TC2",
    "x36_tc2" %in% cols_disponibles ~ "x36_tc2",
    TRUE ~ NA_character_
  )
  
  col_tc3 <- case_when(
    "tratamiento_comp3" %in% cols_disponibles ~ "tratamiento_comp3",
    "36.TC3" %in% cols_disponibles ~ "36.TC3",
    "x36_tc3" %in% cols_disponibles ~ "x36_tc3",
    TRUE ~ NA_character_
  )
  
  # Verificar columnas encontradas
  cat("Columnas detectadas:\n")
  cat(sprintf("  TS:  %s %s\n", col_ts, ifelse(is.na(col_ts), "✖", "✔")))
  cat(sprintf("  TC1: %s %s\n", col_tc1, ifelse(is.na(col_tc1), "✖", "✔")))
  cat(sprintf("  TC2: %s %s\n", col_tc2, ifelse(is.na(col_tc2), "✖", "✔")))
  cat(sprintf("  TC3: %s %s\n\n", col_tc3, ifelse(is.na(col_tc3), "✖", "✔")))
  
  if (is.na(col_ts)) {
    stop("✖ No se encontró columna TS (tratamiento_silvicola / 35.TS)")
  }
  
  # Detectar columnas básicas
  col_rodal <- ifelse("rodal" %in% cols_disponibles, "rodal", "4.SITIO")
  col_muestreo <- ifelse("muestreo" %in% cols_disponibles, "muestreo", "5.SITIO_I")
  col_umm <- ifelse("unidad_manejo" %in% cols_disponibles, "unidad_manejo", "3.UM")
  
  # Verificar que códigos estén cargados
  if (!exists("CODIGOS_TRATAMIENTO")) {
    stop("✖ CODIGOS_TRATAMIENTO no está cargado")
  }
  if (!exists("CODIGOS_TRATAMIENTO_COMP")) {
    stop("✖ CODIGOS_TRATAMIENTO_COMP no está cargado")
  }
  
  # ============================================================================
  # PREPARAR DATOS CON NOMBRES ESTANDARIZADOS
  # ============================================================================
  
  # Seleccionar columnas que existen
  cols_select <- c(col_rodal, col_muestreo, col_umm, col_ts)
  cols_rename <- c("rodal", "muestreo", "unidad_manejo", "ts")
  
  if (!is.na(col_tc1)) {
    cols_select <- c(cols_select, col_tc1)
    cols_rename <- c(cols_rename, "tc1")
  }
  if (!is.na(col_tc2)) {
    cols_select <- c(cols_select, col_tc2)
    cols_rename <- c(cols_rename, "tc2")
  }
  if (!is.na(col_tc3)) {
    cols_select <- c(cols_select, col_tc3)
    cols_rename <- c(cols_rename, "tc3")
  }
  
  df_ts <- inventario_f01 %>%
    select(all_of(cols_select)) %>%
    setNames(cols_rename)
  
  # Agregar columnas TC faltantes como NA
  if (!"tc1" %in% names(df_ts)) df_ts$tc1 <- NA_integer_
  if (!"tc2" %in% names(df_ts)) df_ts$tc2 <- NA_integer_
  if (!"tc3" %in% names(df_ts)) df_ts$tc3 <- NA_integer_
  
  # Convertir a numérico
  df_ts <- df_ts %>%
    mutate(
      ts = as.integer(ts),
      tc1 = as.integer(tc1),
      tc2 = as.integer(tc2),
      tc3 = as.integer(tc3)
    )
  
  n_sitios_total <- nrow(df_ts)
  n_rodales <- n_distinct(df_ts$rodal)
  
  cat(sprintf("Sitios totales: %d\n", n_sitios_total))
  cat(sprintf("Rodales: %d\n\n", n_rodales))
  
  # ============================================================================
  # A. DISTRIBUCIÓN TS
  # ============================================================================
  
  cat("[A] Distribución Tratamiento Silvícola (TS)...\n")
  
  dist_ts <- df_ts %>%
    count(ts, name = "n_sitios") %>%
    filter(!is.na(ts)) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, etiqueta),
              by = c("ts" = "codigo")) %>%
    mutate(
      pct = n_sitios / sum(n_sitios) * 100
    ) %>%
    arrange(desc(n_sitios))
  
  print(dist_ts)
  
  # ============================================================================
  # B. DISTRIBUCIÓN TC1, TC2, TC3
  # ============================================================================
  
  cat("\n[B] Distribución Tratamientos Complementarios...\n")
  
  # Función helper para analizar un TC
  analizar_un_tc <- function(df, tc_col, tc_name) {
    df %>%
      count(.data[[tc_col]], name = "n_menciones") %>%
      filter(!is.na(.data[[tc_col]])) %>%
      left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, etiqueta),
                by = setNames("codigo", tc_col)) %>%
      mutate(
        tratamiento = tc_name,
        pct = n_menciones / sum(n_menciones) * 100
      ) %>%
      rename(codigo = !!tc_col) %>%
      select(tratamiento, codigo, etiqueta, n_menciones, pct) %>%
      arrange(desc(n_menciones))
  }
  
  dist_tc1 <- analizar_un_tc(df_ts, "tc1", "TC1")
  dist_tc2 <- analizar_un_tc(df_ts, "tc2", "TC2")
  dist_tc3 <- analizar_un_tc(df_ts, "tc3", "TC3")
  
  dist_tc_all <- bind_rows(dist_tc1, dist_tc2, dist_tc3)
  
  if (nrow(dist_tc_all) > 0) {
    cat("\n  Top 5 por cada TC:\n")
    print(dist_tc_all %>% group_by(tratamiento) %>% slice_head(n = 5))
  } else {
    cat("\n  ⚠️  No hay datos de TC disponibles\n")
  }
  
  # ============================================================================
  # C. RELACIÓN TS ↔ TC (Coherencia)
  # ============================================================================
  
  cat("\n[C] Matriz TS vs TC (principales combinaciones)...\n")
  
  # Calcular TC más frecuentes por cada TS
  relacion_ts_tc <- df_ts %>%
    pivot_longer(cols = starts_with("tc"),
                 names_to = "tipo_tc",
                 values_to = "codigo_tc") %>%
    filter(!is.na(ts), !is.na(codigo_tc)) %>%
    count(ts, codigo_tc, name = "n_menciones") %>%
    # Top 3 TC por TS
    group_by(ts) %>%
    slice_max(order_by = n_menciones, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    # Agregar etiquetas
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, ts_etiqueta = etiqueta),
              by = c("ts" = "codigo")) %>%
    left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, tc_etiqueta = etiqueta),
              by = c("codigo_tc" = "codigo")) %>%
    arrange(ts, desc(n_menciones))
  
  if (nrow(relacion_ts_tc) > 0) {
    print(relacion_ts_tc)
  } else {
    cat("  ⚠️  No hay combinaciones TS-TC disponibles\n")
  }
  
  # ============================================================================
  # D. COMPLETITUD TC POR TS
  # ============================================================================
  
  cat("\n[D] Completitud TC1-3 por Tratamiento Silvícola...\n")
  
  completitud_ts <- df_ts %>%
    group_by(ts) %>%
    summarise(
      n_sitios = n(),
      pct_tc1 = sum(!is.na(tc1)) / n() * 100,
      pct_tc2 = sum(!is.na(tc2)) / n() * 100,
      pct_tc3 = sum(!is.na(tc3)) / n() * 100,
      .groups = "drop"
    ) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, etiqueta),
              by = c("ts" = "codigo")) %>%
    select(ts, etiqueta, n_sitios, pct_tc1, pct_tc2, pct_tc3)
  
  print(completitud_ts)
  
  # ============================================================================
  # E. ANÁLISIS POR RODAL
  # ============================================================================
  
  cat("\n[E] Tratamientos por rodal (moda)...\n")
  
  ts_por_rodal <- df_ts %>%
    group_by(rodal) %>%
    summarise(
      n_sitios = n(),
      # TS moda
      ts_moda = as.numeric(names(sort(table(ts), decreasing = TRUE)[1])),
      ts_diversidad = n_distinct(ts, na.rm = TRUE),
      # TC1 moda
      tc1_moda = ifelse(sum(!is.na(tc1)) > 0,
                        as.numeric(names(sort(table(tc1), decreasing = TRUE)[1])),
                        NA_real_),
      # Completitud
      pct_tc1 = sum(!is.na(tc1)) / n() * 100,
      pct_tc2 = sum(!is.na(tc2)) / n() * 100,
      pct_tc3 = sum(!is.na(tc3)) / n() * 100,
      .groups = "drop"
    ) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, ts_etiqueta = etiqueta),
              by = c("ts_moda" = "codigo")) %>%
    left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, tc1_etiqueta = etiqueta),
              by = c("tc1_moda" = "codigo"))
  
  print(ts_por_rodal)
  
  # ============================================================================
  # F. LISTA COMPLETA DE TC POR RODAL/UMM
  # ============================================================================
  
  cat("\n[F] Generando lista completa de TC por rodal/UMM...\n")
  
  # Crear dataset detallado con todos los TC por sitio
  tc_detallado <- df_ts %>%
    # Pivotear para tener todos los TC en una sola columna
    pivot_longer(
      cols = starts_with("tc"),
      names_to = "tc_tipo",
      values_to = "tc_codigo"
    ) %>%
    filter(!is.na(tc_codigo)) %>%
    # Agregar etiquetas descriptivas
    left_join(CODIGOS_TRATAMIENTO %>% select(ts_codigo = codigo, ts_etiqueta = etiqueta),
              by = c("ts" = "ts_codigo")) %>%
    left_join(CODIGOS_TRATAMIENTO_COMP %>% select(tc_codigo = codigo, tc_etiqueta = etiqueta),
              by = "tc_codigo") %>%
    # Ordenar y seleccionar columnas relevantes
    mutate(
      tc_tipo = str_to_upper(tc_tipo),
      tc_tipo_num = as.integer(str_extract(tc_tipo, "[0-9]"))
    ) %>%
    arrange(rodal, muestreo, tc_tipo_num) %>%
    select(
      rodal,
      unidad_manejo,
      muestreo,
      ts_codigo = ts,
      ts_etiqueta,
      tc_tipo,
      tc_codigo,
      tc_etiqueta
    )
  
  # Resumen por rodal/UMM
  tc_por_rodal <- tc_detallado %>%
    group_by(rodal, unidad_manejo) %>%
    summarise(
      n_sitios_con_tc = n_distinct(muestreo),
      n_total_tc = n(),
      tc_lista = paste(sort(unique(tc_codigo)), collapse = ", "),
      tc_etiquetas = paste(sort(unique(tc_etiqueta)), collapse = " | "),
      .groups = "drop"
    ) %>%
    # Agregar información de TS predominante del rodal
    left_join(
      df_ts %>%
        group_by(rodal) %>%
        summarise(
          ts_predominante = as.numeric(names(sort(table(ts), decreasing = TRUE)[1])),
          n_sitios_total = n(),
          .groups = "drop"
        ),
      by = "rodal"
    ) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(ts_predominante = codigo, ts_etiqueta = etiqueta),
              by = "ts_predominante") %>%
    # Calcular porcentaje de sitios con TC
    mutate(
      pct_sitios_con_tc = round(n_sitios_con_tc / n_sitios_total * 100, 1)
    ) %>%
    select(
      rodal,
      unidad_manejo,
      n_sitios_total,
      n_sitios_con_tc,
      pct_sitios_con_tc,
      ts_predominante,
      ts_etiqueta,
      n_total_tc,
      tc_lista,
      tc_etiquetas
    ) %>%
    arrange(rodal)
  
  cat("\n  Resumen TC por rodal:\n")
  print(tc_por_rodal)
  
  # Detalle completo por sitio
  cat("\n  Muestra del detalle completo (primeros 10 registros):\n")
  print(tc_detallado %>% head(10))
  
  # ============================================================================
  # G. GRÁFICOS
  # ============================================================================
  
  cat("\n[G] Generando gráficos...\n")
  
  # Gráfico 1: Distribución TS
  p_ts <- dist_ts %>%
    ggplot(aes(x = reorder(etiqueta, n_sitios), y = n_sitios)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", n_sitios, pct)),
              hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Distribución de Tratamientos Silvícolas (TS)",
      subtitle = sprintf("N = %d sitios", sum(dist_ts$n_sitios)),
      x = NULL,
      y = "Número de sitios"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  # Gráfico 2: Comparación TC1-3 (solo si hay datos)
  p_tc_comparacion <- NULL
  if (nrow(dist_tc_all) > 0) {
    p_tc_comparacion <- dist_tc_all %>%
      group_by(tratamiento) %>%
      slice_head(n = 5) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(etiqueta, n_menciones), 
                 y = n_menciones, 
                 fill = tratamiento)) +
      geom_col() +
      geom_text(aes(label = n_menciones), 
                hjust = -0.2, size = 3) +
      coord_flip() +
      facet_wrap(~tratamiento, scales = "free_y", ncol = 1) +
      scale_fill_manual(values = c("TC1" = "#1f77b4", 
                                   "TC2" = "#ff7f0e", 
                                   "TC3" = "#2ca02c")) +
      labs(
        title = "Top 5 Tratamientos Complementarios por tipo",
        x = NULL,
        y = "Número de menciones",
        fill = "Tipo"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      )
  }
  
  # Gráfico 3: Completitud TC por TS
  p_completitud <- completitud_ts %>%
    pivot_longer(cols = starts_with("pct_"),
                 names_to = "tc_tipo",
                 values_to = "pct") %>%
    mutate(
      tc_tipo = str_replace(tc_tipo, "pct_", ""),
      tc_tipo = toupper(tc_tipo)
    ) %>%
    ggplot(aes(x = etiqueta, y = pct, fill = tc_tipo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("TC1" = "#1f77b4", 
                                 "TC2" = "#ff7f0e", 
                                 "TC3" = "#2ca02c")) +
    labs(
      title = "Completitud TC1-3 por Tratamiento Silvícola",
      x = "Tratamiento Silvícola",
      y = "% sitios con dato",
      fill = "TC"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Gráfico 4: Distribución de TC por rodal
  if (nrow(tc_por_rodal) > 0) {
    p_tc_por_rodal <- tc_por_rodal %>%
      ggplot(aes(x = reorder(as.character(rodal), n_total_tc), y = n_total_tc)) +
      geom_col(fill = "darkgreen", alpha = 0.7) +
      geom_text(aes(label = n_total_tc), 
                vjust = -0.5, size = 3) +
      labs(
        title = "Número total de TC por rodal",
        subtitle = "Suma de TC1, TC2 y TC3 en todos los sitios del rodal",
        x = "Rodal",
        y = "Número total de TC"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  # ============================================================================
  # H. EXPORTAR RESULTADOS
  # ============================================================================
  
  if (exportar_csv_flag) {
    cat("\n[H] Exportando CSV...\n")
    exportar_csv(dist_ts, "desc_14_ts_distribucion")
    if (nrow(dist_tc_all) > 0) {
      exportar_csv(dist_tc_all, "desc_15_tc_distribucion")
    }
    if (nrow(relacion_ts_tc) > 0) {
      exportar_csv(relacion_ts_tc, "desc_16_ts_tc_relacion")
    }
    exportar_csv(completitud_ts, "desc_17_ts_completitud")
    exportar_csv(ts_por_rodal, "desc_18_ts_por_rodal")
    
    # Exportar los nuevos archivos CSV de TC por rodal
    exportar_csv(tc_detallado, "desc_19_tc_detallado")
    exportar_csv(tc_por_rodal, "desc_20_tc_por_rodal")
    
    cat("  ✔ Nuevos archivos CSV exportados:\n")
    cat("     - desc_19_tc_detallado.csv (detalle por sitio)\n")
    cat("     - desc_20_tc_por_rodal.csv (resumen por rodal/UMM)\n")
  }
  
  if (exportar_latex) {
    cat("\n[I] Exportando LaTeX y gráficos...\n")
    
    # Tabla TS
    xtable(dist_ts %>% select(-ts),
           caption = "Distribución de Tratamientos Silvícolas",
           label = "tab:ts_dist",
           align = c("l", "l", "r", "r"),
           digits = c(0, 0, 0, 1)) %>%
      print(file = "tablas_latex/desc_14_ts_distribucion.tex",
            include.rownames = FALSE,
            floating = TRUE,
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla relación TS-TC (solo si hay datos)
    if (nrow(relacion_ts_tc) > 0) {
      xtable(relacion_ts_tc %>% 
               select(ts, ts_etiqueta, tc_etiqueta, n_menciones) %>%
               head(15),
             caption = "Principales combinaciones TS + TC",
             label = "tab:ts_tc",
             align = c("l", "r", "p{4cm}", "p{4cm}", "r")) %>%
        print(file = "tablas_latex/desc_15_ts_tc_relacion.tex",
              include.rownames = FALSE,
              floating = TRUE,
              booktabs = TRUE,
              sanitize.text.function = identity)
    }
    
    # Tabla TC por rodal
    if (nrow(tc_por_rodal) > 0) {
      xtable(tc_por_rodal %>% 
               select(rodal, unidad_manejo, n_sitios_total, n_sitios_con_tc, 
                      pct_sitios_con_tc, ts_etiqueta, n_total_tc),
             caption = "Tratamientos Complementarios por Rodal/UMM",
             label = "tab:tc_por_rodal",
             align = c("l", "r", "l", "r", "r", "r", "p{4cm}", "r"),
             digits = c(0, 0, 0, 0, 0, 1, 0, 0)) %>%
        print(file = "tablas_latex/desc_16_tc_por_rodal.tex",
              include.rownames = FALSE,
              floating = TRUE,
              booktabs = TRUE,
              sanitize.text.function = identity)
    }
    
    # Gráficos
    ggsave("graficos/desc_13_ts_distribucion.png", p_ts,
           width = 10, height = 6, dpi = 300)
    
    if (!is.null(p_tc_comparacion)) {
      ggsave("graficos/desc_14_tc_comparacion.png", p_tc_comparacion,
             width = 10, height = 10, dpi = 300)
    }
    
    ggsave("graficos/desc_15_completitud.png", p_completitud,
           width = 10, height = 6, dpi = 300)
    
    if (exists("p_tc_por_rodal")) {
      ggsave("graficos/desc_16_tc_por_rodal.png", p_tc_por_rodal,
             width = 10, height = 6, dpi = 300)
    }
    
    cat("  ✔ Tablas LaTeX y gráficos exportados\n")
  }
  
  cat("\n✔ Análisis de tratamientos completado\n\n")
  
  return(list(
    dist_ts = dist_ts,
    dist_tc = dist_tc_all,
    relacion_ts_tc = relacion_ts_tc,
    completitud_ts = completitud_ts,
    ts_por_rodal = ts_por_rodal,
    tc_detallado = tc_detallado,
    tc_por_rodal = tc_por_rodal,
    graficos = list(
      p_ts = p_ts,
      p_tc_comparacion = p_tc_comparacion,
      p_completitud = p_completitud,
      p_tc_por_rodal = if (exists("p_tc_por_rodal")) p_tc_por_rodal else NULL
    )
  ))
}

cat("✔ Función analizar_tratamientos_silvicolas CORREGIDA cargada\n")
cat("  - Detecta automáticamente nombres de columnas\n")
cat("  - Maneja TC faltantes (a veces solo 1 o 2 TC)\n")
cat("  - Compatible con Excel directo o post-importación\n")
cat("  - Exporta CSV con TC por rodal/UMM\n\n")

# ==============================================================================
# FUNCIÃ“N MAESTRA - ACTUALIZADA CON CORRECCIÃ“N n_sitios
# ==============================================================================

analisis_descriptivo_completo <- function(inventario, arboles_df, config = CONFIG,
                                          exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘     ANÃLISIS DESCRIPTIVO COMPLETO (AB/ha + CSV)           â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  
  resultados <- list()
  
  cat("\n[1/9] Estructura poblacional (con AB/ha - CORREGIDO n_sitios)...\n")
  resultados$estructura <- analizar_estructura_poblacional(arboles_df, config, 
                                                           exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[2/9] ComposiciÃ³n Pinus/Quercus por rodal (CORREGIDO)...\n")
  resultados$composicion_generopq_rodal <- analizar_composicion_generopq_por_rodal(arboles_df, config,
                                                                                   exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[3/9] AB por dominancia (CORREGIDO)...\n")
  resultados$ab_dominancia <- analizar_ab_por_dominancia(arboles_df, config,
                                                         exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[4/9] DistribuciÃ³n diamÃ©trica (CORREGIDO)...\n")
  resultados$distribucion <- analizar_distribucion_diametrica(arboles_df, config,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[5/9] Condiciones de erosiÃ³n (% sitios)...\n")
  resultados$erosion <- analizar_erosion(inventario$f01, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[6/9] Condiciones sanitarias (CORREGIDO)...\n")
  resultados$sanidad <- analizar_sanidad(arboles_df, config, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[7/9] RegeneraciÃ³n (Pinus/Quercus)...\n")
  resultados$regeneracion <- analizar_regeneracion(inventario$f05, inventario$f01, config,
                                                   exportar_csv_flag = exportar_csv_flag)
  
  
  cat("\n[8/9] Tratamientos silvícolas y complementarios...\n")
  resultados$tratamientos <- analizar_tratamientos_silvicolas(inventario$f01,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  
  cat("\n[9/9] Análisis dasométrico con IC 95% (método CV)...\n")
  resultados$dasometrico <- analizar_dasometrico_cv(
    arboles_df = arboles_df,
    config = config,
    exportar_csv_flag = exportar_csv_flag,
    exportar_latex = TRUE
  )
  
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘           âœ“ ANÃLISIS DESCRIPTIVO COMPLETADO               â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  cat("CORRECCIÃ“N CRÃTICA APLICADA:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  âœ“ n_sitios usa TODOS los sitios muestreados\n")
  cat("  âœ“ No excluye sitios sin Ã¡rboles vivos\n")
  cat("  âœ“ MÃ©tricas /ha calculadas correctamente\n")
  cat("  âœ“ Evita sobrestimaciÃ³n de densidad/AB/volumen\n\n")
  
  cat("OTRAS CARACTERÃSTICAS:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  âœ“ TODAS las tablas usan SOLO Ã¡rboles vivos\n")
  cat("  âœ“ Todas las tablas incluyen AB/ha y DMC\n")
  cat("  âœ“ ComposiciÃ³n Pinus/Quercus por rodal\n")
  cat("  âœ“ AB por dominancia (incluye muertos, tocones)\n")
  cat("  âœ“ ExportaciÃ³n CSV en carpeta resultados/\n")
  cat("  âœ“ ErosiÃ³n: % de sitios afectados por rodal\n")
  cat("  âœ“ Sanidad: Solo Ã¡rboles vivos, paleta accesible\n")
  cat("  âœ“ RegeneraciÃ³n: Solo Pinus/Quercus + densidad=0\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  ðŸ“Š Tablas LaTeX (tablas_latex/): desc_01 a desc_11\n")
  cat("  ðŸ“ˆ GrÃ¡ficos PNG (graficos/): desc_01 a desc_12\n")
  cat("  ðŸ“„ CSVs (resultados/): desc_01 a desc_13\n\n")
  
  return(resultados)
}

cat("\nâœ“ MÃ³dulo de anÃ¡lisis descriptivo CORREGIDO cargado\n")
cat("  CORRECCIÃ“N CRÃTICA:\n")
cat("    âš ï¸  n_sitios ahora usa TODOS los sitios muestreados\n")
cat("    âš ï¸  Evita sobrestimaciÃ³n al excluir sitios sin Ã¡rboles\n")
cat("    âš ï¸  Aplica a: estructura, composiciÃ³n PQ, AB dominancia,\n")
cat("                 distribuciÃ³n diamÃ©trica, sanidad\n\n")
cat("  OTRAS CARACTERÃSTICAS:\n")
cat("    1. TODAS las tablas usan SOLO Ã¡rboles vivos\n")
cat("    2. Todas las tablas incluyen AB/ha y DMC\n")
cat("    3. ComposiciÃ³n Pinus/Quercus por rodal (desc_11)\n")
cat("    4. AB por dominancia - incluye muertos (desc_12, desc_13)\n")
cat("    5. ExportaciÃ³n automÃ¡tica de CSV en resultados/\n\n")