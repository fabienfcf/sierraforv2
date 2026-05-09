# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 08_ANALISIS_DESCRIPTIVO.R - VERSION CON CSV Y AB/HA COMPLETO
# MODIFICACIONES:
# 1. Todas las tablas dasométricas incluyen AB/ha
# 2. Exportación CSV de todas las tablas en carpeta resultados/
# 3. Mantiene correcciones previas (erosión, sanidad, regeneración)
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# Verificar que CONFIG esté cargado
if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta primero: source(file.path(PROYECTO_ROOT, 'config/01_parametros_configuracion.R'))")
}

# Verificar funciones de expansión
if (!exists("expandir_a_hectarea")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# ==============================================================================
# VERIFICAR CONFIG
# ==============================================================================

if (is.null(CONFIG$area_parcela_regeneracion_ha)) {
  warning("⚠️ CONFIG$area_parcela_regeneracion_ha no existe, usando 0.0009 ha (9 m²)")
  CONFIG$area_parcela_regeneracion_ha <- 0.0009
}

cat(sprintf("\n[CONFIGURACIÓN DE EXPANSIÓN]\n"))
cat(sprintf("  Arbolado: %.2f ha (factor = %.1f)\n", 
            CONFIG$area_parcela_ha, 
            calcular_factor_expansion(CONFIG$area_parcela_ha)))
cat(sprintf("  Regeneración: %.4f ha (9 m², factor = %.1f)\n\n", 
            CONFIG$area_parcela_regeneracion_ha,
            calcular_factor_expansion(CONFIG$area_parcela_regeneracion_ha)))

# ==============================================================================
# FUNCIÓN AUXILIAR: EXPORTAR CSV
# ==============================================================================

exportar_csv <- function(df, nombre_archivo, carpeta = "resultados") {
  # Crear carpeta si no existe
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  # Ruta completa
  ruta <- file.path(carpeta, paste0(nombre_archivo, ".csv"))
  
  # Exportar
  write_csv(df, ruta)
  
  cat(sprintf("  ✓ CSV: %s\n", ruta))
}

# ==============================================================================
# 1. ESTRUCTURA POBLACIONAL - CON AB/HA COMPLETO + CSV
# ==============================================================================

analizar_estructura_poblacional <- function(arboles_df, config = CONFIG, 
                                            exportar_latex = TRUE,
                                            exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS ESTRUCTURA POBLACIONAL (AB/ha + CSV)         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios <- n_distinct(vivos$muestreo)
  
  # RESUMEN GENERAL - CON AB/HA
  resumen_general <- vivos %>%
    summarise(
      n_arboles_total = n(),
      n_sitios = n_sitios,
      n_rodales = n_distinct(rodal),
      n_especies = n_distinct(nombre_cientifico),
      n_generos = n_distinct(genero_grupo),
      densidad_ha = expandir_a_hectarea(n_arboles_total / n_sitios, config$area_parcela_ha),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
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
  
  cat("[RESUMEN GENERAL - VALORES POR HECTÁREA]\n")
  print(resumen_general)
  
  # POR RODAL - CON AB/HA
  por_rodal <- vivos %>%
    group_by(rodal) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      n_arboles_obs = n(),
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      n_especies = n_distinct(nombre_cientifico),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),
      ab_m2ha = expandir_a_hectarea(
        sum(area_basal, na.rm = TRUE) / n_sitios,
        config$area_parcela_ha
      ),
      vol_m3ha = expandir_a_hectarea(
        sum(volumen_m3, na.rm = TRUE) / n_sitios,
        config$area_parcela_ha
      ),
      .groups = "drop"
    ) %>%
    arrange(rodal)
  
  cat("\n[POR RODAL - VALORES POR HECTÁREA]\n")
  print(por_rodal, n = 50)
  
  # POR GÉNERO - CON AB/HA
  por_genero <- vivos %>%
    count(genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / sum(n_arboles_obs)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(genero_grupo) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          h_media_m = mean(altura_total, na.rm = TRUE),
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
  
  cat("\n[POR GÉNERO - VALORES POR HECTÁREA]\n")
  print(por_genero)
  
  # POR ESPECIE - CON AB/HA (TOP 10)
  por_especie <- vivos %>%
    count(nombre_cientifico, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / nrow(vivos)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(nombre_cientifico) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
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
  
  cat("\n[TOP 10 ESPECIES - DENSIDAD POR HECTÁREA]\n")
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
    
    # Tabla por rodal - AHORA CON AB/HA
    xtable(por_rodal %>% select(-n_sitios, -n_arboles_obs), 
           caption = "Resumen dasométrico por rodal (valores/ha, incluye AB)",
           label = "tab:resumen_rodal",
           digits = c(0, 0, 1, 0, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_01_resumen_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla por género - AHORA CON AB/HA Y %
    xtable(por_genero %>% select(genero_grupo, densidad_ha, proporcion_pct, 
                                 d_medio_cm, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "Composición por género (valores/ha, incluye AB y porcentajes)",
           label = "tab:composicion_genero",
           digits = c(0, 0, 1, 2, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_02_composicion_genero.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla especies - AHORA CON AB/HA
    xtable(por_especie %>% select(-n_arboles_obs),
           caption = "Principales especies (top 10, incluye AB/ha)",
           label = "tab:top_especies",
           digits = c(0, 0, 0, 1, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_03_top_especies.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\n✓ Tablas LaTeX exportadas (desc_01 a desc_03)\n")
  }
  
  return(list(
    general = resumen_general,
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# 2. DISTRIBUCIÓN DIAMÉTRICA + CSV
# ==============================================================================

analizar_distribucion_diametrica <- function(arboles_df, config = CONFIG, 
                                             exportar = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         ANÁLISIS DISTRIBUCIÓN DIAMÉTRICA + CSV            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios <- n_distinct(vivos$muestreo)
  
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
  
  cat("[DISTRIBUCIÓN DIAMÉTRICA - DENSIDAD/HA]\n")
  print(dist_diametrica, n = 30)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(dist_diametrica, "desc_05_distribucion_diametrica")
  }
  
  # GRÁFICOS
  p1 <- ggplot(dist_diametrica, 
               aes(x = clase_d, y = densidad_ha, fill = genero_grupo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "Distribución Diamétrica por Género",
      subtitle = sprintf("Densidad/ha - Parcela: %.2f ha", config$area_parcela_ha),
      x = "Clase Diamétrica (cm)",
      y = "Densidad (árboles/ha)",
      fill = "Género"
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
      title = "Proporción Acumulada por Clase Diamétrica",
      x = "Clase Diamétrica (cm)",
      y = "Proporción Acumulada (%)",
      color = "Género"
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
    cat("\n✓ Gráfico guardado: graficos/desc_01_distribucion_diametrica.png\n")
  }
  
  return(list(
    tabla = dist_diametrica, 
    grafico_barras = p1,
    grafico_acumulado = p2,
    grafico_combinado = p_combined
  ))
}

# ==============================================================================
# 3. EROSIÓN - % SITIOS AFECTADOS + CSV
# ==============================================================================

analizar_erosion <- function(f01, exportar_latex = TRUE, exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      ANÁLISIS DE EROSIÓN (% sitios afectados + CSV)       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("CODIGOS_EROSION")) {
    stop("❌ CODIGOS_EROSION no está cargado")
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
        "erosion_carcavas" = "Cárcavas",
        "erosion_antropica" = "Antrópica"
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
  
  cat("[EROSIÓN: % DE SITIOS AFECTADOS POR RODAL]\n")
  print(erosion_por_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(erosion_por_rodal, "desc_06_erosion")
  }
  
  # Gráfico
  p_erosion <- ggplot(erosion_por_rodal, 
                      aes(x = factor(rodal), y = pct_afectado, fill = tipo_erosion)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    geom_text(aes(label = sprintf("%.0f%%", pct_afectado)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(
      title = "Porcentaje de Sitios con Evidencia de Erosión",
      subtitle = "Por rodal y tipo de erosión",
      x = "Rodal",
      y = "% de Sitios Afectados",
      fill = "Tipo de Erosión"
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
           caption = "Porcentaje de sitios afectados por erosión (por rodal)",
           label = "tab:erosion_pct",
           digits = c(0, 0, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_04_erosion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_02_erosion.png", p_erosion,
           width = 10, height = 6, dpi = 300)
    
    cat("\n✓ Tabla y gráfico de erosión exportados\n")
  }
  
  return(list(
    tabla = erosion_por_rodal,
    grafico = p_erosion
  ))
}

# ==============================================================================
# 4. SANIDAD - PALETA DALTÓNICOS + CSV
# ==============================================================================

analizar_sanidad <- function(arboles_df, config = CONFIG, 
                             exportar_latex = TRUE,
                             exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      ANÁLISIS SANITARIO (paleta daltónicos + CSV)         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("CODIGOS_SANIDAD") || !exists("CODIGOS_INTENSIDAD_INFESTACION")) {
    stop("❌ CODIGOS_SANIDAD o CODIGOS_INTENSIDAD_INFESTACION no están cargados")
  }
  
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  sanidad <- arboles_df %>%
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
  
  # GRÁFICO - PALETA VIRIDIS
  p_sanidad <- sanidad %>%
    filter(problema != "Sano") %>%
    ggplot(aes(x = factor(rodal), y = densidad_ha, fill = problema)) +
    geom_col(alpha = 0.9) +
    scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
    labs(
      title = "Problemas Fitosanitarios Detectados",
      subtitle = "Densidad de árboles afectados/ha por rodal (paleta accesible)",
      x = "Rodal",
      y = "Densidad (árboles/ha)",
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
    
    cat("\n✓ Tabla y gráfico de sanidad exportados\n")
  }
  
  return(list(
    detalle = sanidad, 
    resumen = resumen_sanidad,
    grafico = p_sanidad
  ))
}

# ==============================================================================
# 5. REGENERACIÓN - PINUS/QUERCUS + DENSIDAD=0 + CSV
# ==============================================================================

analizar_regeneracion <- function(f05, f01, config = CONFIG, 
                                  exportar_latex = TRUE,
                                  exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   REGENERACIÓN: Pinus/Quercus + densidad=0 + CSV          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("ESPECIES")) {
    stop("❌ ESPECIES no está cargado")
  }
  
  # 1. TODOS LOS SITIOS
  todos_sitios <- f01 %>%
    select(muestreo, rodal) %>%
    distinct()
  
  n_sitios_total <- nrow(todos_sitios)
  
  cat(sprintf("Total de sitios en inventario: %d\n", n_sitios_total))
  
  # 2. REGENERACIÓN OBSERVADA (solo Pinus/Quercus)
  regeneracion_obs <- f05 %>%
    left_join(ESPECIES %>% select(codigo, nombre_cientifico, genero), 
              by = c("especie" = "codigo")) %>%
    filter(genero %in% c("Pinus", "Quercus")) %>%
    mutate(
      total_individuos = frec_025_150 + frec_151_275 + frec_276_400,
      densidad_ha = expandir_a_hectarea(total_individuos, config$area_parcela_regeneracion_ha)
    ) %>%
    filter(!is.na(genero), !is.na(densidad_ha), is.finite(densidad_ha))
  
  cat(sprintf("Sitios CON regeneración observada (Pinus/Quercus): %d\n", 
              n_distinct(regeneracion_obs$muestreo)))
  
  # 3. COMPLETAR CON SITIOS SIN REGENERACIÓN (densidad = 0)
  regeneracion_completa <- expand_grid(
    muestreo = todos_sitios$muestreo,
    genero = c("Pinus", "Quercus")
  ) %>%
    left_join(todos_sitios, by = "muestreo") %>%
    left_join(
      regeneracion_obs %>% 
        group_by(muestreo, genero) %>%
        summarise(densidad_ha = sum(densidad_ha), .groups = "drop"),
      by = c("muestreo", "genero")
    ) %>%
    mutate(densidad_ha = replace_na(densidad_ha, 0))
  
  cat(sprintf("\nSitios con regeneración = 0: %d\n", 
              sum(regeneracion_completa$densidad_ha == 0)))
  cat(sprintf("Sitios con regeneración > 0: %d\n\n", 
              sum(regeneracion_completa$densidad_ha > 0)))
  
  # 4. RESUMEN POR GÉNERO
  resumen_regeneracion <- regeneracion_completa %>%
    group_by(genero) %>%
    summarise(
      n_sitios_total = n(),
      n_sitios_con_regen = sum(densidad_ha > 0),
      pct_presencia = (n_sitios_con_regen / n_sitios_total) * 100,
      densidad_media_ha = mean(densidad_ha),
      densidad_mediana_ha = median(densidad_ha),
      densidad_min_ha = min(densidad_ha),
      densidad_max_ha = max(densidad_ha),
      .groups = "drop"
    )
  
  cat("[REGENERACIÓN - Solo Pinus/Quercus - Parcela 9 m²]\n")
  print(resumen_regeneracion)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(regeneracion_completa, "desc_09_regeneracion_completa")
    exportar_csv(resumen_regeneracion, "desc_10_regeneracion_resumen")
  }
  
  # 5. GRÁFICO - PALETA COLORBLIND-FRIENDLY
  p_regen <- ggplot(regeneracion_completa, 
                    aes(x = genero, y = densidad_ha, fill = genero)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    scale_fill_manual(
      values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")
    ) +
    labs(
      title = "Densidad de Regeneración Natural (Pinus y Quercus)",
      subtitle = sprintf("Parcela 9 m² (0.0009 ha) - Incluye sitios sin regeneración (n=%d)", 
                         n_sitios_total),
      x = "Género",
      y = "Densidad (individuos/ha)",
      fill = "Género"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    xtable(resumen_regeneracion,
           caption = "Regeneración de Pinus y Quercus (parcela 9 m², incluye densidad=0)",
           label = "tab:regeneracion",
           digits = c(0, 0, 0, 0, 1, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_06_regeneracion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_04_regeneracion.png", p_regen,
           width = 8, height = 6, dpi = 300)
    
    cat("\n✓ Tabla y gráfico de regeneración exportados\n")
  }
  
  return(list(
    datos_completos = regeneracion_completa,
    resumen = resumen_regeneracion,
    grafico = p_regen
  ))
}

# ==============================================================================
# FUNCIÓN MAESTRA - ACTUALIZADA CON CSV
# ==============================================================================

analisis_descriptivo_completo <- function(inventario, arboles_df, config = CONFIG,
                                          exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS DESCRIPTIVO COMPLETO (AB/ha + CSV)           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  resultados <- list()
  
  cat("\n[1/5] Estructura poblacional (con AB/ha)...\n")
  resultados$estructura <- analizar_estructura_poblacional(arboles_df, config, 
                                                           exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[2/5] Distribución diamétrica...\n")
  resultados$distribucion <- analizar_distribucion_diametrica(arboles_df, config,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[3/5] Condiciones de erosión (% sitios)...\n")
  resultados$erosion <- analizar_erosion(inventario$f01, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[4/5] Condiciones sanitarias...\n")
  resultados$sanidad <- analizar_sanidad(arboles_df, config, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[5/5] Regeneración (Pinus/Quercus)...\n")
  resultados$regeneracion <- analizar_regeneracion(inventario$f05, inventario$f01, config,
                                                   exportar_csv_flag = exportar_csv_flag)
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           ✓ ANÁLISIS DESCRIPTIVO COMPLETADO               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("MODIFICACIONES APLICADAS:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  ✓ Todas las tablas dasométricas incluyen AB/ha\n")
  cat("  ✓ Exportación CSV en carpeta resultados/\n")
  cat("  ✓ Erosión: % de sitios afectados por rodal\n")
  cat("  ✓ Sanidad: Paleta viridis (accesible)\n")
  cat("  ✓ Regeneración: Solo Pinus/Quercus + densidad=0\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  📊 Tablas LaTeX (tablas_latex/): desc_01 a desc_06\n")
  cat("  📈 Gráficos PNG (graficos/): desc_01 a desc_04\n")
  cat("  📄 CSVs (resultados/): desc_01 a desc_10\n\n")
  
  return(resultados)
}

cat("\n✓ Módulo de análisis descriptivo ACTUALIZADO cargado\n")
cat("  Modificaciones:\n")
cat("    1. Todas las tablas dasométricas incluyen AB/ha\n")
cat("    2. Exportación automática de CSV en resultados/\n")
cat("    3. Mantiene correcciones previas (erosión, sanidad, regeneración)\n\n")