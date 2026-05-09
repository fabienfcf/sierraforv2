# ==============================================================================
# GRÁFICOS PARA PMF - NIVEL PREDIO
# ==============================================================================
#
# Este módulo genera todos los gráficos significativos a nivel predio:
#   1. Frecuencia de especies (%)
#   2. Frecuencia vs. Área basal/ha (3 gráficos: Pinus, Quercus, Total)
#   3. Frecuencia vs. Volumen/ha (3 gráficos: Pinus, Quercus, Total)
#   4. Distribución por diámetro (3 gráficos: Pinus, Quercus, Total)
#   5. Distribución por altura (3 gráficos: Pinus, Quercus, Total)
#
# ==============================================================================

library(tidyverse)
library(patchwork)

# Colores consistentes para Pinus y Quercus
COLORES_GENERO <- c(
  "Pinus" = "#2E7D32",    # Verde oscuro
  "Quercus" = "#8D6E63",  # Marrón
  "Total" = "#1976D2"     # Azul
)

# ==============================================================================
# 1. FRECUENCIA DE ESPECIES (%)
# ==============================================================================

grafico_frecuencia_especies <- function(arboles_df) {
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  freq_especies <- vivos %>%
    count(nombre_cientifico, genero_grupo) %>%
    mutate(
      pct = (n / sum(n)) * 100,
      especie_label = str_replace(nombre_cientifico, " ", "\n")
    ) %>%
    arrange(desc(pct))
  
  p <- ggplot(freq_especies, aes(x = reorder(especie_label, pct), 
                                 y = pct, 
                                 fill = genero_grupo)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              hjust = -0.1, size = 3) +
    scale_fill_manual(values = COLORES_GENERO, name = "Género") +
    coord_flip() +
    labs(
      title = "Frecuencia de Especies",
      subtitle = sprintf("n = %d árboles", sum(freq_especies$n)),
      x = NULL,
      y = "Frecuencia (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)
}

# ==============================================================================
# 2. FRECUENCIA VS. ÁREA BASAL/HA (CLASES DE 5 EN 5)
# ==============================================================================

grafico_frecuencia_ab <- function(arboles_df, genero_filtro = NULL, config = CONFIG) {

  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # Filtrar por género si especificado
  if (!is.null(genero_filtro)) {
    vivos <- vivos %>% filter(genero_grupo == genero_filtro)
    titulo <- sprintf("Frecuencia vs. Área Basal - %s", genero_filtro)
  } else {
    titulo <- "Frecuencia vs. Área Basal - Total"
  }
  
  # Calcular AB/ha por rodal
  ab_por_rodal <- vivos %>%
    group_by(rodal) %>%
    summarise(
      ab_muestreada = sum(area_basal, na.rm = TRUE),
      n_arboles = n(),
      n_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      ab_ha = expandir_a_hectarea(ab_muestreada, area_total_ha),
      # Categorizar en clases de 5
      clase_ab = cut(ab_ha, 
                     breaks = seq(0, ceiling(max(ab_ha)/5)*5, by = 5),
                     include.lowest = TRUE,
                     labels = FALSE) * 5 - 2.5  # Punto medio de la clase
    )
  
  # Calcular frecuencia por clase
  freq_ab <- ab_por_rodal %>%
    count(clase_ab) %>%
    mutate(pct = (n / sum(n)) * 100)
  
  color_usar <- if (!is.null(genero_filtro)) COLORES_GENERO[genero_filtro] else COLORES_GENERO["Total"]
  
  p <- ggplot(freq_ab, aes(x = clase_ab, y = pct)) +
    geom_col(fill = color_usar, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              vjust = -0.5, size = 3) +
    labs(
      title = titulo,
      subtitle = sprintf("n = %d rodales", nrow(ab_por_rodal)),
      x = "Área Basal (m²/ha) - Clases de 5",
      y = "Frecuencia (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)
}

# ==============================================================================
# 3. FRECUENCIA VS. VOLUMEN/HA (CLASES DE 10 EN 10)
# ==============================================================================

grafico_frecuencia_volumen <- function(arboles_df, genero_filtro = NULL, config = CONFIG) {

  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # Filtrar por género si especificado
  if (!is.null(genero_filtro)) {
    vivos <- vivos %>% filter(genero_grupo == genero_filtro)
    titulo <- sprintf("Frecuencia vs. Volumen - %s", genero_filtro)
  } else {
    titulo <- "Frecuencia vs. Volumen - Total"
  }
  
  # Calcular Vol/ha por rodal
  vol_por_rodal <- vivos %>%
    group_by(rodal) %>%
    summarise(
      vol_muestreado = sum(volumen_m3, na.rm = TRUE),
      n_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      vol_ha = expandir_a_hectarea(vol_muestreado, area_total_ha),
      # Categorizar en clases de 10
      clase_vol = cut(vol_ha, 
                      breaks = seq(0, ceiling(max(vol_ha)/10)*10, by = 10),
                      include.lowest = TRUE,
                      labels = FALSE) * 10 - 5  # Punto medio
    )
  
  # Calcular frecuencia por clase
  freq_vol <- vol_por_rodal %>%
    count(clase_vol) %>%
    mutate(pct = (n / sum(n)) * 100)
  
  color_usar <- if (!is.null(genero_filtro)) COLORES_GENERO[genero_filtro] else COLORES_GENERO["Total"]
  
  p <- ggplot(freq_vol, aes(x = clase_vol, y = pct)) +
    geom_col(fill = color_usar, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              vjust = -0.5, size = 3) +
    labs(
      title = titulo,
      subtitle = sprintf("n = %d rodales", nrow(vol_por_rodal)),
      x = "Volumen (m³/ha) - Clases de 10",
      y = "Frecuencia (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)
}

# ==============================================================================
# 4. DISTRIBUCIÓN POR DIÁMETRO (CLASES DE 5 EN 5)
# ==============================================================================

grafico_distribucion_diametro <- function(arboles_df, genero_filtro = NULL, config = CONFIG) {

  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # Filtrar por género si especificado
  if (!is.null(genero_filtro)) {
    vivos <- vivos %>% filter(genero_grupo == genero_filtro)
    titulo <- sprintf("Distribución Diamétrica - %s", genero_filtro)
  } else {
    titulo <- "Distribución Diamétrica - Total"
  }
  
  # Asignar clases diamétricas
  vivos <- vivos %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal,
      breaks = config$clases_d,
      formato = "rango"
    ))
  
  # Calcular distribución
  dist_d <- vivos %>%
    count(clase_d) %>%
    mutate(pct = (n / sum(n)) * 100)
  
  color_usar <- if (!is.null(genero_filtro)) COLORES_GENERO[genero_filtro] else COLORES_GENERO["Total"]
  
  p <- ggplot(dist_d, aes(x = clase_d, y = pct)) +
    geom_col(fill = color_usar, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              angle = 90, hjust = -0.2, size = 2.5) +
    labs(
      title = titulo,
      subtitle = sprintf("n = %d árboles", sum(dist_d$n)),
      x = "Clase Diamétrica (cm)",
      y = "Distribución (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)
}

# ==============================================================================
# 5. DISTRIBUCIÓN POR ALTURA (CLASES DE 5 EN 5)
# ==============================================================================

grafico_distribucion_altura <- function(arboles_df, genero_filtro = NULL, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(!is.na(altura_total))
  
  # Filtrar por género si especificado
  if (!is.null(genero_filtro)) {
    vivos <- vivos %>% filter(genero_grupo == genero_filtro)
    titulo <- sprintf("Distribución de Alturas - %s", genero_filtro)
  } else {
    titulo <- "Distribución de Alturas - Total"
  }
  
  # Categorizar alturas en clases de 5 m
  vivos <- vivos %>%
    mutate(
      clase_h = cut(altura_total,
                    breaks = seq(0, ceiling(max(altura_total)/5)*5, by = 5),
                    include.lowest = TRUE,
                    labels = paste0(seq(0, ceiling(max(altura_total)/5)*5 - 5, by = 5), 
                                    "-",
                                    seq(5, ceiling(max(altura_total)/5)*5, by = 5), " m"))
    )
  
  # Calcular distribución
  dist_h <- vivos %>%
    count(clase_h) %>%
    mutate(pct = (n / sum(n)) * 100)
  
  color_usar <- if (!is.null(genero_filtro)) COLORES_GENERO[genero_filtro] else COLORES_GENERO["Total"]
  
  p <- ggplot(dist_h, aes(x = clase_h, y = pct)) +
    geom_col(fill = color_usar, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              vjust = -0.5, size = 3) +
    labs(
      title = titulo,
      subtitle = sprintf("n = %d árboles", sum(dist_h$n)),
      x = "Clase de Altura (m)",
      y = "Distribución (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  return(p)
}

# ==============================================================================
# GENERAR Y EXPORTAR TODOS LOS GRÁFICOS
# ==============================================================================

generar_todos_graficos_pmf <- function(arboles_df, config = CONFIG, 
                                       directorio = "graficos") {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          GENERANDO GRÁFICOS PMF                           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # 1. Frecuencia de especies
  cat("[1/13] Frecuencia de especies...\n")
  g1 <- grafico_frecuencia_especies(arboles_df)
  ggsave(file.path(directorio, "33_01_frecuencia_especies.png"),
         g1, width = 8, height = 6, dpi = 300)
  
  # 2-4. Frecuencia vs. Área basal (Pinus, Quercus, Total)
  cat("[2/13] Frecuencia vs. AB - Pinus...\n")
  g2 <- grafico_frecuencia_ab(arboles_df, "Pinus", config)
  ggsave(file.path(directorio, "33_02_frecuencia_ab_pinus.png"),
         g2, width = 8, height = 5, dpi = 300)
  
  cat("[3/13] Frecuencia vs. AB - Quercus...\n")
  g3 <- grafico_frecuencia_ab(arboles_df, "Quercus", config)
  ggsave(file.path(directorio, "33_03_frecuencia_ab_quercus.png"),
         g3, width = 8, height = 5, dpi = 300)
  
  cat("[4/13] Frecuencia vs. AB - Total...\n")
  g4 <- grafico_frecuencia_ab(arboles_df, NULL, config)
  ggsave(file.path(directorio, "33_04_frecuencia_ab_total.png"),
         g4, width = 8, height = 5, dpi = 300)
  
  # 5-7. Frecuencia vs. Volumen (Pinus, Quercus, Total)
  cat("[5/13] Frecuencia vs. Vol - Pinus...\n")
  g5 <- grafico_frecuencia_volumen(arboles_df, "Pinus", config)
  ggsave(file.path(directorio, "33_05_frecuencia_vol_pinus.png"),
         g5, width = 8, height = 5, dpi = 300)
  
  cat("[6/13] Frecuencia vs. Vol - Quercus...\n")
  g6 <- grafico_frecuencia_volumen(arboles_df, "Quercus", config)
  ggsave(file.path(directorio, "33_06_frecuencia_vol_quercus.png"),
         g6, width = 8, height = 5, dpi = 300)
  
  cat("[7/13] Frecuencia vs. Vol - Total...\n")
  g7 <- grafico_frecuencia_volumen(arboles_df, NULL, config)
  ggsave(file.path(directorio, "33_07_frecuencia_vol_total.png"),
         g7, width = 8, height = 5, dpi = 300)
  
  # 8-10. Distribución por diámetro (Pinus, Quercus, Total)
  cat("[8/13] Distribución diámetro - Pinus...\n")
  g8 <- grafico_distribucion_diametro(arboles_df, "Pinus", config)
  ggsave(file.path(directorio, "33_08_dist_diametro_pinus.png"),
         g8, width = 10, height = 5, dpi = 300)
  
  cat("[9/13] Distribución diámetro - Quercus...\n")
  g9 <- grafico_distribucion_diametro(arboles_df, "Quercus", config)
  ggsave(file.path(directorio, "33_09_dist_diametro_quercus.png"),
         g9, width = 10, height = 5, dpi = 300)
  
  cat("[10/13] Distribución diámetro - Total...\n")
  g10 <- grafico_distribucion_diametro(arboles_df, NULL, config)
  ggsave(file.path(directorio, "33_10_dist_diametro_total.png"),
         g10, width = 10, height = 5, dpi = 300)
  
  # 11-13. Distribución por altura (Pinus, Quercus, Total)
  cat("[11/13] Distribución altura - Pinus...\n")
  g11 <- grafico_distribucion_altura(arboles_df, "Pinus", config)
  ggsave(file.path(directorio, "33_11_dist_altura_pinus.png"),
         g11, width = 8, height = 5, dpi = 300)
  
  cat("[12/13] Distribución altura - Quercus...\n")
  g12 <- grafico_distribucion_altura(arboles_df, "Quercus", config)
  ggsave(file.path(directorio, "33_12_dist_altura_quercus.png"),
         g12, width = 8, height = 5, dpi = 300)
  
  cat("[13/13] Distribución altura - Total...\n")
  g13 <- grafico_distribucion_altura(arboles_df, NULL, config)
  ggsave(file.path(directorio, "33_13_dist_altura_total.png"),
         g13, width = 8, height = 5, dpi = 300)
  
  cat("\n✓ Todos los gráficos generados exitosamente\n\n")
  
  return(list(
    g1_especies = g1,
    g2_ab_pinus = g2, g3_ab_quercus = g3, g4_ab_total = g4,
    g5_vol_pinus = g5, g6_vol_quercus = g6, g7_vol_total = g7,
    g8_d_pinus = g8, g9_d_quercus = g9, g10_d_total = g10,
    g11_h_pinus = g11, g12_h_quercus = g12, g13_h_total = g13
  ))
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de gráficos PMF cargado\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Funciones disponibles:\n")
cat("  • grafico_frecuencia_especies(arboles_df)\n")
cat("  • grafico_frecuencia_ab(arboles_df, genero_filtro, config)\n")
cat("  • grafico_frecuencia_volumen(arboles_df, genero_filtro, config)\n")
cat("  • grafico_distribucion_diametro(arboles_df, genero_filtro, config)\n")
cat("  • grafico_distribucion_altura(arboles_df, genero_filtro, config)\n")
cat("  • generar_todos_graficos_pmf(arboles_df, config)\n")
cat("══════════════════════════════════════════════════════════════\n\n")