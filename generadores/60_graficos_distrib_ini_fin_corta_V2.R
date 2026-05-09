# ==============================================================================
# HISTOGRAMMES DE DENSITÉ PAR CLASSE DIAMÉTRIQUE
# Initial vs Año Corta (con Liocourt) vs Final - Pinus & Quercus par UMM
# ==============================================================================

library(tidyverse)
library(patchwork)
library(ggpattern)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Charger CONFIG pour area_parcela_ha
source("config/01_parametros_configuracion.R")
source("core/15_core_calculos.R")

# Créer répertoires si nécessaire
if (!dir.exists("graficos")) dir.create("graficos", recursive = TRUE)

# ==============================================================================
# CHARGER DONNÉES
# ==============================================================================

cat("\n[1/5] Chargement des données...\n")

historial <- readRDS("resultados/historial_completo_10anos.rds")
cortas <- readRDS("resultados/registro_cortas.rds")

cat(sprintf("  ✓ Historial: %d lignes\n", nrow(historial)))
cat(sprintf("  ✓ Cortas: %d arbres\n", nrow(cortas)))

# ==============================================================================
# FONCTIONS AUXILIAIRES
# ==============================================================================

#' Assigner classe diamétrique (5 cm)
asignar_clase_diametrica <- function(d) {
  breaks <- seq(5, 95, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  cut(d, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
}

#' Extraire centro de clase desde label "25-30" → 27.5
extraer_centro_clase <- function(clase_label) {
  if (is.na(clase_label)) return(NA)
  partes <- as.numeric(strsplit(as.character(clase_label), "-")[[1]])
  mean(partes)
}

#' Calculer densité par hectare depuis données individuelles
calcular_densidad_ha <- function(df, area_parcela_ha) {
  
  # Filtrer arbres vivants uniquement
  df_vivos <- filtrar_arboles_vivos(df)
  
  # Assigner classe diamétrique
  df_vivos <- df_vivos %>%
    mutate(clase_d = asignar_clase_diametrica(diametro_normal))
  
  # Calculer nombre d'arbres par UMM, genre et classe diamétrique
  densidad <- df_vivos %>%
    group_by(rodal, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      .groups = "drop"
    )
  
  # Ajouter info de surface muestreada
  info_muestreo <- df_vivos %>%
    group_by(rodal) %>%
    summarise(
      num_muestreos = first(num_muestreos_realizados),  # ✓ Usar valor del inventario
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = num_muestreos * area_parcela_ha
    )
  
  # Joindre et calculer densité par ha
  densidad <- densidad %>%
    left_join(info_muestreo, by = "rodal") %>%
    mutate(
      n_arboles_por_ha = n_arboles / area_muestreada_ha
    )
  
  return(densidad)
}

#' Calculer courbe Liocourt
calcular_curva_liocourt <- function(rodal_id, q_factor = Q_FACTOR, clase_ref = CLASE_REFERENCIA_LIOCOURT) {
  
  # Obtener N_REF para este UMM
  n_ref <- obtener_n_ref_liocourt(rodal_id)
  
  # Generar clases diamétricas
  breaks <- seq(5, 95, by = 5)
  centros <- breaks[-length(breaks)] + 2.5
  
  # Calcular Liocourt: N(d) = N_ref × q^((d_ref - d) / 5)
  liocourt <- tibble(
    clase_centro = centros,
    clase_d = paste0(breaks[-length(breaks)], "-", breaks[-1])
  ) %>%
    mutate(
      idx = (clase_ref - clase_centro) / 5,
      n_liocourt = n_ref * q_factor^idx
    )
  
  return(liocourt)
}

# ==============================================================================
# PRÉPARER DONNÉES PAR ÉTAT
# ==============================================================================

cat("\n[2/5] Préparation des données...\n")

# État INITIAL (année 0)
inicial <- historial %>%
  filter(ano_simulacion == 0) %>%
  calcular_densidad_ha(CONFIG$area_parcela_ha)

cat(sprintf("  ✓ Inicial: %d UMM × genres × classes\n", 
            n_distinct(inicial$rodal)))

# État FINAL (année 10)
final <- historial %>%
  filter(ano_simulacion == 10) %>%
  calcular_densidad_ha(CONFIG$area_parcela_ha)

cat(sprintf("  ✓ Final: %d UMM × genres × classes\n", 
            n_distinct(final$rodal)))

# ==============================================================================
# PRÉPARER DONNÉES AÑO DE CORTA (RESIDUAL + CORTADO)
# ==============================================================================

cat("\n[3/5] Préparation données année de coupe...\n")

# Identifier l'année de coupe par UMM
anos_corta <- PROGRAMA_CORTAS %>%
  select(rodal, ano_corta)

# Pour chaque UMM, obtenir état JUSTE APRÈS la coupe
residual_por_umm <- list()
cortado_por_umm <- list()

for (i in 1:nrow(anos_corta)) {
  rodal_id <- anos_corta$rodal[i]
  ano <- anos_corta$ano_corta[i]
  
  # État résiduel (après coupe)
  residual <- historial %>%
    filter(rodal == rodal_id, ano_simulacion == ano) %>%
    calcular_densidad_ha(CONFIG$area_parcela_ha) %>%
    mutate(
      tipo = "Residual",
      ano_ref = ano
    )
  
  residual_por_umm[[i]] <- residual
  
  # Arbres coupés cette année
  cortado <- cortas %>%
    filter(rodal_cortado == rodal_id, ano_corta == ano) %>%
    filter(es_arbol_vivo(dominancia)) %>%
    mutate(
      rodal = rodal_cortado,
      clase_d = asignar_clase_diametrica(diametro_normal)
    )
  
  # ✓ DIAGNÓSTICO: Verificar géneros antes de agrupar
  if (nrow(cortado) > 0) {
    generos_raw <- unique(cortado$genero_grupo)
    cat(sprintf("    UMM %d - Géneros en cortas RAW: %s\n", 
                rodal_id, paste(generos_raw, collapse = ", ")))
  }
  
  cortado <- cortado %>%
    group_by(rodal, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      .groups = "drop"
    )
  
  # Calculer densité coupée
  num_muestreos <- cortas %>%
    filter(rodal_cortado == rodal_id) %>%
    pull(num_muestreos_realizados) %>%
    first()
  
  area_muestreada <- num_muestreos * CONFIG$area_parcela_ha
  
  cortado <- cortado %>%
    mutate(
      n_arboles_por_ha = n_arboles / area_muestreada,
      tipo = "Cortado",
      ano_ref = ano
    )
  
  cortado_por_umm[[i]] <- cortado
}

# Combiner
residual_total <- bind_rows(residual_por_umm)
cortado_total <- bind_rows(cortado_por_umm)

cat(sprintf("  ✓ Residual: %d UMM après coupe\n", 
            n_distinct(residual_total$rodal)))
cat(sprintf("  ✓ Cortado: %d UMM avec coupes\n", 
            n_distinct(cortado_total$rodal)))

# ==============================================================================
# FONCTION CRÉATION HISTOGRAMME
# ==============================================================================

#' Créer histogramme pour état simple (Initial ou Final)
crear_histograma_simple <- function(datos, titulo, rodal_id = NULL) {
  
  # Filtrer par UMM si spécifié
  if (!is.null(rodal_id)) {
    datos <- datos %>% filter(rodal == rodal_id)
  }
  
  if (nrow(datos) == 0) return(NULL)
  
  # Assurer factor avec tous les niveaux
  breaks <- seq(5, 95, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  datos <- datos %>%
    mutate(
      clase_d = factor(clase_d, levels = labels),
      genero_grupo = factor(genero_grupo, levels = c("Pinus", "Quercus"))  # ← FORZAR ORDEN
    )
  
  # Créer graphique
  p <- ggplot(datos, aes(x = clase_d, y = n_arboles_por_ha, fill = genero_grupo)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
    scale_fill_manual(
      values = c("Pinus" = "#2E7D32", "Quercus" = "#D84315"),
      name = "Género"
    ) +
    labs(
      title = titulo,
      x = "Clase diamétrico (cm)",
      y = "Densidad (árboles/ha)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Créer histogramme pour année de coupe (Residual + Cortado + Liocourt)
crear_histograma_corta <- function(residual, cortado, rodal_id) {
  
  # Filtrer par UMM
  residual_umm <- residual %>% filter(rodal == rodal_id)
  cortado_umm <- cortado %>% filter(rodal == rodal_id)
  
  if (nrow(residual_umm) == 0) return(NULL)
  
  ano_corta_umm <- first(residual_umm$ano_ref)
  
  # Assurer factor avec tous les niveaux
  breaks <- seq(5, 95, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  residual_umm <- residual_umm %>%
    mutate(
      clase_d = factor(clase_d, levels = labels),
      genero_grupo = factor(genero_grupo, levels = c("Pinus", "Quercus"))  # ← FORZAR ORDEN
    )
  
  cortado_umm <- cortado_umm %>%
    mutate(
      clase_d = factor(clase_d, levels = labels),
      genero_grupo = factor(genero_grupo, levels = c("Pinus", "Quercus"))  # ← FORZAR ORDEN
    )
  
  # Calculer courbe Liocourt
  liocourt <- calcular_curva_liocourt(rodal_id) %>%
    mutate(clase_d = factor(clase_d, levels = labels))
  
  # ═══════════════════════════════════════════════════════════════════════
  # APILAMIENTO: RESIDUAL Y CORTADO SEPARADOS
  # ═══════════════════════════════════════════════════════════════════════
  
  # RESIDUAL: Pinus primero (abajo), Quercus encima
  residual_apilado <- residual_umm %>%
    arrange(clase_d, genero_grupo) %>%  # factor con levels = c("Pinus", "Quercus")
    group_by(clase_d) %>%
    mutate(
      ymin = cumsum(n_arboles_por_ha) - n_arboles_por_ha,
      ymax = cumsum(n_arboles_por_ha),
      x_num = as.numeric(clase_d),
      xmin = x_num - 0.45,
      xmax = x_num + 0.45
    ) %>%
    ungroup()
  
  # Calcular altura máxima del residual por clase
  altura_residual <- residual_apilado %>%
    group_by(clase_d) %>%
    summarise(altura_max = max(ymax, na.rm = TRUE), .groups = "drop")
  
  # CORTADO: Apilar ENCIMA del residual
  cortado_apilado <- cortado_umm %>%
    arrange(clase_d, genero_grupo) %>%  # factor con levels = c("Pinus", "Quercus")
    left_join(altura_residual, by = "clase_d") %>%
    mutate(altura_max = if_else(is.na(altura_max), 0, altura_max)) %>%
    group_by(clase_d) %>%
    mutate(
      ymin = altura_max + (cumsum(n_arboles_por_ha) - n_arboles_por_ha),
      ymax = altura_max + cumsum(n_arboles_por_ha),
      x_num = as.numeric(clase_d),
      xmin = x_num - 0.45,
      xmax = x_num + 0.45
    ) %>%
    ungroup()
  
  # ═══════════════════════════════════════════════════════════════════════
  # DIAGNÓSTICO
  # ═══════════════════════════════════════════════════════════════════════
  
  cat(sprintf("\n    UMM %d:\n", rodal_id))
  cat(sprintf("      Residual: %d filas\n", nrow(residual_apilado)))
  cat(sprintf("      Cortado:  %d filas\n", nrow(cortado_apilado)))
  
  # Ejemplo clase 25-30
  if (nrow(residual_apilado) > 0) {
    cat("\n    RESIDUAL clase 25-30:\n")
    residual_apilado %>%
      filter(clase_d == "25-30") %>%
      select(genero_grupo, n_arboles_por_ha, ymin, ymax) %>%
      print()
  }
  
  if (nrow(cortado_apilado) > 0) {
    cat("\n    CORTADO clase 25-30:\n")
    cortado_apilado %>%
      filter(clase_d == "25-30") %>%
      select(genero_grupo, n_arboles_por_ha, altura_max, ymin, ymax) %>%
      print()
  }
  
  # ═══════════════════════════════════════════════════════════════════════
  # DIBUJAR
  # ═══════════════════════════════════════════════════════════════════════
  
  p <- ggplot()
  
  # 1. RESIDUAL (barras sólidas)
  if (nrow(residual_apilado) > 0) {
    p <- p +
      geom_rect(
        data = residual_apilado,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = genero_grupo),
        alpha = 0.9,
        color = "black",
        linewidth = 0.2
      )
  }
  
  # 2. CORTADO (barras rayadas)
  if (nrow(cortado_apilado) > 0) {
    
    cortado_pinus <- cortado_apilado %>% filter(genero_grupo == "Pinus")
    cortado_quercus <- cortado_apilado %>% filter(genero_grupo == "Quercus")
    
    if (nrow(cortado_pinus) > 0) {
      p <- p +
        ggpattern::geom_rect_pattern(
          data = cortado_pinus,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#2E7D32",
          pattern = "stripe",
          pattern_fill = "black",
          pattern_color = "black",
          pattern_density = 0.15,
          pattern_spacing = 0.02,
          pattern_angle = 45,
          pattern_alpha = 0.7,
          alpha = 0.9,
          color = "black",
          linewidth = 0.2,
          show.legend = FALSE
        )
    }
    
    if (nrow(cortado_quercus) > 0) {
      p <- p +
        ggpattern::geom_rect_pattern(
          data = cortado_quercus,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#D84315",
          pattern = "stripe",
          pattern_fill = "black",
          pattern_color = "black",
          pattern_density = 0.15,
          pattern_spacing = 0.02,
          pattern_angle = 45,
          pattern_alpha = 0.7,
          alpha = 0.9,
          color = "black",
          linewidth = 0.2,
          show.legend = FALSE
        )
    }
  }
  
  # 3. LÍNEA LIOCOURT
  p <- p +
    geom_line(
      data = liocourt,
      aes(x = clase_d, y = n_liocourt, group = 1),
      color = "#0066CC",
      linewidth = 1.5,
      linetype = "dashed"
    ) +
    geom_point(
      data = liocourt,
      aes(x = clase_d, y = n_liocourt),
      color = "#0066CC",
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.5
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = sprintf("Liocourt: Q=%.2f", Q_FACTOR),
      hjust = 1.05, vjust = 1.5,
      size = 3.5,
      color = "#0066CC",
      fontface = "italic"
    ) +
    scale_fill_manual(
      values = c("Pinus" = "#2E7D32", "Quercus" = "#D84315"),
      name = "Género"
    ) +
    scale_x_discrete(
      drop = FALSE,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = sprintf("UMM %d - Año Corta (Año %d)", rodal_id, INICIO_SIMULACION+ano_corta_umm-1),
      subtitle = "Sólido: Residual | Rayado: Cortado | Azul: Liocourt",
      x = "Clase diamétrico (cm)",
      y = "Densidad (árboles/ha)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 8.5, color = "gray30"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ==============================================================================
# GRAPHIQUES PAR UMM INDIVIDUEL (3 PANNEAUX)
# ==============================================================================

cat("\n[4/5] Génération des graphiques par UMM...\n")

# Liste des UMM
rodales <- sort(unique(inicial$rodal))

for (rodal_id in rodales) {
  
  cat(sprintf("  Traitement UMM %d...\n", rodal_id))
  
  # ═══════════════════════════════════════════════════════════════════════
  # PASO 0: CALCULAR ESCALA COMÚN PARA LOS 3 GRÁFICOS
  # ═══════════════════════════════════════════════════════════════════════
  
  # Obtener datos para este UMM
  inicial_umm <- inicial %>% filter(rodal == rodal_id)
  residual_umm <- residual_total %>% filter(rodal == rodal_id)
  cortado_umm <- cortado_total %>% filter(rodal == rodal_id)
  final_umm <- final %>% filter(rodal == rodal_id)
  
  # # Calcular densidad máxima por clase en cada estado
  # # Para año corta: sumar residual + cortado
  max_inicial <- inicial_umm %>%
    group_by(clase_d) %>%
    summarise(total = sum(n_arboles_por_ha, na.rm = TRUE), .groups = "drop") %>%
    pull(total) %>%
    max(na.rm = TRUE)

  max_corta <- bind_rows(
    residual_umm,
    cortado_umm
  ) %>%
    group_by(clase_d) %>%
    summarise(total = sum(n_arboles_por_ha, na.rm = TRUE), .groups = "drop") %>%
    pull(total) %>%
    max(na.rm = TRUE)

  max_final <- final_umm %>%
    group_by(clase_d) %>%
    summarise(total = sum(n_arboles_por_ha, na.rm = TRUE), .groups = "drop") %>%
    pull(total) %>%
    max(na.rm = TRUE)
  # 
  # # Incluir Liocourt en el cálculo
  # liocourt_umm <- calcular_curva_liocourt(rodal_id)
  # max_liocourt <- max(liocourt_umm$n_liocourt, na.rm = TRUE)
  
  # Escala común: máximo de todos + 20% margen
  y_max_comun <- max(c(max_inicial, max_corta, max_final), na.rm = TRUE) * 1.01
  
  # Clases comunes
  breaks <- seq(5, 70, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  # ═══════════════════════════════════════════════════════════════════════
  # GRAPHIQUES CON ESCALA COMÚN
  # ═══════════════════════════════════════════════════════════════════════
  
  # GRAPHIQUE 1: Initial (Année 0)
  p1 <- crear_histograma_simple(
    inicial, 
    sprintf("UMM %d - Inicial - 2026", rodal_id), 
    rodal_id
  )
  if (!is.null(p1)) {
    p1 <- p1 +
      scale_y_continuous(limits = c(0, y_max_comun), expand = expansion(mult = c(0, 0))) +
      scale_x_discrete(drop = FALSE, limits = labels)
  }
  
  # GRAPHIQUE 2: Año Corta (Residual + Cortado + Liocourt)
  p2 <- crear_histograma_corta(
    residual_total,
    cortado_total,
    rodal_id
  )
  if (!is.null(p2)) {
    p2 <- p2 +
      scale_y_continuous(limits = c(0, y_max_comun), expand = expansion(mult = c(0, 0))) +
      scale_x_discrete(drop = FALSE, limits = labels)
  }
  
  # GRAPHIQUE 3: Final (Année 10)
  p3 <- crear_histograma_simple(
    final, 
    sprintf("UMM %d - Final (2036)", rodal_id), 
    rodal_id
  )
  if (!is.null(p3)) {
    p3 <- p3 +
      scale_y_continuous(limits = c(0, y_max_comun), expand = expansion(mult = c(0, 0))) +
      scale_x_discrete(drop = FALSE, limits = labels)
  }
  
  # Filtrer graphiques valides
  graficos_validos <- list(p1, p2, p3)
  graficos_validos <- graficos_validos[!sapply(graficos_validos, is.null)]
  
  if (length(graficos_validos) >= 2) {
    
    # Assembler
    if (length(graficos_validos) == 3) {
      p_umm <- p1 / p2 / p3
    } else if (length(graficos_validos) == 2) {
      p_umm <- graficos_validos[[1]] / graficos_validos[[2]]
    } else {
      p_umm <- graficos_validos[[1]]
    }
    
    # Ajouter titre général
    p_umm <- p_umm + 
      plot_annotation(
        title = sprintf("Evolución de la densidad forestal - UMM %d", rodal_id),
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50")
        )
      )
    
    # Sauvegarder
    ggsave(
      sprintf("graficos/densidad_diametrica_UMM_%02d.png", rodal_id),
      p_umm,
      width = 12,
      height = 16,
      dpi = 300
    )
    
    cat(sprintf("    ✓ UMM_%02d.png (y_max = %.0f árb/ha)\n", rodal_id, y_max_comun))
  }
}



# ==============================================================================
# RÉSUMÉ FINAL
# ==============================================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║       ✓ GRAPHIQUES DE DENSITÉ GÉNÉRÉS                   ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("FICHIERS GÉNÉRÉS:\n")
cat("  📊 graficos/densidad_diametrica_GLOBAL.png\n")
for (rodal_id in rodales) {
  cat(sprintf("  📊 graficos/densidad_diametrica_UMM_%02d.png\n", rodal_id))
}

cat("\nFORMAT:\n")
cat("  • Graphique 1: Año 0 (Initial)\n")
cat("  • Graphique 2: Año Corta (Residual + Cortado + Liocourt)\n")
cat("  • Graphique 3: Año 10\n")

cat("\nLÉGENDE GRAPHIQUE 2:\n")
cat("  ■ Barres sólidas:        Residual (après coupe)\n")
cat("  ▨ Barres rayées (negro): Cortado (mismo color, rayado fino 45°)\n")
cat("  ─ ─ Ligne pointillée:    Courbe Liocourt idéale\n")

# Copiar a LATEX/anexos/
latex_anexos <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/anexos"
if (dir.exists(latex_anexos)) {
  archivos_png <- c(
    "graficos/densidad_diametrica_GLOBAL.png",
    sprintf("graficos/densidad_diametrica_UMM_%02d.png", rodales)
  )
  for (f in archivos_png) {
    if (file.exists(f)) file.copy(f, file.path(latex_anexos, basename(f)), overwrite = TRUE)
  }
  cat(sprintf("\n✓ Gráficos copiados a LATEX/anexos/ (%d archivos)\n", length(archivos_png)))
}

cat("\n")