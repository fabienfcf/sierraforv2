# ==============================================================================
# HISTOGRAMMES DE DENSITÉ PAR CLASSE DIAMÉTRIQUE
# Initial vs Final vs Coupé - Pinus & Quercus par UMM
# ==============================================================================

library(tidyverse)
library(patchwork)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Charger CONFIG pour area_parcela_ha
source("config/01_parametros_configuracion.R")

# Créer répertoires si nécessaire
if (!dir.exists("graficos")) dir.create("graficos", recursive = TRUE)

# ==============================================================================
# CHARGER DONNÉES
# ==============================================================================

cat("\n[1/4] Chargement des données...\n")

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
      num_muestreos = n_distinct(muestreo),
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

# ==============================================================================
# PRÉPARER DONNÉES PAR ÉTAT
# ==============================================================================

cat("\n[2/4] Préparation des données...\n")

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

# Arbres COUPÉS (toutes années confondues)
if (nrow(cortas) > 0) {
  
  cortado <- filtrar_arboles_vivos(cortas) %>%
    mutate(
      rodal = rodal_cortado,
      clase_d = asignar_clase_diametrica(diametro_normal)
    ) %>%
    group_by(rodal, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      .groups = "drop"
    )
  
  # Info muestreo pour cortas
  info_cortas <- cortas %>%
    group_by(rodal_cortado) %>%
    summarise(
      num_muestreos = first(num_muestreos_realizados),  # ✓ TOTAL sites UMM
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = num_muestreos * CONFIG$area_parcela_ha
    )
  
  cortado <- cortado %>%
    left_join(info_cortas, by = c("rodal" = "rodal_cortado")) %>%
    mutate(
      n_arboles_por_ha = n_arboles / area_muestreada_ha
    )
  
  cat(sprintf("  ✓ Cortado: %d UMM avec coupes\n", 
              n_distinct(cortado$rodal)))
  
} else {
  cortado <- tibble()
  cat("  ⚠ Aucune coupe enregistrée\n")
}

# ==============================================================================
# FONCTION CRÉATION HISTOGRAMME
# ==============================================================================

#' Créer histogramme empilé Pinus + Quercus
crear_histograma <- function(datos, titulo, rodal_id = NULL) {
  
  # Filtrer par UMM si spécifié
  if (!is.null(rodal_id)) {
    datos <- datos %>% filter(rodal == rodal_id)
  }
  
  # Si pas de données, retourner NULL
  if (nrow(datos) == 0) {
    return(NULL)
  }
  
  # Assurer factor avec tous les niveaux
  breaks <- seq(5, 95, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  datos <- datos %>%
    mutate(clase_d = factor(clase_d, levels = labels))
  
  # Créer graphique
  p <- ggplot(datos, aes(x = clase_d, y = n_arboles_por_ha, fill = genero_grupo)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = c("Pinus" = "#2E7D32", "Quercus" = "#D84315"),
      name = "Género"
    ) +
    labs(
      title = titulo,
      x = "Classe diamétrique (cm)",
      y = "Densité (arbres/ha)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      legend.position = "right",
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
  return(p)
}

# ==============================================================================
# GRAPHIQUES GLOBAUX (TOUS LES UMM COMBINÉS)
# ==============================================================================

cat("\n[3/4] Génération des graphiques globaux...\n")

# Agréger tous les UMM
inicial_total <- inicial %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_por_ha = sum(n_arboles_por_ha),
    .groups = "drop"
  )

final_total <- final %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_por_ha = sum(n_arboles_por_ha),
    .groups = "drop"
  )

cortado_total <- cortado %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_por_ha = sum(n_arboles_por_ha),
    .groups = "drop"
  )

# Créer graphiques
p1 <- crear_histograma(inicial_total, "État Initial (Année 0)")
p2 <- crear_histograma(final_total, "État Final (Année 10)")
p3 <- crear_histograma(cortado_total, "Arbres Coupés (Années 1-10)")

# Combiner
if (!is.null(p1) && !is.null(p2) && !is.null(p3)) {
  
  p_global <- p1 / p2 / p3 + 
    plot_annotation(
      title = "Évolution de la Densité Forestière - Tous les UMM",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
    )
  
  ggsave(
    "graficos/densidad_diametrica_GLOBAL.png",
    p_global,
    width = 12,
    height = 14,
    dpi = 300
  )
  
  cat("  ✓ densidad_diametrica_GLOBAL.png\n")
}

# ==============================================================================
# GRAPHIQUES PAR UMM INDIVIDUEL
# ==============================================================================

cat("\n[4/4] Génération des graphiques par UMM...\n")

# Liste des UMM
rodales <- sort(unique(inicial$rodal))

for (rodal_id in rodales) {
  
  cat(sprintf("  Traitement UMM %d...\n", rodal_id))
  
  # Créer les 3 graphiques pour cet UMM
  p1_umm <- crear_histograma(
    inicial, 
    sprintf("UMM %d - Initial (Année 0)", rodal_id), 
    rodal_id
  )
  
  p2_umm <- crear_histograma(
    final, 
    sprintf("UMM %d - Final (Année 10)", rodal_id), 
    rodal_id
  )
  
  p3_umm <- crear_histograma(
    cortado, 
    sprintf("UMM %d - Coupé", rodal_id), 
    rodal_id
  )
  
  # Filtrer graphiques valides
  graficos_validos <- list(p1_umm, p2_umm, p3_umm)
  graficos_validos <- graficos_validos[!sapply(graficos_validos, is.null)]
  
  if (length(graficos_validos) > 0) {
    
    # Assembler selon disponibilité
    if (length(graficos_validos) == 3) {
      p_umm <- p1_umm / p2_umm / p3_umm
    } else if (length(graficos_validos) == 2) {
      p_umm <- graficos_validos[[1]] / graficos_validos[[2]]
    } else {
      p_umm <- graficos_validos[[1]]
    }
    
    # Ajouter titre général
    p_umm <- p_umm + 
      plot_annotation(
        title = sprintf("Évolution de la Densité Forestière - UMM %d", rodal_id),
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
      )
    
    # Sauvegarder
    ggsave(
      sprintf("graficos/densidad_diametrica_UMM_%02d.png", rodal_id),
      p_umm,
      width = 12,
      height = 14,
      dpi = 300
    )
    
    cat(sprintf("    ✓ UMM_%02d.png\n", rodal_id))
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

cat("\nSTATISTIQUES:\n")
cat(sprintf("  • UMM traités:          %d\n", length(rodales)))
cat(sprintf("  • Classes diamétriques: %d (5 cm)\n", 18))
cat(sprintf("  • Genres:               %s\n", 
            paste(sort(unique(inicial$genero_grupo)), collapse = ", ")))

if (nrow(cortado) > 0) {
  vol_coupe_total <- sum(cortado$n_arboles_por_ha, na.rm = TRUE)
  cat(sprintf("  • Densité coupée:       %.1f arbres/ha (total)\n", vol_coupe_total))
}

cat("\n")

# ==============================================================================
# TABLEAU RÉCAPITULATIF
# ==============================================================================

cat("RÉCAPITULATIF PAR UMM:\n")
cat("═══════════════════════════════════════════════════════════\n\n")

resumen <- tibble(
  UMM = rodales
) %>%
  left_join(
    inicial %>%
      group_by(rodal) %>%
      summarise(dens_inicial = sum(n_arboles_por_ha), .groups = "drop"),
    by = c("UMM" = "rodal")
  ) %>%
  left_join(
    final %>%
      group_by(rodal) %>%
      summarise(dens_final = sum(n_arboles_por_ha), .groups = "drop"),
    by = c("UMM" = "rodal")
  ) %>%
  left_join(
    cortado %>%
      group_by(rodal) %>%
      summarise(dens_coupe = sum(n_arboles_por_ha), .groups = "drop"),
    by = c("UMM" = "rodal")
  ) %>%
  mutate(
    dens_coupe = replace_na(dens_coupe, 0),
    diff_dens = dens_final - dens_inicial,
    pct_change = (diff_dens / dens_inicial) * 100
  )

print(resumen, n = Inf)

cat("\n")
