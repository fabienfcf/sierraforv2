# ==============================================================================
# 14_OPTIMIZADOR_CORTAS.R - DISTANCE À COURBE LIOCOURT
# ==============================================================================
# 
# PHILOSOPHIE:
#   1. ICA fixe le volume à couper (sustainability)
#   2. Priorité: Arbres matures d'abord (>= D_MADUREZ)
#   3. Liocourt: Sélection par DISTANCE à la courbe idéale
#
# LOGIQUE DISTANCE:
#   - Chaque arbre a une "distance" à la courbe Liocourt
#   - Distance = position dans classe - n_liocourt
#   - Arbres avec distance > 0 = AU-DESSUS de la courbe (excédent)
#   - Arbres avec distance ≤ 0 = SUR/SOUS la courbe (à conserver)
#   - Sélection: distance décroissante jusqu'à vol_objetivo
#
# ==============================================================================

library(tidyverse)

if (!exists("CONFIG")) {
  stop("❌ CONFIG no cargado. Ejecuta: source('01_parametros_configuracion.R')")
}

# ==============================================================================
# CALCULAR VOLUMEN OBJETIVO (STEP 1)
# ==============================================================================

calcular_volumen_objetivo <- function(arboles_vivos, 
                                      config = CONFIG,
                                      corte_config) {
  
  rodal_actual <- unique(arboles_vivos$rodal)[1]
  archivo_ica <- "resultados/31_ica_por_rodal.csv"
  
  if (!file.exists(archivo_ica)) {
    stop("❌ Archivo ICA no encontrado: ", archivo_ica)
  }
  
  # Cargar ICA pre-calculado
  ica_data <- read.csv(archivo_ica)
  ica_rodal <- ica_data %>% filter(rodal == rodal_actual)
  
  if (nrow(ica_rodal) == 0) {
    stop("❌ ICA no encontrado para rodal ", rodal_actual)
  }
  
  # Extraer valores
  ica_anual_m3_ha <- ica_rodal$ICA_m3_ha
  superficie_ha <- first(arboles_vivos$superficie_corta_ha)
  
  # Calcular volumen objetivo por ha
  ica_10anos_m3_ha <- ica_anual_m3_ha * 10
  vol_objetivo_m3_ha <- ica_10anos_m3_ha * (corte_config$intensidad_pct / 100)
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  VOLUMEN OBJETIVO (ICA)                                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  cat(sprintf("  ICA anual:        %.3f m³/ha/año\n", ica_anual_m3_ha))
  cat(sprintf("  ICA 10 años:      %.2f m³/ha\n", ica_10anos_m3_ha))
  cat(sprintf("  Intensidad:       %d%%\n", corte_config$intensidad_pct))
  cat(sprintf("  ▶ Vol objetivo:   %.2f m³/ha\n", vol_objetivo_m3_ha))
  cat(sprintf("  Superficie UMM:   %.2f ha\n", superficie_ha))
  cat(sprintf("  ▶ Vol total UMM:  %.2f m³\n\n", vol_objetivo_m3_ha * superficie_ha))
  
  list(
    vol_objetivo_m3_ha = vol_objetivo_m3_ha,
    vol_objetivo_total = vol_objetivo_m3_ha * superficie_ha,
    ica_anual_m3_ha = ica_anual_m3_ha,
    superficie_ha = superficie_ha
  )
}

# ==============================================================================
# MARCAR ÁRBOLES MADUROS (STEP 2)
# ==============================================================================

marcar_maduros <- function(arboles_vivos, 
                           vol_restante_m3_ha,
                           corte_config,
                           config = CONFIG) {
  
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  PASO 1: ÁRBOLES MADUROS                                  ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  superficie_ha <- first(arboles_vivos$superficie_corta_ha)
  num_muestreos <- first(arboles_vivos$num_muestreos_realizados)
  area_muestreada_ha <- num_muestreos * config$area_parcela_ha
  
  # Identificar maduros
  candidatos_maduros <- arboles_vivos %>%
    mutate(
      d_madurez = case_when(
        genero_grupo == "Pinus" ~ config$d_madurez$Pinus,
        genero_grupo == "Quercus" ~ config$d_madurez$Quercus,
        TRUE ~ 999
      ),
      es_maduro = diametro_normal >= d_madurez,
      protegido = case_when(
        genero_grupo == "Pinus" & corte_config$proteger_maduros_pinus ~ TRUE,
        genero_grupo == "Quercus" & corte_config$proteger_maduros_quercus ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(es_maduro, !protegido)
  
  if (nrow(candidatos_maduros) == 0) {
    cat("  ℹ️  No hay árboles maduros disponibles\n")
    cat(sprintf("  Vol restante: %.2f m³/ha\n\n", vol_restante_m3_ha))
    return(list(
      marcados = tibble(),
      vol_marcado_m3_ha = 0,
      vol_restante_m3_ha = vol_restante_m3_ha
    ))
  }
  
  # Estadísticas maduros
  stats_maduros <- candidatos_maduros %>%
    group_by(genero_grupo) %>%
    summarise(
      n = n(),
      vol_total = sum(volumen_m3),
      .groups = "drop"
    ) %>%
    mutate(
      n_ha = n / area_muestreada_ha,
      vol_m3_ha = vol_total / area_muestreada_ha
    )
  
  cat("  Disponibles:\n")
  for (i in 1:nrow(stats_maduros)) {
    cat(sprintf("    %s: %d árboles (%.1f/ha), %.2f m³/ha\n",
                stats_maduros$genero_grupo[i],
                stats_maduros$n[i],
                stats_maduros$n_ha[i],
                stats_maduros$vol_m3_ha[i]))
  }
  
  # Ordenar: gros d'abord
  candidatos_maduros <- candidatos_maduros %>%
    arrange(desc(diametro_normal))
  
  # Selección greedy
  vol_acum <- 0
  vol_objetivo_inventario <- vol_restante_m3_ha * area_muestreada_ha
  marcados <- tibble()
  
  for (i in 1:nrow(candidatos_maduros)) {
    if (vol_acum >= vol_objetivo_inventario) break
    
    arbol <- candidatos_maduros[i, ]
    marcados <- bind_rows(marcados, arbol)
    vol_acum <- vol_acum + arbol$volumen_m3
  }
  
  vol_marcado_m3_ha <- vol_acum / area_muestreada_ha
  vol_restante_m3_ha <- vol_restante_m3_ha - vol_marcado_m3_ha
  
  cat(sprintf("\n  ▶ Marcados: %d árboles, %.2f m³/ha\n", 
              nrow(marcados), vol_marcado_m3_ha))
  cat(sprintf("  ▶ Restante: %.2f m³/ha\n\n", 
              max(0, vol_restante_m3_ha)))
  
  list(
    marcados = marcados,
    vol_marcado_m3_ha = vol_marcado_m3_ha,
    vol_restante_m3_ha = max(0, vol_restante_m3_ha)
  )
}

# ==============================================================================
# FUNCIÓN MODIFICADA: calcular_distribucion_liocourt
# ==============================================================================
# 
# CAMBIO PRINCIPAL:
#   - N_REF ya NO se extrae de la distribución real de árboles
#   - N_REF viene de N_REF_LIOCOURT_POR_UMM (definido arbitrariamente)
#
# VENTAJAS:
#   1. Curva Liocourt INDEPENDIENTE de la densidad actual
#   2. Permite definir objetivos de manejo por UMM
#   3. Más control sobre la estructura deseada del bosque
#
# ==============================================================================

calcular_distribucion_liocourt <- function(arboles_vivos,
                                           rodal_id,  # ← NUEVO PARÁMETRO
                                           q = Q_FACTOR,
                                           tolerancia = TOLERANCIA_EQUILIBRIO) {
  
  # Clasificar por clase diamétrica (5 cm)
  dist_actual <- arboles_vivos %>%
    mutate(clase_d = floor(diametro_normal / 5) * 5) %>%
    group_by(clase_d) %>%
    summarise(
      n_actual = n(),
      vol_total = sum(volumen_m3),
      .groups = "drop"
    ) %>%
    arrange(clase_d)
  
  if (nrow(dist_actual) == 0) return(NULL)
  
  # ════════════════════════════════════════════════════════════════════════
  # CAMBIO CRÍTICO: N_REF ARBITRARIO (no extraído de datos reales)
  # ════════════════════════════════════════════════════════════════════════
  
  clase_ref <- CLASE_REFERENCIA_LIOCOURT
  
  # ANTES (versión antigua):
  # n_ref <- dist_actual %>%
  #   filter(clase_d == clase_ref) %>%
  #   pull(n_actual)
  
  # AHORA (versión nueva):
  n_ref <- obtener_n_ref_liocourt(rodal_id)
  
  # Verificar que n_ref es válido
  if (is.na(n_ref) || n_ref <= 0) {
    warning(sprintf("⚠️ N_REF inválido para UMM %d, usando 10 árb/ha", rodal_id))
    n_ref <- 10
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # CALCULAR CURVA LIOCOURT IDEAL
  # ════════════════════════════════════════════════════════════════════════
  
  # Rango de clases diamétricas
  d_min <- min(dist_actual$clase_d)
  d_max <- max(dist_actual$clase_d)
  clases <- seq(d_min, d_max, by = 5)
  
  # Distribución Liocourt: N(d) = N_ref × q^((d_ref - d) / amplitud)
  dist_liocourt <- data.frame(clase_d = clases) %>%
    mutate(
      idx = (clase_ref - clase_d) / 5,
      n_liocourt = round(n_ref * q^idx)
    ) %>%
    left_join(dist_actual, by = "clase_d") %>%
    mutate(
      n_actual = replace_na(n_actual, 0),
      vol_total = replace_na(vol_total, 0),
      excedente = n_actual - n_liocourt,
      excedente_pct = ifelse(n_liocourt > 0, 
                             (excedente / n_liocourt) * 100, 
                             0)
    ) %>%
    arrange(clase_d)
  
  # ════════════════════════════════════════════════════════════════════════
  # REPORTE
  # ════════════════════════════════════════════════════════════════════════
  
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  DISTRIBUCIÓN LIOCOURT (GUÍA)                             ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  cat(sprintf("  UMM: %d\n", rodal_id))
  cat(sprintf("  Clase ref: %d cm\n", clase_ref))
  cat(sprintf("  N_REF (arbitrario): %d árb/ha\n", n_ref))
  cat(sprintf("  Q-factor: %.2f\n", q))
  cat(sprintf("  Tolerancia: ±%d%%\n\n", tolerancia))
  
  # Densidad actual en clase de referencia (para comparación)
  n_actual_ref <- dist_actual %>%
    filter(clase_d == clase_ref) %>%
    pull(n_actual)
  
  if (length(n_actual_ref) == 0) {
    n_actual_ref <- 0
  }
  
  cat(sprintf("  ℹ️  Densidad ACTUAL en clase ref: %d árb/ha\n", n_actual_ref))
  cat(sprintf("  ℹ️  Densidad OBJETIVO (N_REF): %d árb/ha\n", n_ref))
  
  if (n_actual_ref > n_ref) {
    cat(sprintf("  → Clase ref está SOBREPOBLADA (+%d árb/ha)\n\n", 
                n_actual_ref - n_ref))
  } else if (n_actual_ref < n_ref) {
    cat(sprintf("  → Clase ref está SUBPOBLADA (-%d árb/ha)\n\n", 
                n_ref - n_actual_ref))
  } else {
    cat("  → Clase ref está EN EQUILIBRIO\n\n")
  }
  
  # Mostrar clases con excedente
  clases_excedentes <- dist_liocourt %>%
    filter(excedente > 0) %>%
    arrange(desc(excedente_pct))
  
  if (nrow(clases_excedentes) > 0) {
    cat("  Clases sobrepobladas (respecto a curva ideal):\n")
    for (i in 1:min(5, nrow(clases_excedentes))) {
      cat(sprintf("    %d. Clase %d cm: +%.0f%% (%d árboles exceso)\n",
                  i,
                  clases_excedentes$clase_d[i],
                  clases_excedentes$excedente_pct[i],
                  clases_excedentes$excedente[i]))
    }
    cat("\n")
  }
  
  dist_liocourt
}


# ==============================================================================
# MARCAR SEGÚN LIOCOURT - DISTANCE À COURBE (STEP 4)
# ==============================================================================

marcar_liocourt_distance <- function(arboles_disponibles,
                                     dist_liocourt,
                                     vol_restante_m3_ha,
                                     corte_config,
                                     config = CONFIG) {
  
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  PASO 2: LIOCOURT (Distance à courbe)                    ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  if (vol_restante_m3_ha <= 0) {
    cat("  ✓ Objetivo ya alcanzado\n\n")
    return(list(marcados = tibble(), vol_marcado_m3_ha = 0))
  }
  
  num_muestreos <- first(arboles_disponibles$num_muestreos_realizados)
  area_muestreada_ha <- num_muestreos * config$area_parcela_ha
  superficie_ha <- first(arboles_disponibles$superficie_corta_ha)
  vol_objetivo_inventario <- vol_restante_m3_ha * area_muestreada_ha
  
  # STEP 1: Calcular position de cada arbre dans sa clase
  # ORDRE: ALÉATOIRE (neutre genre)
  # Seed basé sur rodal pour reproductibilité
  rodal_actual <- unique(arboles_disponibles$rodal)[1]
  set.seed(rodal_actual * 12345)  # Seed reproductible
  
  arboles_con_posicion <- arboles_disponibles %>%
    mutate(clase_d = floor(diametro_normal / 5) * 5) %>%
    arrange(clase_d, runif(n())) %>%  # Ordre aléatoire dans classe
    group_by(clase_d) %>%
    mutate(
      position = row_number()  # 1 à N, ordre aléatoire
    ) %>%
    ungroup()
  
  # STEP 2: Calculer distance à courbe Liocourt
  arboles_con_distance <- arboles_con_posicion %>%
    left_join(
      dist_liocourt %>% select(clase_d, n_liocourt),
      by = "clase_d"
    ) %>%
    mutate(
      n_liocourt = replace_na(n_liocourt, 0),
      distance = position - n_liocourt
    )
  
  # STEP 5: Ne garder QUE les arbres avec distance > 0 (au-dessus de la courbe)
  candidatos <- arboles_con_distance %>%
    filter(distance > 0)
  
  if (nrow(candidatos) == 0) {
    cat("  ℹ️  Aucun arbre au-dessus de la courbe Liocourt\n")
    cat("  → Distribution déjà équilibrée\n\n")
    return(list(marcados = tibble(), vol_marcado_m3_ha = 0))
  }
  
  cat(sprintf("  Candidatos (distance > 0): %d árboles\n", nrow(candidatos)))
  
  # Statistiques distances
  stats_distance <- candidatos %>%
    summarise(
      distance_max = max(distance),
      distance_mean = mean(distance),
      distance_min = min(distance),
      .groups = "drop"
    )
  
  cat(sprintf("  Distance max: %.0f | moyenne: %.1f | min: %.0f\n\n",
              stats_distance$distance_max,
              stats_distance$distance_mean,
              stats_distance$distance_min))
  
  # STEP 3: Trier par distance décroissante (plus grand excédent en premier)
  candidatos <- candidatos %>%
    arrange(desc(distance))
  
  # STEP 4: Sélection avec contrôle proportions (roll dice)
  prop_quercus_objetivo <- corte_config$proporcion_quercus
  
  if (!is.null(prop_quercus_objetivo) && !is.na(prop_quercus_objetivo)) {
    
    cat("  → Contrôle proportions actif:\n")
    cat(sprintf("     Objetivo: %.0f%% Quercus / %.0f%% Pinus\n\n",
                prop_quercus_objetivo * 100,
                (1 - prop_quercus_objetivo) * 100))
    
    # Sélection avec roll dice
    set.seed(unique(arboles_disponibles$rodal)[1] * 54321)  # Seed pour dice
    
    vol_acum <- 0
    vol_quercus <- 0
    vol_pinus <- 0
    marcados <- tibble()
    
    for (i in 1:nrow(candidatos)) {
      if (vol_acum >= vol_objetivo_inventario) break
      
      arbol <- candidatos[i, ]
      
      # Calcular proporciones actuales
      if (vol_acum > 0) {
        prop_actual_quercus <- vol_quercus / vol_acum
        prop_actual_pinus <- vol_pinus / vol_acum
      } else {
        # Primeros árboles: usar objetivos
        prop_actual_quercus <- prop_quercus_objetivo
        prop_actual_pinus <- 1 - prop_quercus_objetivo
      }
      
      # Calcular probabilidad de aceptación
      if (arbol$genero_grupo == "Quercus") {
        # Si Quercus sous-représenté, prob élevée
        prob_accept <- min(1.0, prop_quercus_objetivo / max(prop_actual_quercus, 0.01))
      } else {
        # Si Pinus sous-représenté, prob élevée
        prob_accept <- min(1.0, (1 - prop_quercus_objetivo) / max(prop_actual_pinus, 0.01))
      }
      
      # Roll dice
      if (runif(1) < prob_accept) {
        marcados <- bind_rows(marcados, arbol)
        vol_acum <- vol_acum + arbol$volumen_m3
        
        if (arbol$genero_grupo == "Quercus") {
          vol_quercus <- vol_quercus + arbol$volumen_m3
        } else {
          vol_pinus <- vol_pinus + arbol$volumen_m3
        }
      }
    }
    
    # Résumé proportions finales
    if (vol_acum > 0) {
      prop_final_quercus <- vol_quercus / vol_acum
      prop_final_pinus <- vol_pinus / vol_acum
      
      cat(sprintf("  Proporciones finales:\n"))
      cat(sprintf("     Quercus: %.1f%% (objetivo: %.0f%%)\n",
                  prop_final_quercus * 100,
                  prop_quercus_objetivo * 100))
      cat(sprintf("     Pinus:   %.1f%% (objetivo: %.0f%%)\n\n",
                  prop_final_pinus * 100,
                  (1 - prop_quercus_objetivo) * 100))
    }
    
  } else {
    
    # Sélection simple sans contrôle proportions
    vol_acum <- 0
    marcados <- tibble()
    
    for (i in 1:nrow(candidatos)) {
      if (vol_acum >= vol_objetivo_inventario) break
      
      arbol <- candidatos[i, ]
      marcados <- bind_rows(marcados, arbol)
      vol_acum <- vol_acum + arbol$volumen_m3
    }
  }
  
  vol_marcado_m3_ha <- vol_acum / area_muestreada_ha
  
  cat(sprintf("  ▶ Marcados: %d árboles, %.2f m³/ha\n", 
              nrow(marcados), vol_marcado_m3_ha))
  
  # Resumen por clase
  if (nrow(marcados) > 0) {
    resumen_clase <- marcados %>%
      group_by(clase_d) %>%
      summarise(
        n = n(),
        distance_mean = mean(distance),
        .groups = "drop"
      ) %>%
      arrange(clase_d)
    
    cat("\n  Distribución marcados:\n")
    for (i in 1:nrow(resumen_clase)) {
      cat(sprintf("    Clase %d cm: %d árboles (distance moy: %.1f)\n",
                  resumen_clase$clase_d[i],
                  resumen_clase$n[i],
                  resumen_clase$distance_mean[i]))
    }
  }
  cat("\n")
  
  list(
    marcados = marcados,
    vol_marcado_m3_ha = vol_marcado_m3_ha
  )
}

# ==============================================================================
# WORKFLOW PRINCIPAL
# ==============================================================================

calcular_plan_cortas <- function(arboles_df,
                                 config = CONFIG,
                                 arboles_inicial = NULL,
                                 arboles_ano_anterior = NULL,
                                 corte_config = NULL,
                                 ano_actual = NULL) {
  
  if (is.null(corte_config)) {
    corte_config <- configurar_corte()
  }
  
  
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  OPTIMIZADOR DE CORTAS                                    ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  # Filtrar vivos
  vivos <- arboles_df %>% filtrar_arboles_vivos()
  
  if (nrow(vivos) < 10) {
    cat("\n  ⚠️  Muy pocos árboles vivos\n")
    return(list(
      arboles_marcados = tibble(),
      resumen = tibble(),
      vol_info = list(vol_objetivo_m3_ha = 0)
    ))
  }
  
  # Aplicar DMC
  vivos <- vivos %>%
    mutate(
      dmc = case_when(
        genero_grupo == "Pinus" ~ config$dmc$Pinus,
        genero_grupo == "Quercus" ~ config$dmc$Quercus,
        TRUE ~ 25
      )
    ) %>%
    filter(diametro_normal >= dmc)
  
  rodal_actual <- unique(vivos$rodal)[1]
  
  # STEP 1: Volumen objetivo
  vol_info <- calcular_volumen_objetivo(vivos, config, corte_config)
  vol_restante_m3_ha <- vol_info$vol_objetivo_m3_ha
  
  # STEP 2: Marcar maduros
  resultado_maduros <- marcar_maduros(vivos, vol_restante_m3_ha, corte_config, config)
  marcados_maduros <- resultado_maduros$marcados
  vol_restante_m3_ha <- resultado_maduros$vol_restante_m3_ha
  
  # Árboles disponibles para Liocourt (excluir maduros ya marcados)
  vivos_para_liocourt <- vivos %>%
    filter(!arbol_id %in% marcados_maduros$arbol_id)
  
  # STEP 3: Distribución Liocourt
    dist_liocourt <- calcular_distribucion_liocourt(
      vivos_para_liocourt,
     rodal_id = rodal_actual,  # ← AGREGAR ESTE PARÁMETRO
     q = corte_config$q_factor,
      tolerancia = corte_config$tolerancia
   )
  
  # STEP 4: Marcar según Liocourt (distance à courbe)
  resultado_liocourt <- marcar_liocourt_distance(
    vivos_para_liocourt,
    dist_liocourt,
    vol_restante_m3_ha,
    corte_config,
    config
  )
  marcados_liocourt <- resultado_liocourt$marcados
  
  # Combinar todos los marcados
  marcados_total <- bind_rows(marcados_maduros, marcados_liocourt)
  
  num_muestreos <- first(marcados_total$num_muestreos_realizados)
  area_muestreada_ha <- num_muestreos * CONFIG$area_parcela_ha
  
  
  # RESUMEN FINAL
  if (nrow(marcados_total) > 0) {
    superficie_ha <- vol_info$superficie_ha
    vol_marcado_m3_ha <- sum(marcados_total$volumen_m3) / area_muestreada_ha
    vol_marcado_total <- vol_marcado_m3_ha * superficie_ha
    
    cat("╔═══════════════════════════════════════════════════════════╗\n")
    cat("║  RESUMEN FINAL                                            ║\n")
    cat("╚═══════════════════════════════════════════════════════════╝\n")
    cat(sprintf("  Árboles marcados:  %d\n", nrow(marcados_total)))
    cat(sprintf("  Volumen marcado:   %.2f m³/ha (%.2f m³ total)\n",
                vol_marcado_m3_ha, vol_marcado_total))
    cat(sprintf("  Objetivo:          %.2f m³/ha (%.2f m³ total)\n",
                vol_info$vol_objetivo_m3_ha, vol_info$vol_objetivo_total))
    cat(sprintf("  Diferencia:        %.2f m³/ha (%.1f%%)\n\n",
                vol_marcado_m3_ha - vol_info$vol_objetivo_m3_ha,
                ((vol_marcado_m3_ha / vol_info$vol_objetivo_m3_ha) - 1) * 100))
    
    # Detalle por género
    detalle_genero <- marcados_total %>%
      group_by(genero_grupo) %>%
      summarise(
        n = n(),
        vol = sum(volumen_m3),
        .groups = "drop"
      ) %>%
      mutate(
        n_ha = n / area_muestreada_ha,
        vol_m3_ha = vol / area_muestreada_ha
      )
    
    cat("  Por género:\n")
    for (i in 1:nrow(detalle_genero)) {
      cat(sprintf("    %s: %d árb (%.1f/ha), %.2f m³/ha\n",
                  detalle_genero$genero_grupo[i],
                  detalle_genero$n[i],
                  detalle_genero$n_ha[i],
                  detalle_genero$vol_m3_ha[i]))
    }
    cat("\n")
    
    resumen <- tibble(
      metodo = "ICA",
      n_arboles = nrow(marcados_total),
      vol_m3_ha = vol_marcado_m3_ha,
      vol_total_m3 = vol_marcado_total,
      vol_objetivo_m3_ha = vol_info$vol_objetivo_m3_ha,
      diferencia_pct = ((vol_marcado_m3_ha / vol_info$vol_objetivo_m3_ha) - 1) * 100
    )
  } else {
    resumen <- tibble()
  }
  
  list(
    arboles_marcados = marcados_total,
    resumen = resumen,
    vol_info = vol_info,
    dist_liocourt = dist_liocourt
  )
}

# ==============================================================================
# APLICAR CORTAS
# ==============================================================================

aplicar_cortas <- function(arboles_df, plan_cortas, ano_corta) {
  
  if (nrow(plan_cortas$arboles_marcados) == 0) {
    return(arboles_df)
  }
  
  ids_marcados <- plan_cortas$arboles_marcados$arbol_id
  
  arboles_df %>%
    mutate(
      fue_cortado = arbol_id %in% ids_marcados,
      dominancia = if_else(fue_cortado, 8, dominancia),
      ano_corta = if_else(fue_cortado, ano_corta, NA_real_)
    )
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Optimizador de cortas (DISTANCE À COURBE LIOCOURT)\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  PHILOSOPHIE:\n")
cat("    1. ICA fixe le volume (sustainability)\n")
cat("    2. Maduros d'abord (si non protégés)\n")
cat("    3. Distance à courbe: arbres les plus excédentaires\n\n")
cat("  LOGIQUE DISTANCE:\n")
cat("    • Position = rang aléatoire dans classe (neutre)\n")
cat("    • Distance = position - n_liocourt\n")
cat("    • Distance > 0 → AU-DESSUS courbe (excédent)\n")
cat("    • Sélection par distance décroissante\n\n")
cat("  CONTRÔLE PROPORTIONS:\n")
cat("    • Si proporcion_quercus définie: roll dice\n")
cat("    • Auto-régulation vers proportions cibles\n")
cat("    • Garde priorité distance Liocourt\n\n")