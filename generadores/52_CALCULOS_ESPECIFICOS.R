# ==============================================================================
# CÁLCULOS ESPECÍFICOS REUTILIZABLES - SIERRAFOR
# ==============================================================================
#
# Este módulo contiene funciones de cálculo específicas del dominio forestal
# que serán reutilizadas en múltiples secciones de reportes.
#
# Categorías:
#   1. Diversidad (Shannon, Simpson, riqueza)
#   2. Mortalidad (muertos en pie, tocones)
#   3. Demografía (balance entrada/salida)
#   4. Resumen predio (agregaciones)
#   5. Productividad (ranking, clasificación)
#   6. Regeneración
#   7. Biofísica (erosión, sanidad, uso pecuario)
#
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. ÍNDICES DE DIVERSIDAD
# ==============================================================================

#' Calcular índices de diversidad
#'
#' @param arboles_df Data frame con árboles
#' @param nivel "predio" o "rodal" para agrupar
#' @param columna_especie Nombre columna con especies (default: "nombre_cientifico")
#' @return Data frame con índices de diversidad
#' @examples
#' diversidad <- calcular_diversidad(arboles_df, nivel = "rodal")
calcular_diversidad <- function(arboles_df, 
                               nivel = "predio",
                               columna_especie = "nombre_cientifico") {
  
  # Validación
  if (!columna_especie %in% names(arboles_df)) {
    stop(sprintf("Columna '%s' no existe en datos", columna_especie))
  }
  
  # Filtrar solo árboles vivos
  if ("dominancia" %in% names(arboles_df)) {
    arboles_df <- filtrar_arboles_vivos(arboles_df)
  }
  
  # Función para calcular índices
  calcular_indices <- function(df) {
    
    # Contar individuos por especie
    freq <- df %>%
      count(.data[[columna_especie]]) %>%
      mutate(p = n / sum(n))
    
    # Shannon: H' = -Σ(pi × ln(pi))
    shannon <- -sum(freq$p * log(freq$p), na.rm = TRUE)
    
    # Simpson: D = 1 - Σ(pi²)
    simpson <- 1 - sum(freq$p^2, na.rm = TRUE)
    
    # Riqueza específica (número de especies)
    riqueza <- nrow(freq)
    
    # Equitabilidad de Pielou: J' = H' / ln(S)
    equitabilidad <- if (riqueza > 1) {
      shannon / log(riqueza)
    } else {
      NA_real_
    }
    
    return(tibble(
      n_individuos = nrow(df),
      riqueza = riqueza,
      shannon = shannon,
      simpson = simpson,
      equitabilidad = equitabilidad
    ))
  }
  
  # Calcular según nivel
  if (nivel == "predio") {
    resultado <- calcular_indices(arboles_df)
  } else if (nivel == "rodal") {
    resultado <- arboles_df %>%
      group_by(rodal) %>%
      group_modify(~ calcular_indices(.x)) %>%
      ungroup()
  } else {
    stop("Nivel debe ser 'predio' o 'rodal'")
  }
  
  return(resultado)
}

# ==============================================================================
# 2. ANÁLISIS DE MORTALIDAD (MUERTOS EN PIE Y TOCONES)
# ==============================================================================

#' Calcular métricas de muertos en pie y tocones
#'
#' @param arboles_df Data frame con árboles
#' @param config Objeto CONFIG con parámetros
#' @param altura_tocones Altura asumida para tocones (default: 1.5m)
#' @param factor_ajuste_d Factor para ajustar diámetro tocones (default: 0.85)
#' @return Lista con muertos_pie y tocones
#' @details
#'   - Muertos en pie: dominancia = 7-8
#'   - Tocones: dominancia = 9
#'   - Diámetro tocón ajustado: d_medido × factor_ajuste_d
#'   - Altura inferida con ecuaciones alométricas
calcular_muertos_tocones <- function(arboles_df, 
                                    config = CONFIG,
                                    altura_tocones = 1.5,
                                    factor_ajuste_d = 0.85) {
  
  # Cargar función de cálculo de volumen si no está disponible
  if (!exists("calcular_volumen_arbol")) {
    stop("Función calcular_volumen_arbol() no encontrada. Cargar 15_core_calculos.R")
  }
  
  if (!exists("calcular_area_basal")) {
    stop("Función calcular_area_basal() no encontrada. Cargar 15_core_calculos.R")
  }
  
  if (!exists("expandir_a_hectarea")) {
    stop("Función expandir_a_hectarea() no encontrada. Cargar 15_core_calculos.R")
  }
  
  # Calcular n_sitios para expansión
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  # ============================================================================
  # MUERTOS EN PIE (dominancia = 7 y 8, excluir tocones)
  # ============================================================================
  
  muertos_pie <- arboles_df %>%
    filter(dominancia %in% c(7, 8)) %>%
    mutate(
      area_basal = calcular_area_basal(diametro_normal)
    ) %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_muertos_obs = n(),
      ab_total_m2 = sum(area_basal, na.rm = TRUE),
      vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_ha = expandir_a_hectarea(n_muertos_obs / n_sitios, config$area_parcela_ha),
      ab_ha_m2 = expandir_a_hectarea(ab_total_m2 / n_sitios, config$area_parcela_ha),
      vol_ha_m3 = expandir_a_hectarea(vol_total_m3 / n_sitios, config$area_parcela_ha)
    ) %>%
    select(rodal, genero_grupo, n_muertos_obs, n_ha, ab_ha_m2, vol_ha_m3, d_medio_cm)
  
  # ============================================================================
  # TOCONES (dominancia = 9)
  # ============================================================================
  
  # Ajustar diámetro (medido al nivel del suelo → 1.3m)
  # Inferir altura usando ecuaciones alométricas del género
  
  tocones <- arboles_df %>%
    filter(dominancia == 9) %>%
    mutate(
      # Ajustar diámetro
      d_ajustado = diametro_normal * factor_ajuste_d,
      
      # Inferir altura usando ecuaciones del género
      # Usar altura promedio del género si no hay ecuación
      h_inferida = altura_tocones
    )
  
  # Si hay datos de altura en el género, usar promedio
  alturas_genero <- arboles_df %>%
    filter(es_arbol_vivo(dominancia), !is.na(altura_total)) %>%
    group_by(genero_grupo) %>%
    summarise(h_media_genero = mean(altura_total, na.rm = TRUE), .groups = "drop")
  
  tocones <- tocones %>%
    left_join(alturas_genero, by = "genero_grupo") %>%
    mutate(
      h_final = coalesce(h_media_genero, h_inferida),
      
      # Calcular volumen con diámetro ajustado y altura inferida
      vol_estimado = calcular_volumen_arbol(
        d = d_ajustado,
        h = h_final,
        tipo = tipo,
        a = a,
        b = b,
        c = c
      ),
      
      area_basal = calcular_area_basal(d_ajustado)
    )
  
  # Agregar por rodal y género
  tocones_resumen <- tocones %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_tocones_obs = n(),
      d_ajustado_medio = mean(d_ajustado, na.rm = TRUE),
      ab_total_m2 = sum(area_basal, na.rm = TRUE),
      vol_total_m3 = sum(vol_estimado, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_ha = expandir_a_hectarea(n_tocones_obs / n_sitios, config$area_parcela_ha),
      ab_ha_m2 = expandir_a_hectarea(ab_total_m2 / n_sitios, config$area_parcela_ha),
      vol_ha_m3 = expandir_a_hectarea(vol_total_m3 / n_sitios, config$area_parcela_ha)
    ) %>%
    select(rodal, genero_grupo, n_tocones_obs, n_ha, ab_ha_m2, vol_ha_m3, d_ajustado_medio)
  
  # Retornar ambos
  return(list(
    muertos_pie = muertos_pie,
    tocones = tocones_resumen,
    parametros = list(
      altura_tocones = altura_tocones,
      factor_ajuste_d = factor_ajuste_d,
      n_sitios = n_sitios
    )
  ))
}

# ==============================================================================
# 3. BALANCE DEMOGRÁFICO
# ==============================================================================

#' Calcular balance demográfico de simulación
#'
#' @param resultado_simulacion Resultado de simular_crecimiento_rodal()
#' @param incluir_cortas Incluir árboles cortados en balance (default: TRUE)
#' @return Data frame con flujos demográficos
calcular_balance_demografico <- function(resultado_simulacion,
                                        incluir_cortas = TRUE) {
  
  inicial <- resultado_simulacion$poblacion_inicial
  final <- resultado_simulacion$poblacion_final
  
  # Contar estados
  n_inicial <- nrow(filtrar_arboles_vivos(inicial))
  n_final <- nrow(filtrar_arboles_vivos(final))

  # Identificar eventos
  # Muertos: árboles con dominancia 7, 8, 9 que no estaban así inicialmente
  ids_iniciales <- filtrar_arboles_vivos(inicial) %>%
    pull(arbol_id)

  muertos <- final %>%
    filter(arbol_id %in% ids_iniciales, !es_arbol_vivo(dominancia)) %>%
    nrow()
  
  # Reclutas: árboles con ID que contiene "RECLUTA"
  reclutas <- final %>%
    filter(str_detect(arbol_id, "RECLUTA")) %>%
    nrow()
  
  # Cortados (si aplica)
  cortados <- if (incluir_cortas && "año_corta" %in% names(final)) {
    final %>% filter(!is.na(año_corta)) %>% nrow()
  } else {
    0
  }
  
  # Crear tabla de balance
  balance <- tibble(
    categoria = c("Inicial", "Reclutamiento", "Mortalidad", "Cortas", "Final"),
    n_arboles = c(n_inicial, reclutas, -muertos, -cortados, n_final),
    tipo = c("stock", "entrada", "salida", "salida", "stock"),
    color = c("blue", "green", "red", "orange", "blue")
  )
  
  # Agregar verificación
  balance_calculado <- n_inicial + reclutas - muertos - cortados
  
  if (abs(balance_calculado - n_final) > 1) {
    warning(sprintf("Balance no cuadra: calculado=%d, observado=%d", 
                   balance_calculado, n_final))
  }
  
  return(balance)
}

# ==============================================================================
# 4. RESUMEN PREDIO
# ==============================================================================

#' Calcular resumen general del predio
#'
#' @param arboles_df Data frame con árboles
#' @param inventario Lista con datos de inventario (F01-F06)
#' @param caracteristicas_df Data frame con características por rodal (opcional)
#' @param config Objeto CONFIG
#' @return Data frame con resumen del predio
calcular_resumen_predio <- function(arboles_df, 
                                   inventario = NULL,
                                   caracteristicas_df = NULL,
                                   config = CONFIG) {
  
  # Métricas básicas
  n_rodales <- n_distinct(arboles_df$rodal)
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_arboles_total <- nrow(vivos)
  
  # Superficie
  if (!is.null(inventario) && "F01" %in% names(inventario)) {
    superficie_total <- inventario$F01 %>%
      summarise(superficie_ha = sum(superficie, na.rm = TRUE)) %>%
      pull(superficie_ha)
  } else {
    # Inferir de datos
    superficie_total <- n_rodales * 10  # Asumir 10 ha por rodal si no hay datos
  }
  
  # Características topográficas
  if (!is.null(caracteristicas_df)) {
    topo <- caracteristicas_df %>%
      summarise(
        pendiente_prom = mean(pendiente_prom, na.rm = TRUE),
        altitud_media = mean(altitud_media, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Exposición más frecuente
    exposicion_prom <- caracteristicas_df %>%
      count(exposicion, sort = TRUE) %>%
      slice(1) %>%
      pull(exposicion)
    
  } else {
    topo <- tibble(pendiente_prom = NA_real_, altitud_media = NA_real_)
    exposicion_prom <- "No disponible"
  }
  
  # Construir resumen
  resumen <- tibble(
    n_rodales = n_rodales,
    superficie_ha = superficie_total,
    superficie_por_rodal_ha = superficie_total / n_rodales,
    n_sitios_muestreo = n_sitios,
    n_arboles_vivos = n_arboles_total,
    pendiente_promedio = topo$pendiente_prom,
    altitud_media_m = topo$altitud_media,
    exposicion_predominante = exposicion_prom
  )
  
  return(resumen)
}

# ==============================================================================
# 5. COMPOSICIÓN FLORÍSTICA CON SD INTER-RODAL
# ==============================================================================

#' Calcular composición florística con desviación estándar entre rodales
#'
#' @param arboles_df Data frame con árboles
#' @param config Objeto CONFIG
#' @return Data frame con métricas promedio ± SD inter-rodal
calcular_composicion_floristica <- function(arboles_df, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  # Calcular métricas por rodal primero
  metricas_rodal <- vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      h_media = mean(altura_total, na.rm = TRUE),
      ab_total = sum(calcular_area_basal(diametro_normal)),
      vol_total = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_ha = expandir_a_hectarea(n_arboles / n_sitios, config$area_parcela_ha),
      ab_ha = expandir_a_hectarea(ab_total / n_sitios, config$area_parcela_ha),
      vol_ha = expandir_a_hectarea(vol_total / n_sitios, config$area_parcela_ha)
    )
  
  # Agregar a nivel predio con SD
  composicion <- metricas_rodal %>%
    group_by(genero_grupo) %>%
    summarise(
      n_ha_media = mean(n_ha, na.rm = TRUE),
      n_ha_sd = sd(n_ha, na.rm = TRUE),
      ab_ha_media = mean(ab_ha, na.rm = TRUE),
      ab_ha_sd = sd(ab_ha, na.rm = TRUE),
      vol_ha_media = mean(vol_ha, na.rm = TRUE),
      vol_ha_sd = sd(vol_ha, na.rm = TRUE),
      dmc_media = mean(d_medio, na.rm = TRUE),
      dmc_sd = sd(d_medio, na.rm = TRUE),
      altura_media = mean(h_media, na.rm = TRUE),
      altura_sd = sd(h_media, na.rm = TRUE),
      n_rodales = n(),
      .groups = "drop"
    )
  
  return(composicion)
}

# ==============================================================================
# 6. RANKING Y CLASIFICACIÓN DE PRODUCTIVIDAD
# ==============================================================================

#' Calcular ranking de productividad por rodal
#'
#' @param metricas_rodal Data frame con métricas por rodal
#' @param criterios Vector con columnas para ranking (default: c("vol_ha", "ica"))
#' @param umbrales Lista con umbrales para clasificación
#' @return Data frame con ranking y clasificación
calcular_ranking_productividad <- function(metricas_rodal,
                                          criterios = c("vol_ha_m3"),
                                          umbrales = NULL) {
  
  # Umbrales por defecto
  if (is.null(umbrales)) {
    umbrales <- list(
      alta = 150,    # Vol/ha > 150 m³
      media = 100    # Vol/ha 100-150 m³
    )
  }
  
  # Calcular score compuesto (promedio de percentiles)
  ranking <- metricas_rodal
  
  for (criterio in criterios) {
    if (criterio %in% names(metricas_rodal)) {
      col_percentil <- paste0(criterio, "_percentil")
      ranking[[col_percentil]] <- percent_rank(ranking[[criterio]]) * 100
    }
  }
  
  # Score compuesto (promedio de percentiles)
  cols_percentil <- paste0(criterios, "_percentil")
  cols_percentil <- cols_percentil[cols_percentil %in% names(ranking)]
  
  ranking <- ranking %>%
    mutate(
      score = rowMeans(select(., all_of(cols_percentil)), na.rm = TRUE),
      ranking = rank(-score, ties.method = "min")
    )
  
  # Clasificar según volumen
  if ("vol_ha_m3" %in% names(ranking)) {
    ranking <- ranking %>%
      mutate(
        clasificacion = case_when(
          vol_ha_m3 >= umbrales$alta ~ "Alta",
          vol_ha_m3 >= umbrales$media ~ "Media",
          TRUE ~ "Baja"
        ),
        clasificacion = factor(clasificacion, levels = c("Alta", "Media", "Baja"))
      )
  }
  
  # Ordenar por ranking
  ranking <- ranking %>%
    arrange(ranking)
  
  return(ranking)
}

# ==============================================================================
# 7. ANÁLISIS DE REGENERACIÓN
# ==============================================================================

#' Analizar regeneración desde hoja F05
#'
#' @param inventario Lista con datos de inventario
#' @param config Objeto CONFIG
#' @param diametro_max Diámetro máximo para considerar regeneración (default: 7.5cm)
#' @return Data frame con densidad de plántulas por rodal y género
calcular_regeneracion <- function(inventario, 
                                  config = CONFIG,
                                  diametro_max = 7.5) {
  
  if (!"F05" %in% names(inventario)) {
    warning("Hoja F05 (regeneración) no encontrada en inventario")
    return(NULL)
  }
  
  # Leer F05
  regeneracion <- inventario$F05 %>%
    filter(!is.na(genero), diametro_normal < diametro_max)
  
  if (nrow(regeneracion) == 0) {
    warning("No hay datos de regeneración en F05")
    return(NULL)
  }
  
  # Calcular densidad por rodal y género
  n_sitios_rodal <- regeneracion %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  densidad_regen <- regeneracion %>%
    group_by(rodal, genero) %>%
    summarise(
      n_plantulas_obs = n(),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(n_sitios_rodal, by = "rodal") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(
        n_plantulas_obs / n_sitios,
        config$area_parcela_ha
      )
    ) %>%
    select(rodal, genero, n_plantulas_obs, densidad_ha, d_medio_cm)
  
  return(densidad_regen)
}

# ==============================================================================
# 8. ANÁLISIS DE CARACTERÍSTICAS BIOFÍSICAS
# ==============================================================================

#' Analizar erosión por rodal
#'
#' @param inventario Lista con datos de inventario (F02)
#' @return Data frame con porcentaje de sitios por categoría de erosión
calcular_erosion <- function(inventario) {
  
  if (!"F02" %in% names(inventario)) {
    warning("Hoja F02 no encontrada en inventario")
    return(NULL)
  }
  
  erosion <- inventario$F02 %>%
    filter(!is.na(erosion)) %>%
    mutate(
      categoria_erosion = case_when(
        erosion == 0 ~ "Nula",
        erosion == 1 ~ "Leve",
        erosion == 2 ~ "Moderada",
        erosion == 3 ~ "Severa",
        TRUE ~ "Desconocida"
      )
    )
  
  # Calcular porcentaje por rodal
  erosion_resumen <- erosion %>%
    group_by(rodal) %>%
    mutate(n_sitios_rodal = n()) %>%
    group_by(rodal, categoria_erosion, n_sitios_rodal) %>%
    summarise(n_sitios = n(), .groups = "drop") %>%
    mutate(pct_sitios = (n_sitios / n_sitios_rodal) * 100) %>%
    select(rodal, categoria_erosion, n_sitios, pct_sitios)
  
  return(erosion_resumen)
}

#' Analizar sanidad de árboles
#'
#' @param arboles_df Data frame con árboles
#' @return Data frame con distribución por categoría de sanidad
calcular_sanidad <- function(arboles_df) {
  
  if (!"sanidad" %in% names(arboles_df)) {
    warning("Columna 'sanidad' no encontrada")
    return(NULL)
  }
  
  # Solo árboles vivos
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  sanidad <- vivos %>%
    mutate(
      categoria_sanidad = case_when(
        sanidad == 1 ~ "Sano",
        sanidad == 2 ~ "Enfermo leve",
        sanidad == 3 ~ "Enfermo moderado",
        sanidad == 4 ~ "Enfermo severo",
        sanidad == 5 ~ "Plaga",
        TRUE ~ "Desconocido"
      )
    )
  
  # Resumen por rodal
  sanidad_resumen <- sanidad %>%
    group_by(rodal, categoria_sanidad) %>%
    summarise(n_arboles = n(), .groups = "drop") %>%
    group_by(rodal) %>%
    mutate(
      total_rodal = sum(n_arboles),
      pct = (n_arboles / total_rodal) * 100
    ) %>%
    ungroup()
  
  return(sanidad_resumen)
}

#' Interpretar uso pecuario
#'
#' @param inventario Lista con datos de inventario (F01)
#' @return Data frame con interpretación de uso pecuario por rodal
calcular_uso_pecuario <- function(inventario) {
  
  if (!"F01" %in% names(inventario)) {
    warning("Hoja F01 no encontrada")
    return(NULL)
  }
  
  if (!"uso_pecuario" %in% names(inventario$F01)) {
    warning("Columna 'uso_pecuario' no encontrada en F01")
    return(NULL)
  }
  
  uso <- inventario$F01 %>%
    filter(!is.na(uso_pecuario)) %>%
    mutate(
      intensidad = case_when(
        uso_pecuario == 0 ~ "Sin uso",
        uso_pecuario == 1 ~ "Bajo",
        uso_pecuario == 2 ~ "Moderado",
        uso_pecuario == 3 ~ "Alto",
        TRUE ~ "Desconocido"
      )
    ) %>%
    select(rodal, uso_pecuario, intensidad)
  
  return(uso)
}

# ==============================================================================
# 9. SOSTENIBILIDAD
# ==============================================================================

#' Evaluar sostenibilidad del aprovechamiento
#'
#' @param ica_rodal Data frame con ICA por rodal
#' @param vol_aprovechado Data frame con volumen aprovechado por rodal
#' @param periodo Periodo en años
#' @return Data frame con evaluación de sostenibilidad
evaluar_sostenibilidad <- function(ica_rodal, vol_aprovechado, periodo = 10) {
  
  # Unir ICA con volumen aprovechado
  sostenibilidad <- ica_rodal %>%
    left_join(vol_aprovechado, by = "rodal") %>%
    mutate(
      # ICA anual vs aprovechamiento anual
      vol_aprovechado_anual = vol_cortado_m3 / periodo,
      balance_m3_año = ica_m3_ha_año - vol_aprovechado_anual,
      
      # Clasificación
      sostenible = case_when(
        balance_m3_año >= 0 ~ "Sí",
        balance_m3_año >= -5 ~ "Límite",
        TRUE ~ "No"
      ),
      
      # Porcentaje del ICA aprovechado
      pct_ica_aprovechado = (vol_aprovechado_anual / ica_m3_ha_año) * 100
    )
  
  return(sostenibilidad)
}

# ==============================================================================
# INICIALIZACIÓN
# ==============================================================================

cat("\n✓ Módulo de cálculos específicos cargado\n")
cat("  Categorías disponibles:\n")
cat("    1. Diversidad (Shannon, Simpson, riqueza)\n")
cat("    2. Mortalidad (muertos, tocones)\n")
cat("    3. Demografía (balance)\n")
cat("    4. Resumen predio\n")
cat("    5. Productividad (ranking)\n")
cat("    6. Regeneración\n")
cat("    7. Biofísica (erosión, sanidad, uso pecuario)\n")
cat("    8. Sostenibilidad\n\n")
