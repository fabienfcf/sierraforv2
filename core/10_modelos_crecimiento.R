# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO 2: MODELOS DE CRECIMIENTO INDIVIDUAL - VERSIÓN FINAL
# Compatible con la signature exacta de calcular_volumen_arbol
# ==============================================================================

library(tidyverse)

# Cargar dependencias si no están en memoria
if (!exists("filtrar_arboles_vivos")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}
if (!exists("validar_crecimiento")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_validacion.R"))
}

# ==============================================================================
# 1. CÁLCULO DE INCREMENTO DIAMÉTRICO
# ==============================================================================

calcular_incremento_diametro <- function(arbol, config) {
  genero <- arbol$genero_grupo
  dominancia <- arbol$dominancia
  
  # Validar que el árbol esté vivo
  if (!es_arbol_vivo(dominancia)) {
    return(0)  # Árboles muertos no crecen
  }
  
  # 1. Obtener tasa base de crecimiento por género
  tasa_base <- config$crecimiento_base[[genero]]
  
  if (is.null(tasa_base)) {
    warning(sprintf("Género '%s' no encontrado en config. Usando %.2f cm/año por defecto.", genero, config$crecimiento_d_default))
    tasa_base <- config$crecimiento_d_default
  }
  
  # 2. Aplicar modificador por dominancia - compatible avec codigo ou codigo_dom
  modificador_dom <- tryCatch({
    config$modificadores_dominancia %>%
      filter(codigo == dominancia) %>%
      pull(factor_crecimiento)
  }, error = function(e) {
    config$modificadores_dominancia %>%
      filter(codigo_dom == dominancia) %>%
      pull(factor_crecimiento)
  })
  
  if (length(modificador_dom) == 0) {
    warning(sprintf("Dominancia %d no encontrada. Usando factor 0.70 por defecto.", dominancia))
    modificador_dom <- 0.70
  }
  
  # 3. Cálculo final del incremento
  incremento_d <- tasa_base * modificador_dom
  
  return(incremento_d)
}

# ==============================================================================
# 2. CÁLCULO DE INCREMENTO EN ALTURA
# ==============================================================================
calcular_incremento_altura <- function(arbol, incremento_d, config) {
  
  # Validar que el árbol esté vivo
  if (!es_arbol_vivo(arbol$dominancia)) {
    return(0)
  }
  
  # Si no hubo incremento en diámetro, no hay en altura
  if (incremento_d <= 0) {
    return(0)
  }
  
  # LÍMITE DE ALTURA MÁXIMA
  # ══════════════════════════════════════════════════════════════

  h_max <- config$altura_maxima[[arbol$genero_grupo]]
  if (!is.null(h_max) && arbol$altura_total >= h_max) {
    return(0)
  }

  # TASA DE BASE EN ALTURA (m/año)
  # ═══════════════════════════════

  incremento_h_base <- config$crecimiento_altura[[arbol$genero_grupo]]
  if (is.null(incremento_h_base)) {
    incremento_h_base <- config$crecimiento_h_default
  }
  
  # MODIFICADOR POR DOMINANCIA (desde CODIGOS_DOMINANCIA)
  # ══════════════════════════════════════════════════════════════

  factor_dom <- config$modificadores_dominancia %>%
    filter(codigo == arbol$dominancia) %>%
    pull(factor_crecimiento_h)

  if (length(factor_dom) == 0) {
    factor_dom <- 1.0
  }
  
  # AJUSTEMENT PAR DIAMÈTRE
  # ══════════════════════════════════════════════════════════════
  
  factor_diametre <- case_when(
    arbol$diametro_normal < 20 ~ 1.3,                                    # Arboles jóvenes
    arbol$diametro_normal >= 20 & arbol$diametro_normal < 40 ~ 1.0,     # Medianos (corregido)
    arbol$diametro_normal >= 40 ~ 0.8,                                   # Grandes
    TRUE ~ 1.0
  )
  
  # CALCUL FINAL
  # ══════════════════════════════════════════════════════════════
  
  incremento_h <- incremento_h_base * factor_dom * factor_diametre  # (corregido: un seul *)
  
  return(incremento_h)
}

# ==============================================================================
# 3. FUNCIÓN PRINCIPAL: APLICAR CRECIMIENTO ANUAL
# ==============================================================================

aplicar_crecimiento_anual <- function(arbol, config) {
  
  # ✅ SIEMPRE inicializar campos de incremento
  arbol$incremento_d_cm <- 0
  arbol$incremento_h_m <- 0
  arbol$incremento_vol_m3 <- 0
  
  # Solo procesar árboles vivos
  if (!es_arbol_vivo(arbol$dominancia)) {
    return(arbol)  # Retornar con incrementos = 0
  }
  
  # 1. Calcular incrementos
  delta_d <- calcular_incremento_diametro(arbol, config)
  delta_h <- calcular_incremento_altura(arbol, delta_d, config)
  
  # 2. Nuevas dimensiones
  diametro_nuevo <- arbol$diametro_normal + delta_d
  altura_nueva <- arbol$altura_total + delta_h
  
  # 3. Recalcular volumen - USAR LA SIGNATURE CORRECTA
  volumen_nuevo <- calcular_volumen_arbol(
    d_cm = diametro_nuevo,  # ✅ Nom correct
    h_m = altura_nueva,       # ✅ Nom correct
    tipo = arbol$tipo,
    a = arbol$a,
    b = arbol$b,
    c = arbol$c
  )
  
  # 4. Recalcular área basal
  area_basal_nueva <- calcular_area_basal(diametro_nuevo)
  
  # 5. Guardar incrementos
  arbol$incremento_d_cm <- delta_d
  arbol$incremento_h_m <- delta_h
  arbol$incremento_vol_m3 <- if_else(
    is.na(volumen_nuevo) | is.na(arbol$volumen_m3),
    NA_real_,
    volumen_nuevo - arbol$volumen_m3
  )
  
  # 6. Actualizar valores
  arbol$diametro_normal <- diametro_nuevo
  arbol$altura_total <- altura_nueva
  arbol$volumen_m3 <- volumen_nuevo
  arbol$area_basal <- area_basal_nueva
  
  return(arbol)
}

# ==============================================================================
# 4. APLICAR CRECIMIENTO A POBLACIÓN
# ==============================================================================

aplicar_crecimiento_poblacion <- function(arboles_df, config, año_actual = NULL) {
  
  if (!is.null(año_actual)) {
    cat(sprintf("\n[AÑO %d] Aplicando crecimiento...\n", año_actual))
  } else {
    cat("\n[CRECIMIENTO] Aplicando incrementos...\n")
  }
  
  # Contar árboles vivos
  vivos_inicial <- filtrar_arboles_vivos(arboles_df)
  n_vivos_inicial <- nrow(vivos_inicial)
  cat(sprintf("  Árboles vivos: %d\n", n_vivos_inicial))
  
  # Contar muertos
  muertos_inicial <- filtrar_arboles_muertos(arboles_df)
  n_muertos_inicial <- nrow(muertos_inicial)
  if (n_muertos_inicial > 0) {
    cat(sprintf("  Árboles muertos (sin crecimiento): %d\n", n_muertos_inicial))
  }
  
  # Aplicar crecimiento a cada árbol
  arboles_crecidos <- arboles_df %>%
    rowwise() %>%
    mutate(
      arbol_actualizado = list(aplicar_crecimiento_anual(
        arbol = pick(everything()),
        config = config
      ))
    ) %>%
    ungroup() %>%
    # Desempaquetar resultados
    mutate(
      diametro_normal = map_dbl(arbol_actualizado, ~.x$diametro_normal),
      altura_total = map_dbl(arbol_actualizado, ~.x$altura_total),
      volumen_m3 = map_dbl(arbol_actualizado, ~.x$volumen_m3),
      area_basal = map_dbl(arbol_actualizado, ~.x$area_basal),
      incremento_d_cm = map_dbl(arbol_actualizado, ~.x$incremento_d_cm),
      incremento_h_m = map_dbl(arbol_actualizado, ~.x$incremento_h_m),
      incremento_vol_m3 = map_dbl(arbol_actualizado, ~.x$incremento_vol_m3)
    ) %>%
    select(-arbol_actualizado)
  
  # Estadísticas de crecimiento
  stats <- filtrar_arboles_vivos(arboles_crecidos) %>%
    summarise(
      delta_d_medio = mean(incremento_d_cm, na.rm = TRUE),
      delta_h_medio = mean(incremento_h_m, na.rm = TRUE),
      delta_vol_total = sum(incremento_vol_m3, na.rm = TRUE)
    )
  
  cat(sprintf("  Incremento medio diámetro: %.3f cm\n", stats$delta_d_medio))
  cat(sprintf("  Incremento medio altura:   %.3f m\n", stats$delta_h_medio))
  cat(sprintf("  Incremento total volumen:  %.2f m³\n", stats$delta_vol_total))
  
  return(arboles_crecidos)
}

# ==============================================================================
# 5. VALIDACIÓN (usa función compartida)
# ==============================================================================

# La función validar_crecimiento() ahora está en utils_validacion.R
# para evitar duplicación de código


# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de crecimiento cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_crecimiento_anual(arbol, config)\n")
cat("    - aplicar_crecimiento_poblacion(arboles_df, config, año)\n")
cat("    - validar_crecimiento(antes, despues) [en utils_validacion.R]\n\n")