# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO DE UTILIDADES: FUNCIONES DE VALIDACIÓN COMPARTIDAS
# Centraliza todas las funciones de validación para evitar duplicación
# ==============================================================================

library(tidyverse)

# ==============================================================================
# VALIDACIÓN DE CRECIMIENTO
# ==============================================================================

#' @title Validar crecimiento aplicado
#' @description Verifica que el crecimiento se aplicó correctamente
validar_crecimiento <- function(arboles_antes, arboles_despues, config = CONFIG) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           VALIDACIÓN DE CRECIMIENTO APLICADO              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")

  if (nrow(arboles_antes) != nrow(arboles_despues)) {
    warning("⚠ Número de árboles cambió durante crecimiento!")
  }

  # Cargar función de filtrado si no existe
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }

  vivos_antes <- filtrar_arboles_vivos(arboles_antes)
  vivos_despues <- filtrar_arboles_vivos(arboles_despues)

  diametros_decrecieron <- sum(
    vivos_despues$diametro_normal < vivos_antes$diametro_normal,
    na.rm = TRUE
  )

  if (diametros_decrecieron > 0) {
    warning(sprintf("⚠ %d árboles con diámetro decreciente!", diametros_decrecieron))
  } else {
    cat("✓ Todos los diámetros aumentaron correctamente\n")
  }

  # Verificar rangos de incremento
  incrementos <- vivos_despues %>%
    summarise(
      delta_d_min = min(incremento_d_cm, na.rm = TRUE),
      delta_d_max = max(incremento_d_cm, na.rm = TRUE),
      delta_h_min = min(incremento_h_m, na.rm = TRUE),
      delta_h_max = max(incremento_h_m, na.rm = TRUE)
    )

  cat(sprintf("✓ Rango incremento diámetro: [%.3f - %.3f] cm\n",
              incrementos$delta_d_min, incrementos$delta_d_max))
  cat(sprintf("✓ Rango incremento altura:   [%.3f - %.3f] m\n",
              incrementos$delta_h_min, incrementos$delta_h_max))

  if (incrementos$delta_d_max > config$umbral_delta_d_max) {
    warning(sprintf("⚠ Incremento diamétrico muy alto (>%.1f cm/año)", config$umbral_delta_d_max))
  }

  if (incrementos$delta_h_max > config$umbral_delta_h_max) {
    warning(sprintf("⚠ Incremento altura muy alto (>%.1f m/año)", config$umbral_delta_h_max))
  }

  # Verificar que muertos no crecieron
  muertos_despues <- filtrar_arboles_muertos(arboles_despues)
  if (nrow(muertos_despues) > 0) {
    incrementos_muertos <- sum(
      muertos_despues$incremento_d_cm > 0 |
        muertos_despues$incremento_h_m > 0,
      na.rm = TRUE
    )

    if (incrementos_muertos > 0) {
      warning(sprintf("⚠ %d árboles muertos con incremento!", incrementos_muertos))
    } else {
      cat("✓ Árboles muertos correctamente sin crecimiento\n")
    }
  }

  cat("\n✓ Validación completada\n\n")
  return(TRUE)
}

# ==============================================================================
# VALIDACIÓN DE MORTALIDAD
# ==============================================================================

#' @title Validar mortalidad aplicada
validar_mortalidad <- function(arboles_antes, arboles_despues) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           VALIDACIÓN DE MORTALIDAD APLICADA               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")

  # Verificar que no se perdieron árboles
  if (nrow(arboles_antes) != nrow(arboles_despues)) {
    warning("⚠ Número de árboles cambió durante aplicación de mortalidad!")
  }

  # Contar cambios
  vivos_antes <- sum(es_arbol_vivo(arboles_antes$dominancia))
  vivos_despues <- sum(es_arbol_vivo(arboles_despues$dominancia))
  muertos_nuevos <- vivos_antes - vivos_despues

  cat(sprintf("✓ Árboles vivos antes:    %d\n", vivos_antes))
  cat(sprintf("✓ Árboles vivos después:  %d\n", vivos_despues))
  cat(sprintf("✓ Mortalidad aplicada:    %d árboles (%.2f%%)\n",
              muertos_nuevos, (muertos_nuevos / vivos_antes) * 100))

  # Verificar que árboles muertos no crecieron
  arboles_verificar <- arboles_despues %>%
    filter(dominancia == 7, murio_este_año == TRUE)

  if (nrow(arboles_verificar) > 0) {
    if ("incremento_d_cm" %in% names(arboles_verificar)) {
      incrementos_invalidos <- sum(arboles_verificar$incremento_d_cm > 0, na.rm = TRUE)
      if (incrementos_invalidos > 0) {
        warning(sprintf("⚠ %d árboles muertos tienen incremento > 0!",
                        incrementos_invalidos))
      } else {
        cat("✓ Árboles muertos no tienen crecimiento\n")
      }
    }
  }

  cat("\n✓ Validación completada\n\n")

  return(TRUE)
}

# ==============================================================================
# VALIDACIÓN DE RECLUTAMIENTO
# ==============================================================================

#' @title Validar reclutamiento aplicado
validar_reclutamiento <- function(arboles_antes, arboles_despues, config = CONFIG) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         VALIDACIÓN DE RECLUTAMIENTO APLICADO              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")

  # Identificar reclutas
  if ("es_recluta" %in% names(arboles_despues)) {
    reclutas <- arboles_despues %>% filter(es_recluta == TRUE)
  } else {
    reclutas <- tibble()
  }

  n_reclutas <- nrow(reclutas)

  if (n_reclutas == 0) {
    cat("✓ No hubo reclutamiento este año\n")
    return(TRUE)
  }

  cat(sprintf("✓ Total reclutas: %d\n", n_reclutas))

  # Validar diámetros
  d_min <- min(reclutas$diametro_normal)
  d_max <- max(reclutas$diametro_normal)

  cat(sprintf("✓ Rango diámetros: [%.2f - %.2f] cm\n", d_min, d_max))

  if (d_min < config$reclut_d_min | d_max > config$reclut_d_max) {
    warning("⚠ Algunos diámetros fuera del rango esperado")
  }

  # Validar dominancia
  dominancias_unicas <- unique(reclutas$dominancia)
  if (length(dominancias_unicas) == 1 && dominancias_unicas[1] == config$reclut_dominancia) {
    cat(sprintf("✓ Todos los reclutas son suprimidos (dom = %d)\n",
                config$reclut_dominancia))
  } else {
    warning("⚠ Algunos reclutas tienen dominancia incorrecta")
  }

  # Validar géneros
  generos_reclutas <- unique(reclutas$genero_grupo)
  if (all(generos_reclutas %in% config$generos)) {
    cat(sprintf("✓ Géneros válidos: %s\n", paste(generos_reclutas, collapse = ", ")))
  } else {
    warning("⚠ Algunos reclutas tienen género no objetivo")
  }

  cat("\n✓ Validación completada\n\n")

  return(TRUE)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de validación compartida cargado\n")
cat("  Funciones disponibles:\n")
cat("    - validar_crecimiento(antes, despues)\n")
cat("    - validar_mortalidad(antes, despues)\n")
cat("    - validar_reclutamiento(antes, despues, config)\n\n")
