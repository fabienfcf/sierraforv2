# ==============================================================================
# 01_CONFIG_ESPECIES.R
# Información biológica y dasométrica por especie
# ==============================================================================

library(tidyverse)

cat("\n[1/4] Cargando configuración de especies...\n")

# ==============================================================================
# 1. CATÁLOGO DE ESPECIES
# ==============================================================================

ESPECIES <- tribble(
  ~codigo, ~nombre_cientifico,      ~genero,           ~grupo,          ~maderable,
  10,      "Pinus sp.",             "Pinus",           "Coníferas",     "SI",
  11,      "Pinus pseudostrobus",   "Pinus",           "Coníferas",     "SI",
  12,      "Pinus teocote",         "Pinus",           "Coníferas",     "SI",
  13,      "Pinus cembroides",      "Pinus",           "Coníferas",     "NO",
  20,      "Juniperus sp",          "Juniperus",       "Coníferas",     "NO",
  21,      "Juniperus flaccida",    "Juniperus",       "Coníferas",     "NO",
  30,      "Cupressus sp",          "Cupressus",       "Coníferas",     "NO",
  40,      "Pseudotsuga sp",        "Pseudotsuga",     "Coníferas",     "NO",
  50,      "otras coniferas",       "otras coniferas", "Coníferas",     "NO",
  61,      "Quercus rysophylla",    "Quercus",         "Latifoliadas",  "SI",
  62,      "Quercus laeta",         "Quercus",         "Latifoliadas",  "SI",
  63,      "Quercus affinis",       "Quercus",         "Latifoliadas",  "SI",
  64,      "Quercus laceyi",        "Quercus",         "Latifoliadas",  "SI",
  70,      "Alnus sp",              "Alnus",           "Latifoliadas",  "NO",
  81,      "Arbutus xalapensis",    "Arbutus",         "Latifoliadas",  "NO",
  90,      "Fraxinus sp",           "Fraxinus",        "Latifoliadas",  "NO",
  101,     "Crataegus mexicana",    "Crataegus",       "Latifoliadas",  "NO",
  68,       "Amelanchier denticulata", "Amelanchier",        "Latifoliadas",  "NO",
  67, "Cercis canadensis", "Cercis",        "Latifoliadas",  "NO",
  65, "Juglans sp", "Juglans",        "Latifoliadas",  "NO",
  66, "Carya sp", "Carya",        "Latifoliadas",  "NO"
  
)

# ==============================================================================
# 2. ECUACIONES ALOMÉTRICAS DE VOLUMEN
# ==============================================================================

ECUACIONES_VOLUMEN <- tribble(
  ~nombre_cientifico,    ~tipo,       ~a,            ~b,          ~c,          ~fuente,
  "Quercus affinis",     "potencia",  0.00006,       1.85604,     0.97898,     "SIPLAFOR",
  "Quercus laceyi",      "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Quercus rysophylla",  "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Quercus laeta",       "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Pinus cembroides",    "exp",       -9.8207876,    1.89180185,  1.08048365,  "1er INFYS",
  "Pinus pseudostrobus", "potencia",  0.00004,       1.93694,     1.03169,     "SIPLAFOR",
  "Pinus teocote",       "exp",       -8.72641434,   1.43032994,  1.19541675,  "1er INFYS",
  "Juniperus flaccida",  "potencia",  0.00013,       1.77192,     0.73487,     "SIPLAFOR",
  "Arbutus xalapensis",  "potencia",  0.00006,       1.82154,     0.96124,     "SIPLAFOR",
  "Fraxinus sp",         "exp",       -9.80434696,   1.91033696,  1.03262007,  "1er INFYS",
  "Carya sp",         "exp",       -9.98222558,   1.94239763,  1.02228707,  "1er INFYS",
  "Crataegus mexicana",     "potencia",  0.00006,       1.98226,     0.84224,     "SIPLAFOR",
  "Juglans sp",    "exp",       -9.82944377,    1.9060093,  1.04047533,  "1er INFYS"
  
)

# ==============================================================================
# 3. TASAS DE CRECIMIENTO DIAMÉTRICO BASE (cm/año)
# ==============================================================================

CRECIMIENTO_DIAMETRICO <- tribble(
  ~genero,   ~tasa_base_cm_año, ~tasa_altura_m_año, ~altura_max_m, ~fuente,
  "Pinus",   0.40,              0.20,               22,            "Observaciones campo + literatura regional",
  "Quercus", 0.25,              0.15,               17,            "Observaciones campo + literatura regional"
)

# Valores por defecto para géneros no catalogados
CRECIMIENTO_D_DEFAULT <- 0.30  # cm/año
CRECIMIENTO_H_DEFAULT <- 0.15  # m/año

# ==============================================================================
# 5. FUNCIONES DE ACCESO RÁPIDO
# ==============================================================================

#' @title Obtener ecuación de volumen para una especie
#' @param nombre_cientifico Nombre científico de la especie
#' @return Tibble con parámetros (tipo, a, b, c, fuente) o NULL
#' @examples
#' obtener_ecuacion_volumen("Pinus pseudostrobus")
obtener_ecuacion_volumen <- function(nombre_cientifico) {
  resultado <- ECUACIONES_VOLUMEN %>%
    filter(nombre_cientifico == !!nombre_cientifico)

  if (nrow(resultado) == 0) {
    warning(sprintf("No se encontró ecuación para '%s'", nombre_cientifico))
    return(NULL)
  }

  return(resultado)
}

#' @title Obtener tasa de crecimiento diamétrico para un género
#' @param genero "Pinus" o "Quercus"
#' @return Tasa en cm/año
#' @examples
#' obtener_tasa_crecimiento("Pinus")
obtener_tasa_crecimiento <- function(genero) {
  tasa <- CRECIMIENTO_DIAMETRICO %>%
    filter(genero == !!genero) %>%
    pull(tasa_base_cm_año)
  
  if (length(tasa) == 0) {
    warning(sprintf("Género '%s' no encontrado, usando 0.30 cm/año por defecto", genero))
    return(0.30)
  }
  
  return(tasa)
}

# ==============================================================================
# RESUMEN DE CARGA
# ==============================================================================

cat(sprintf("  ✓ Especies catalogadas:      %d\n", nrow(ESPECIES)))
cat(sprintf("  ✓ Ecuaciones de volumen:     %d\n", nrow(ECUACIONES_VOLUMEN)))
cat(sprintf("  ✓ Géneros con tasa de crec.: %d\n", nrow(CRECIMIENTO_DIAMETRICO)))
cat("  ✓ Funciones disponibles:\n")
cat("      obtener_ecuacion_volumen(especie)\n")
cat("      obtener_tasa_crecimiento(genero)\n")