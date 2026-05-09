# ==============================================================================
# 05_CONFIG_PROGRAMA_CORTAS.R - CON N_REF ARBITRARIO POR UMM
# ==============================================================================

library(tidyverse)

cat("\n[4/4] Cargando programa de cortas...\n")

# ==============================================================================
# 1. DIÁMETROS MÍNIMOS DE CORTA (DMC) por género
# ==============================================================================

DMC <- list(
  "Pinus" = 20,
  "Quercus" = 2
)

# ==============================================================================
# 2. DIÁMETROS DE MADUREZ por género (para cosecha prioritaria)
# ==============================================================================

D_MADUREZ <- list(
  "Pinus" = 55,
  "Quercus" = 45
)

# ==============================================================================
# 3. PARÁMETROS LIOCOURT
# ==============================================================================

Q_FACTOR <- 1.5
TOLERANCIA_EQUILIBRIO <- 20
CLASE_REFERENCIA_LIOCOURT <- 40  # cm (centro de clase 40-45)

# ==============================================================================
# 4. DENSIDAD REFERENCIA LIOCOURT POR UMM (ARBITRARIA)
# ==============================================================================
#
# N_REF_LIOCOURT: Densidad objetivo en la clase de referencia (40-45 cm)
# Unidad: árboles/ha (Pinus + Quercus combinados)
#
# DEFINICIÓN ARBITRARIA POR UMM:
#   - UMM productivas con alta densidad: 10-15 árb/ha
#   - UMM promedio: 8-12 árb/ha
#   - UMM degradadas o baja densidad: 5-8 árb/ha
#
# IMPORTANTE: Este valor define la CURVA IDEAL hacia la cual se dirige
# el manejo forestal, NO la densidad actual observada en campo.
#
# ==============================================================================

N_REF_LIOCOURT_POR_UMM <- tribble(
  ~rodal, ~n_ref_arboles_ha,
  #-------|-------------------
  1,      10,   # UMM 1: alta densidad objetivo
  2,      10,   # UMM 2: densidad media-alta
  3,      10,    # UMM 3: densidad media
  4,      10,   # UMM 4: densidad media-alta
  5,      10,    # UMM 5: densidad media
  6,      10    # UMM 6: densidad alta
)

# ==============================================================================
# 5. PROGRAMA DE CORTAS
# ==============================================================================
#
# COLUMNAS:
#   rodal: Número de rodal (UMM)
#   ano_corta: Año del ciclo para intervención (1-10)
#   metodo: "ICA" (único método implementado)
#   intensidad_pct: % del ICA a cortar (típicamente 50-80%)
#   proteger_maduros_pinus: TRUE = NO cortar Pinus maduros (>= D_MADUREZ)
#   proteger_maduros_quercus: TRUE = NO cortar Quercus maduros (>= D_MADUREZ)
#   proporcion_quercus: Proporción objetivo de Quercus en volumen cortado
#                       (0.60 = 60% Quercus, 40% Pinus)
#                       NULL = sin control de proporción
#
# LÓGICA:
#   1. Calcular vol_objetivo = ICA × 10 años × intensidad_pct
#   2. Cortar árboles maduros primero (si no protegidos)
#   3. Luego cortar según Liocourt (distance à courbe idéale)
#      → Curva Liocourt usa N_REF_LIOCOURT_POR_UMM[rodal]
#   4. Si proporcion_quercus definida: roll dice para auto-regular
#
# ==============================================================================

PROGRAMA_CORTAS <- tribble(
  ~rodal, ~ano_corta, ~metodo, ~intensidad_pct, ~proteger_maduros_pinus, ~proteger_maduros_quercus, ~proporcion_quercus,
  #-------|-----------|---------|----------------|-------------------------|---------------------------|---------------------
  1,      1,          "ICA",   70,               TRUE,                    FALSE,                     1,
  2,      3,          "ICA",   50,               TRUE,                    FALSE,                     1,
  3,      5,          "ICA",   80,               TRUE,                    FALSE,                     0.50,
  4,      7,          "ICA",   70,               TRUE,                    FALSE,                     0.90,
  5,      9,          "ICA",   70,               TRUE,                    FALSE,                     0.90,
  6,      10,         "ICA",   65,               TRUE,                    FALSE,                     0.90
)

# ==============================================================================
# 6. FUNCIÓN HELPER PARA OBTENER N_REF POR UMM
# ==============================================================================

#' @title Obtener N_REF Liocourt para un rodal
#' @param rodal Número de UMM
#' @return Densidad objetivo en clase referencia (árb/ha)
obtener_n_ref_liocourt <- function(rodal) {
  n_ref <- N_REF_LIOCOURT_POR_UMM %>%
    filter(rodal == !!rodal) %>%
    pull(n_ref_arboles_ha)
  
  if (length(n_ref) == 0) {
    warning(sprintf("⚠️ N_REF no definido para UMM %d, usando 10 árb/ha por defecto", rodal))
    return(10)
  }
  
  return(n_ref)
}

# ==============================================================================
# 7. FUNCIÓN HELPER PARA CONFIGURAR CORTE
# ==============================================================================

configurar_corte <- function(metodo = "ICA",
                             intensidad_pct = 50,
                             proteger_maduros_pinus = TRUE,
                             proteger_maduros_quercus = FALSE,
                             proporcion_quercus = 0.90,
                             q_factor = Q_FACTOR,
                             tolerancia = TOLERANCIA_EQUILIBRIO) {
  
  if (metodo != "ICA") {
    stop("❌ Solo método 'ICA' está implementado")
  }
  
  list(
    metodo = metodo,
    intensidad_pct = intensidad_pct,
    proteger_maduros_pinus = proteger_maduros_pinus,
    proteger_maduros_quercus = proteger_maduros_quercus,
    proporcion_quercus = proporcion_quercus,
    q_factor = q_factor,
    tolerancia = tolerancia
  )
}

# ==============================================================================
# VALIDACIÓN
# ==============================================================================

cat("  ✓ DMC: Pinus =", DMC$Pinus, "cm, Quercus =", DMC$Quercus, "cm\n")
cat("  ✓ D_MADUREZ: Pinus =", D_MADUREZ$Pinus, "cm, Quercus =", D_MADUREZ$Quercus, "cm\n")
cat("  ✓ Q-factor:", Q_FACTOR, "\n")
cat("  ✓ Clase referencia:", CLASE_REFERENCIA_LIOCOURT, "cm\n")
cat("  ✓ N_REF arbitrario definido para", nrow(N_REF_LIOCOURT_POR_UMM), "UMM\n")
cat("  ✓ Programa de cortas:", nrow(PROGRAMA_CORTAS), "intervenciones\n\n")