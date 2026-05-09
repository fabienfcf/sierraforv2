# ==============================================================================
# 02_CONFIG_CODIGOS.R
# Códigos de interpretación del formato SIPLAFOR
# ==============================================================================

library(tidyverse)

cat("\n[2/4] Cargando códigos SIPLAFOR...\n")

# ==============================================================================
# CÓDIGOS DE SITIO
# ==============================================================================

CODIGOS_EXPOSICION <- tibble(
  codigo = 1:9,
  etiqueta = c("Cénit", "Norte", "Noreste", "Este", "Sureste", 
               "Sur", "Suroeste", "Oeste", "Noroeste")
)

CODIGOS_COMPACTACION <- tibble(
  codigo = 1:3,
  etiqueta = c("Alta", "Media", "Baja")
)

CODIGOS_TEXTURA <- tibble(
  codigo = 1:13,
  etiqueta = c("Limosa", "Arenosa", "Arcillosa", "Limo-arenosa", "Limo-arcillosa",
               "Areno-limosa", "Areno-arcillosa", "Arcillo-limosa", "Arcillo-arenosa",
               "Franca", "Franco-limosa", "Franco-arenosa", "Franco-arcillosa")
)

CODIGOS_MATERIAL <- tibble(
  codigo = 1:6,
  etiqueta = c("Suelo", "Arena", "Grava", "Piedra", "Roca", "Laja")
)

# ==============================================================================
# CÓDIGOS DE USO Y MANEJO
# ==============================================================================

CODIGOS_USO_SUELO <- tibble(
  codigo = 1:19,
  etiqueta = c("Forestal producción", "Forestal protección", "Franjas cauces agua",
               "Franjas vías comunicación", "Fauna silvestre", "Vegetación", "Recreación",
               "Suelo desnudo", "Bajas existencias", "Forestal inaccesible", "Agricultura",
               "Pastizal", "Minería", "Vías comunicación", "Rocoso", 
               "Erosión crítica", "Asentamientos humanos", "Zonas industriales", "Investigación")
)

CODIGOS_INTENSIDAD <- tibble(
  codigo = 1:4,
  etiqueta = c("Nula", "Baja", "Moderada", "Intensa")
)

# ==============================================================================
# CÓDIGOS DE EROSIÓN
# ==============================================================================

CODIGOS_EROSION <- tibble(
  codigo = 1:9,
  etiqueta = c("Sin afectación", "1-10%", "11-20%", "21-30%", "31-40%", 
               "41-50%", "51-60%", "61-70%", ">70%")
)

CODIGOS_ACCESIBILIDAD <- tibble(
  codigo = 1:3,
  etiqueta = c("Buena", "Regular", "Mala")
)

# ==============================================================================
# CÓDIGOS DE PERTURBACIONES
# ==============================================================================

CODIGOS_PERTURBACIONES <- tibble(
  codigo = 1:15,
  etiqueta = c("Sin perturbación", "Hongos/enfermedades", "Plagas", "Aprovechamiento ilegal",
               "Anillado", "Resinado", "Incendios", "Pastoreo", "Extracción de leña",
               "Plantas parásitas", "Lianas", "Roedores", "Rayos", "Viento", "Otras")
)

# ==============================================================================
# CÓDIGOS DE TRATAMIENTOS
# ==============================================================================

CODIGOS_TRATAMIENTO <- tibble(
  codigo = 1:10,
  etiqueta = c("No corta", "Corta selección", "Corta regeneración", 
               "Corta liberación pre-clareo", "Primer clareo", 
               "Segundo clareo", "Tercer clareo", "Cuarto clareo",
               "Corta rasa con plantación", "Corta protección")
)

CODIGOS_TRATAMIENTO_COMP <- tibble(
  codigo = 1:13,
  etiqueta = c("Quema controlada", "Desbroce", "Limpieza suelo", 
               "Reducción densidad regeneración", "Reforestación", "Plantación directa",
               "Limpieza regeneración", "Restauración suelos", "Cortas sanitarias",
               "Control azolves", "Brecha cortafuego", "Cercado", "Podas")
)

# ==============================================================================
# CÓDIGOS DE ÁRBOLES
# ==============================================================================

CODIGOS_DOMINANCIA <- tribble(
  ~codigo, ~etiqueta,                  ~factor_crecimiento, ~factor_crecimiento_h, ~factor_mortalidad,
  1,       "Dominante",                1.00,                1.00,                  1.0,
  2,       "Intermedio",               0.80,                1.00,                  1.0,
  3,       "Suprimido",                0.60,                0.75,                  1.2,
  4,       "Libre sin supresión",      1.00,                1.00,                  1.0,
  5,       "Libre con supresión",      0.70,                0.75,                  1.2,
  6,       "Aislado con piso alto",    0.50,                0.50,                  1.5,
  7,       "Muerto en pie",            0.00,                0.00,                  NA,
  8,       "Muerto caído",             0.00,                0.00,                  NA,
  9,       "Tocón",                    0.00,                0.00,                  NA
)

CODIGOS_DANOS <- tibble(
  codigo = 1:10,
  etiqueta = c("Sin daño", "Viejo/resinado", "Fuste nudoso", "Ladeado/torcido", 
               "Descortezado", "Puntiseco", "Anillado", "Fuste ovoide", 
               "Daño por cable", "Bifurcado")
)

CODIGOS_UBICACION_DANO <- tibble(
  codigo = 1:8,
  etiqueta = c("Sin daño", "Punta", "Parte media", "Base", "Punta+media",
               "Punta+base", "Media+base", "Completo")
)

CODIGOS_SANIDAD <- tibble(
  codigo = 1:6,
  etiqueta = c("Sano", "Muérdago", "Barrenadores yemas", "Descortezadores", 
               "Defoliadores", "Paxtle")
)

CODIGOS_INTENSIDAD_INFESTACION <- tibble(
  codigo = 1:4,
  etiqueta = c("Sin afectación", "Baja (<33%)", "Moderada (33-66%)", "Alta (>66%)")
)

# ==============================================================================
# CÓDIGOS DE REGENERACIÓN
# ==============================================================================

CODIGOS_DISTRIBUCION <- tibble(
  codigo = 1:3,
  etiqueta = c("Uniforme", "Aleatoria", "En manchones")
)

CODIGOS_DEGRADACION <- tibble(
  codigo = 1:5,
  etiqueta = c("Corteza intacta", 
               "Parte de la corteza ya no presente", 
               "Mayoría de la corteza ya no presente",
               "Madera suena a hueco cuando se golpea", 
               "Fácil de desintegrar cuando se golpea con el pie")
)

# ==============================================================================
# FUNCIONES HELPER
# ==============================================================================

#' @title Traducir código a etiqueta
#' @param codigo Código numérico
#' @param tipo Tipo de código ("exposicion", "dominancia", etc.)
#' @return Etiqueta correspondiente o NA
traducir_codigo <- function(codigo, tipo) {
  
  if (is.na(codigo)) return(NA_character_)
  
  tabla <- switch(
    tipo,
    "exposicion" = CODIGOS_EXPOSICION,
    "compactacion" = CODIGOS_COMPACTACION,
    "textura" = CODIGOS_TEXTURA,
    "material" = CODIGOS_MATERIAL,
    "uso_suelo" = CODIGOS_USO_SUELO,
    "intensidad" = CODIGOS_INTENSIDAD,
    "erosion" = CODIGOS_EROSION,
    "accesibilidad" = CODIGOS_ACCESIBILIDAD,
    "perturbaciones" = CODIGOS_PERTURBACIONES,
    "tratamiento" = CODIGOS_TRATAMIENTO,
    "tratamiento_comp" = CODIGOS_TRATAMIENTO_COMP,
    "dominancia" = CODIGOS_DOMINANCIA,
    "danos" = CODIGOS_DANOS,
    "ubicacion_dano" = CODIGOS_UBICACION_DANO,
    "sanidad" = CODIGOS_SANIDAD,
    "intensidad_infestacion" = CODIGOS_INTENSIDAD_INFESTACION,
    "distribucion" = CODIGOS_DISTRIBUCION,
    "degradacion" = CODIGOS_DEGRADACION,
    stop(sprintf("Tipo de código '%s' desconocido", tipo))
  )
  
  resultado <- tabla %>%
    filter(codigo == !!codigo) %>%
    pull(etiqueta)
  
  if (length(resultado) == 0) {
    warning(sprintf("Código %d no encontrado en tipo '%s'", codigo, tipo))
    return(NA_character_)
  }
  
  return(resultado)
}

#' @title Traducir múltiples códigos (vectorizado)
#' @param codigos Vector de códigos
#' @param tipo Tipo de código
#' @return Vector de etiquetas
traducir_codigos <- function(codigos, tipo) {
  sapply(codigos, function(x) traducir_codigo(x, tipo))
}

# ==============================================================================
# RESUMEN DE CARGA
# ==============================================================================

cat(sprintf("  ✓ Tablas de códigos cargadas: 18\n"))
cat("  ✓ Funciones disponibles:\n")
cat("      traducir_codigo(codigo, tipo)\n")
cat("      traducir_codigos(vector_codigos, tipo)\n")
cat("\n  Tipos disponibles:\n")
cat("      exposicion, compactacion, textura, material,\n")
cat("      uso_suelo, intensidad, erosion, accesibilidad,\n")
cat("      perturbaciones, tratamiento, dominancia, danos,\n")
cat("      sanidad, distribucion, degradacion\n")