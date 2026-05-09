# ==============================================================================
# CORE_CALCULOS.R
# Funciones puras para cálculos individuales (sin efectos secundarios)
# Autor: Sistema de Gestión Forestal Dinámica
# Versión: 1.0 - Refactorización
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. FILTRADO DE ÁRBOLES
# ==============================================================================

#' @title Filtrar árboles vivos
#' @description Función única para filtrar árboles vivos (elimina muertos)
#' @param arboles_df Data frame con columna 'dominancia'
#' @return Data frame solo con árboles vivos (dominancia ∉ {7,8,9})
filtrar_arboles_vivos <- function(arboles_df) {
  arboles_df %>% filter(!dominancia %in% c(7, 8, 9))
}

#' @title Filtrar árboles muertos
#' @description Complemento de filtrar_arboles_vivos()
filtrar_arboles_muertos <- function(arboles_df) {
  arboles_df %>% filter(dominancia %in% c(7, 8, 9))
}

#' @title Verificar si árbol está vivo
#' @description Función helper para operaciones rowwise
es_arbol_vivo <- function(dominancia_codigo) {
  !dominancia_codigo %in% c(7, 8, 9)
}

# ==============================================================================
# 2. CÁLCULO DE VOLUMEN (CONSOLIDADO)
# ==============================================================================

#' @title Calcular volumen de árbol individual
#' @description Función ÚNICA para cálculo de volumen con ecuaciones alométricas
#' 
#' @param d_cm Diámetro normal en centímetros
#' @param h_m Altura total en metros
#' @param tipo Tipo de ecuación: "potencia" o "exp"
#' @param a Parámetro a de la ecuación
#' @param b Parámetro b de la ecuación
#' @param c Parámetro c de la ecuación
#' 
#' @details
#' Ecuaciones soportadas:
#' - "potencia": V = a × d^b × h^c
#' - "exp": V = exp(a + b×ln(d) + c×ln(h))
#' 
#' @return Volumen en m³, o NA si datos inválidos
#' 
#' @examples
#' calcular_volumen_arbol(30, 12, "potencia", 0.00004, 1.93694, 1.03169)
calcular_volumen_arbol <- function(d_cm, h_m, tipo, a, b, c) {
  
  # Validación de inputs
  if (any(is.na(c(d_cm, h_m, tipo, a, b, c)))) {
    return(NA_real_)
  }
  
  if (d_cm <= 0 || h_m <= 0) {
    return(NA_real_)
  }
  
  # Cálculo según tipo de ecuación
  volumen <- switch(
    tipo,
    
    "potencia" = {
      # V = a × d^b × h^c
      a * (d_cm ^ b) * (h_m ^ c)
    },
    
    "exp" = {
      # V = exp(a + b×ln(d) + c×ln(h))
      exp(a + b * log(d_cm) + c * log(h_m))
    },
    
    # Default: NA para tipos desconocidos
    NA_real_
  )
  
  # Validar resultado
  if (is.na(volumen) || !is.finite(volumen) || volumen < 0) {
    return(NA_real_)
  }
  
  return(volumen)
}

#' @title Calcular volumen vectorizado
#' @description Wrapper para aplicar a múltiples árboles
#' @note Usa pmap_dbl para eficiencia
calcular_volumenes_vectorizado <- function(arboles_df) {
  
  if (!all(c("diametro_normal", "altura_total", "tipo", "a", "b", "c") %in% names(arboles_df))) {
    stop("Faltan columnas requeridas: diametro_normal, altura_total, tipo, a, b, c")
  }
  
  arboles_df %>%
    mutate(
      volumen_m3 = pmap_dbl(
        list(diametro_normal, altura_total, tipo, a, b, c),
        calcular_volumen_arbol
      )
    )
}

# ==============================================================================
# 3. CLASES DIAMÉTRICAS (CONSOLIDADO)
# ==============================================================================

#' @title Asignar clase diamétrica
#' @description Función única para clasificación por diámetro
#' 
#' @param diametro Diámetro normal en cm (puede ser vector)
#' @param breaks Vector de límites de clase (por defecto: seq(5, 100, 5))
#' @param formato Formato de etiquetas: "rango" (ej: "30-35") o "medio" (ej: "32.5")
#' 
#' @return Factor con clases diamétricas
asignar_clase_diametrica <- function(diametro, 
                                     breaks = seq(5, 100, by = 5),
                                     formato = "rango") {
  
  # Validar
  if (any(is.na(diametro))) {
    warning("Hay valores NA en diámetros. Se mantendrán como NA en clases.")
  }
  
  # Crear etiquetas según formato
  if (formato == "centro") {
    # NUEVO: Formato "centro" para compatibilidad con optimizador
    # Devuelve valores numéricos (7.5, 12.5, 17.5, etc.)
    clase_numerica <- cut(
      diametro,
      breaks = breaks,
      labels = FALSE,  # Devuelve índices
      include.lowest = TRUE,
      right = FALSE
    )
    
    # Convertir índices a valores centrales de clase
    centros <- (breaks[-length(breaks)] + breaks[-1]) / 2
    resultado <- centros[clase_numerica]
    
    return(resultado)  # Retorna numérico
    
  } else {
    # Formatos originales (rango, medio) - devuelven factor/character
    labels <- switch(
      formato,
      
      "rango" = {
        # "5-10", "10-15", etc.
        paste0(breaks[-length(breaks)], "-", breaks[-1])
      },
      
      "medio" = {
        # "7.5", "12.5", etc.
        as.character((breaks[-length(breaks)] + breaks[-1]) / 2)
      },
      
      # Default: rango
      paste0(breaks[-length(breaks)], "-", breaks[-1])
    )
    
    # Clasificar
    clase <- cut(
      diametro,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
    
    return(clase)  # Retorna factor
  }
}
#' @title Extraer límite inferior de clase
#' @description Obtiene el valor numérico inicial de una clase (ej: "30-35" → 30)
extraer_limite_inferior_clase <- function(clase_diametrica) {
  as.numeric(str_extract(as.character(clase_diametrica), "^\\d+"))
}

#' @title Extraer límite superior de clase
#' @description Obtiene el valor numérico final de una clase (ej: "30-35" → 35)
extraer_limite_superior_clase <- function(clase_diametrica) {
  as.numeric(str_extract(as.character(clase_diametrica), "\\d+$"))
}

#' @title Extraer valor medio de clase
#' @description Calcula el punto medio de una clase (ej: "30-35" → 32.5)
extraer_valor_medio_clase <- function(clase_diametrica) {
  (extraer_limite_inferior_clase(clase_diametrica) + 
     extraer_limite_superior_clase(clase_diametrica)) / 2
}

# ==============================================================================
# 4. CÁLCULOS DASOMÉTRICOS BÁSICOS (CORREGIDO - VECTORIZADO)
# ==============================================================================

#' @title Calcular área basal individual o vectorizado
#' @param d_cm Diámetro normal en cm (escalar o vector)
#' @return Área basal en m² (mismo tipo que input)
calcular_area_basal <- function(d_cm) {
  # Vectorizar la validación
  result <- ifelse(is.na(d_cm) | d_cm <= 0, 
                   NA_real_, 
                   pi * (d_cm / 200)^2)
  return(result)
}

#' @title Calcular diámetro cuadrático
#' @param diametros Vector de diámetros en cm
#' @return Diámetro cuadrático en cm
calcular_dmc <- function(diametros) {
  sqrt(mean(diametros^2, na.rm = TRUE))
}

#' @title Calcular altura de Lorey (ponderada por AB)
#' @param alturas Vector de alturas en m
#' @param areas_basales Vector de áreas basales en m²
#' @return Altura de Lorey en m
calcular_altura_lorey <- function(alturas, areas_basales) {
  sum(alturas * areas_basales, na.rm = TRUE) / sum(areas_basales, na.rm = TRUE)
}

# ==============================================================================
# 5. FACTOR DE EXPANSIÓN
# ==============================================================================

#' @title Calcular factor de expansión
#' @description Convierte área a factor multiplicador
#' @param area_parcela_ha Área total muestreada en hectáreas
#' @return Factor multiplicador (ej: 0.3 ha → factor 3.33)
#' 
#' @details
#' Si tienes múltiples parcelas, primero calcula el área total:
#' area_total = area_una_parcela × n_muestreos
#' 
#' Ejemplo:
#' - 3 parcelas de 0.1 ha cada una
#' - area_total = 0.1 × 3 = 0.3 ha
#' - factor = calcular_factor_expansion(0.3) = 3.33
#' 
calcular_factor_expansion <- function(area_parcela_ha = CONFIG$area_parcela_ha) {
  1 / area_parcela_ha
}

#' @title Expandir a hectárea
#' @description Multiplica valores de parcela por factor de expansión
#' @param valor_parcela Valor medido en la(s) parcela(s)
#' @param area_parcela_ha Área TOTAL muestreada en ha
#' @return Valor expandido por hectárea
#' 
#' @details
#' Uso típico con múltiples muestreos:
#' area_total <- config$area_parcela_ha × n_muestreos
#' valor_ha <- expandir_a_hectarea(valor_medido, area_total)
#' 
#' Ejemplo:
#' - Volumen medido en 3 parcelas: 15 m³
#' - Área total: 0.1 × 3 = 0.3 ha
#' - Volumen/ha: expandir_a_hectarea(15, 0.3) = 50 m³/ha
#' 
expandir_a_hectarea <- function(valor_parcela, area_parcela_ha = 0.05) {
  valor_parcela * calcular_factor_expansion(area_parcela_ha)
}

# ==============================================================================
# 6. VALIDACIONES
# ==============================================================================

#' @title Validar rango de valores dasométricos
#' @description Verifica que valores estén en rangos biológicamente razonables
validar_rangos_dasometricos <- function(arboles_df, 
                                        d_min = 7.5, d_max = 150,
                                        h_min = 1.5, h_max = 50,
                                        ratio_hd_min = 0.15, ratio_hd_max = 2.5) {
  
  problemas <- list()
  
  # Diámetros
  if (any(arboles_df$diametro_normal < d_min, na.rm = TRUE)) {
    n <- sum(arboles_df$diametro_normal < d_min, na.rm = TRUE)
    problemas$d_bajo <- sprintf("%d árboles con d < %.1f cm", n, d_min)
  }
  
  if (any(arboles_df$diametro_normal > d_max, na.rm = TRUE)) {
    n <- sum(arboles_df$diametro_normal > d_max, na.rm = TRUE)
    problemas$d_alto <- sprintf("%d árboles con d > %.1f cm", n, d_max)
  }
  
  # Alturas
  if (any(arboles_df$altura_total < h_min, na.rm = TRUE)) {
    n <- sum(arboles_df$altura_total < h_min, na.rm = TRUE)
    problemas$h_bajo <- sprintf("%d árboles con h < %.1f m", n, h_min)
  }
  
  if (any(arboles_df$altura_total > h_max, na.rm = TRUE)) {
    n <- sum(arboles_df$altura_total > h_max, na.rm = TRUE)
    problemas$h_alto <- sprintf("%d árboles con h > %.1f m", n, h_max)
  }
  
  # Relación H/D
  ratio_hd <- arboles_df$altura_total / arboles_df$diametro_normal
  
  if (any(ratio_hd < ratio_hd_min, na.rm = TRUE)) {
    n <- sum(ratio_hd < ratio_hd_min, na.rm = TRUE)
    problemas$ratio_bajo <- sprintf("%d árboles con H/D < %.2f", n, ratio_hd_min)
  }
  
  if (any(ratio_hd > ratio_hd_max, na.rm = TRUE)) {
    n <- sum(ratio_hd > ratio_hd_max, na.rm = TRUE)
    problemas$ratio_alto <- sprintf("%d árboles con H/D > %.2f", n, ratio_hd_max)
  }
  
  # Resultado
  if (length(problemas) == 0) {
    cat("✓ Todos los valores en rangos válidos\n")
    return(TRUE)
  } else {
    cat("⚠ Problemas detectados:\n")
    for (p in problemas) {
      cat(sprintf("  - %s\n", p))
    }
    return(FALSE)
  }
}

# ==============================================================================
# 7. TESTS UNITARIOS
# ==============================================================================

test_core_calculos <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          TEST UNITARIO - CORE_CALCULOS.R                  ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Test 1: Volumen
  cat("[TEST 1] Cálculo de volumen\n")
  vol_potencia <- calcular_volumen_arbol(30, 12, "potencia", 0.00004, 1.93694, 1.03169)
  vol_exp <- calcular_volumen_arbol(30, 12, "exp", -9.48686252, 1.82408096, 0.96892639)
  
  cat(sprintf("  Potencia: %.4f m³\n", vol_potencia))
  cat(sprintf("  Exp:      %.4f m³\n", vol_exp))
  
  stopifnot(vol_potencia > 0, vol_potencia < 2)  # Rango razonable
  stopifnot(vol_exp > 0, vol_exp < 2)
  cat("  ✓ Volúmenes en rango esperado\n\n")
  
  # Test 2: Filtrado
  cat("[TEST 2] Filtrado de árboles\n")
  arboles_test <- tibble(
    arbol_id = 1:10,
    dominancia = c(1, 2, 3, 6, 7, 8, 9, 1, 2, 3)
  )
  
  vivos <- filtrar_arboles_vivos(arboles_test)
  muertos <- filtrar_arboles_muertos(arboles_test)
  
  cat(sprintf("  Total: %d | Vivos: %d | Muertos: %d\n", 
              nrow(arboles_test), nrow(vivos), nrow(muertos)))
  
  stopifnot(nrow(vivos) == 7)
  stopifnot(nrow(muertos) == 3)
  stopifnot(nrow(vivos) + nrow(muertos) == nrow(arboles_test))
  cat("  ✓ Filtrado correcto\n\n")
  
  # Test 3: Clases diamétricas
  cat("[TEST 3] Clasificación diamétrica\n")
  diametros <- c(8, 15, 23, 37, 48)
  clases <- asignar_clase_diametrica(diametros)
  
  cat("  Diámetros:", paste(diametros, collapse=", "), "cm\n")
  cat("  Clases:   ", paste(as.character(clases), collapse=", "), "\n")
  
  stopifnot(length(clases) == length(diametros))
  stopifnot(as.character(clases[1]) == "5-10")
  stopifnot(as.character(clases[3]) == "20-25")
  cat("  ✓ Clasificación correcta\n\n")
  
  # Test 4: Extracción de límites
  cat("[TEST 4] Extracción de límites de clase\n")
  clase_test <- "30-35"
  lim_inf <- extraer_limite_inferior_clase(clase_test)
  lim_sup <- extraer_limite_superior_clase(clase_test)
  lim_med <- extraer_valor_medio_clase(clase_test)
  
  cat(sprintf("  Clase: %s\n", clase_test))
  cat(sprintf("  Límite inferior: %.0f cm\n", lim_inf))
  cat(sprintf("  Límite superior: %.0f cm\n", lim_sup))
  cat(sprintf("  Valor medio:     %.1f cm\n", lim_med))
  
  stopifnot(lim_inf == 30)
  stopifnot(lim_sup == 35)
  stopifnot(lim_med == 32.5)
  cat("  ✓ Extracción correcta\n\n")
  
  # Test 5: Área basal
  cat("[TEST 5] Cálculo de área basal\n")
  ab <- calcular_area_basal(30)
  cat(sprintf("  d = 30 cm → AB = %.6f m²\n", ab))
  
  # Valor esperado: π × (30/200)² = π × 0.15² ≈ 0.0707
  stopifnot(abs(ab - 0.0707) < 0.001)
  cat("  ✓ Área basal correcta\n\n")
  
  # Test 6: Factor de expansión
  cat("[TEST 6] Factor de expansión\n")
  factor <- calcular_factor_expansion(0.05)
  cat(sprintf("  0.05 ha → factor = %.0f\n", factor))
  
  stopifnot(factor == 20)
  cat("  ✓ Factor correcto\n\n")
  
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║               ✓ TODOS LOS TESTS PASARON                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  return(TRUE)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ core_calculos.R cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    FILTRADO:\n")
cat("      - filtrar_arboles_vivos(arboles_df)\n")
cat("      - filtrar_arboles_muertos(arboles_df)\n")
cat("      - es_arbol_vivo(dominancia)\n")
cat("    VOLUMEN:\n")
cat("      - calcular_volumen_arbol(d, h, tipo, a, b, c)\n")
cat("      - calcular_volumenes_vectorizado(arboles_df)\n")
cat("    CLASES:\n")
cat("      - asignar_clase_diametrica(diametro, breaks, formato)\n")
cat("      - extraer_limite_inferior_clase(clase)\n")
cat("      - extraer_limite_superior_clase(clase)\n")
cat("      - extraer_valor_medio_clase(clase)\n")
cat("    DASOMÉTRICOS:\n")
cat("      - calcular_area_basal(d_cm)\n")
cat("      - calcular_diametro_cuadratico(diametros)\n")
cat("      - calcular_altura_lorey(alturas, areas_basales)\n")
cat("    EXPANSIÓN:\n")
cat("      - calcular_factor_expansion(area_parcela_ha)\n")
cat("      - expandir_a_hectarea(valor, area_parcela_ha)\n")
cat("    VALIDACIÓN:\n")
cat("      - validar_rangos_dasometricos(arboles_df)\n")
cat("    TEST:\n")
cat("      - test_core_calculos()  [ejecutar para probar]\n\n")