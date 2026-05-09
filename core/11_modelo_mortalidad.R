# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO 3: MODELO DE MORTALIDAD
# Aplicación de tasas de mortalidad diferencial por dominancia
# Actualizado para nueva estructura de configuración modular
# ==============================================================================

library(tidyverse)

# Cargar validaciones compartidas
if (!exists("validar_mortalidad")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_validacion.R"))
}

cat("\n[MORTALIDAD] Cargando módulo de mortalidad...\n")

# ==============================================================================
# 1. CALCULAR PROBABILIDAD DE MUERTE INDIVIDUAL
# ==============================================================================

calcular_probabilidad_muerte <- function(arbol, config = CONFIG) {
  
  # Árboles ya muertos permanecen muertos
  if (!es_arbol_vivo(arbol$dominancia)) {
    return(1.0)  # 100% muerto (ya está muerto)
  }
  
  # Obtener tasa base de mortalidad
  tasa_base <- config$mortalidad_base
  
  # Obtener modificador por dominancia usando CODIGOS_DOMINANCIA
  modificador_mort <- config$modificadores_dominancia %>%
    filter(codigo == arbol$dominancia) %>%
    pull(factor_mortalidad)
  
  if (length(modificador_mort) == 0) {
    warning(sprintf("Dominancia %d no encontrada. Usando factor 1.0 por defecto.", 
                    arbol$dominancia))
    modificador_mort <- 1.0
  }
  
  # Probabilidad anual de muerte
  prob_muerte <- tasa_base * modificador_mort
  
  # Limitar a rango válido [0, 1]
  prob_muerte <- max(0, min(prob_muerte, 1))
  
  return(prob_muerte)
}

# ==============================================================================
# 2. APLICAR MORTALIDAD A UN ÁRBOL
# ==============================================================================

aplicar_mortalidad_arbol <- function(arbol, config = CONFIG, valor_aleatorio) {
  
  # Si ya está muerto, no hacer nada
  if (!es_arbol_vivo(arbol$dominancia)) {
    arbol$murio_este_año <- FALSE
    arbol$dominancia_original <- NA_real_
    return(arbol)
  }
  
  # Calcular probabilidad de muerte
  prob_muerte <- calcular_probabilidad_muerte(arbol, config)
  
  # Decidir si muere (comparar con valor aleatorio)
  muere <- valor_aleatorio < prob_muerte
  
  if (muere) {
    # Marcar como muerto en pie
    arbol$dominancia_original <- arbol$dominancia
    arbol$dominancia <- 7  # Muerto en pie (código de CODIGOS_DOMINANCIA)
    arbol$murio_este_año <- TRUE
    arbol$año_muerte <- NA  # Se asignará en la función principal
  } else {
    # NO murió
    arbol$dominancia_original <- NA_real_
    arbol$murio_este_año <- FALSE
  }
  
  return(arbol)
}

# ==============================================================================
# 3. APLICAR MORTALIDAD A TODA LA POBLACIÓN
# ==============================================================================

aplicar_mortalidad_poblacion <- function(arboles_df, config = CONFIG, año_actual) {
  
  cat(sprintf("\n[AÑO %d] Aplicando mortalidad...\n", año_actual))
  
  # Contar vivos iniciales
  n_vivos_inicial <- sum(es_arbol_vivo(arboles_df$dominancia))
  
  cat(sprintf("  Árboles vivos al inicio: %d\n", n_vivos_inicial))
  
  # Establecer semilla para reproducibilidad
  set.seed(config$semilla_mortalidad + año_actual)
  
  # Generar valores aleatorios para todos los árboles
  n_arboles <- nrow(arboles_df)
  valores_aleatorios <- runif(n_arboles)
  
  # Aplicar mortalidad a cada árbol
  arboles_procesados <- arboles_df %>%
    mutate(
      valor_aleatorio = valores_aleatorios,
      probabilidad_muerte = map_dbl(
        1:n(),
        ~calcular_probabilidad_muerte(arboles_df[.x, ], config)
      )
    ) %>%
    rowwise() %>%
    mutate(
      arbol_actualizado = list(aplicar_mortalidad_arbol(
        arbol = pick(everything()),
        config = config,
        valor_aleatorio = valor_aleatorio
      ))
    ) %>%
    ungroup() %>%
    # Desempaquetar resultados
    mutate(
      dominancia_original = map_dbl(arbol_actualizado, ~.x$dominancia_original),
      dominancia = map_dbl(arbol_actualizado, ~.x$dominancia),
      murio_este_año = map_lgl(arbol_actualizado, ~.x$murio_este_año),
      año_muerte = if_else(murio_este_año, año_actual, NA_real_)
    ) %>%
    select(-arbol_actualizado, -valor_aleatorio)
  
  # Contar muertos este año
  n_muertos_año <- sum(arboles_procesados$murio_este_año, na.rm = TRUE)
  n_vivos_final <- sum(es_arbol_vivo(arboles_procesados$dominancia))
  
  cat(sprintf("  Árboles muertos este año: %d (%.2f%%)\n", 
              n_muertos_año, 
              (n_muertos_año / n_vivos_inicial) * 100))
  cat(sprintf("  Árboles vivos al final:   %d\n", n_vivos_final))
  
  # Estadísticas por dominancia original (si hubo muertes)
  if (n_muertos_año > 0) {
    cat("\n  Mortalidad por clase de dominancia:\n")
    
    muertos_este_año <- arboles_procesados %>%
      filter(murio_este_año, !is.na(dominancia_original))
    
    if (nrow(muertos_este_año) > 0) {
      # Usar CODIGOS_DOMINANCIA para etiquetar
      if (!exists("CODIGOS_DOMINANCIA")) {
        # Si no está en ambiente global, usar de config
        codigos_dom <- config$modificadores_dominancia
      } else {
        codigos_dom <- CODIGOS_DOMINANCIA
      }
      
      resumen_mort <- muertos_este_año %>%
        left_join(
          codigos_dom %>% select(codigo, etiqueta),
          by = c("dominancia_original" = "codigo")
        ) %>%
        count(etiqueta, name = "n_muertos") %>%
        arrange(desc(n_muertos))
      
      print(resumen_mort)
    }
  }
  
  return(arboles_procesados)
}

# ==============================================================================
# 4. VALIDACIÓN DE MORTALIDAD (usa función compartida)
# ==============================================================================

# La función validar_mortalidad() ahora está en utils_validacion.R
# para evitar duplicación de código

# ==============================================================================
# 5. REPORTE DE MORTALIDAD ACUMULADA
# ==============================================================================

reporte_mortalidad_acumulada <- function(arboles_df, config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        REPORTE DE MORTALIDAD ACUMULADA                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Mortalidad por género
  mort_genero <- arboles_df %>%
    group_by(genero_grupo) %>%
    summarise(
      n_total = n(),
      n_muertos = sum(!es_arbol_vivo(dominancia)),
      n_vivos = n_total - n_muertos,
      tasa_mort_pct = (n_muertos / n_total) * 100,
      .groups = "drop"
    )
  
  cat("[MORTALIDAD POR GÉNERO]\n")
  print(mort_genero)
  
  # Mortalidad por dominancia original
  if ("dominancia_original" %in% names(arboles_df)) {
    # Usar CODIGOS_DOMINANCIA
    if (!exists("CODIGOS_DOMINANCIA")) {
      codigos_dom <- config$modificadores_dominancia
    } else {
      codigos_dom <- CODIGOS_DOMINANCIA
    }
    
    mort_dominancia <- arboles_df %>%
      filter(dominancia == 7, !is.na(dominancia_original)) %>%
      left_join(
        codigos_dom %>% select(codigo, etiqueta),
        by = c("dominancia_original" = "codigo")
      ) %>%
      count(etiqueta, name = "n_muertos") %>%
      arrange(desc(n_muertos))
    
    if (nrow(mort_dominancia) > 0) {
      cat("\n[MORTALIDAD POR CLASE DE DOMINANCIA ORIGINAL]\n")
      print(mort_dominancia)
    }
  }
  
  # Distribución temporal de mortalidad
  if ("año_muerte" %in% names(arboles_df)) {
    mort_temporal <- arboles_df %>%
      filter(!is.na(año_muerte)) %>%
      count(año_muerte, name = "n_muertos") %>%
      arrange(año_muerte)
    
    if (nrow(mort_temporal) > 0) {
      cat("\n[MORTALIDAD POR AÑO]\n")
      print(mort_temporal)
    }
  }
  
  cat("\n")
  
  return(mort_genero)
}

cat("\n✓ Módulo de mortalidad cargado\n")
cat("  Usa CODIGOS_DOMINANCIA de CONFIG\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_mortalidad_poblacion(arboles, CONFIG, año)\n")
cat("    - validar_mortalidad(antes, despues)\n")
cat("    - reporte_mortalidad_acumulada(arboles, CONFIG)\n\n")