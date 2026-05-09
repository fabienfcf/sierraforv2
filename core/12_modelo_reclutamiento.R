# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO 4: MODELO DE RECLUTAMIENTO
# Ingreso de nuevos árboles que alcanzan el diámetro mínimo de inventario
# Actualizado para nueva estructura de configuración modular
# ==============================================================================

library(tidyverse)
library(stringr)

# Cargar validaciones compartidas
if (!exists("validar_reclutamiento")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_validacion.R"))
}

cat("\n[RECLUTAMIENTO] Cargando módulo de reclutamiento...\n")

# ==============================================================================
# 1. CALCULAR NÚMERO DE RECLUTAS POR RODAL
# ==============================================================================

calcular_n_reclutas <- function(arboles_rodal, config = CONFIG) {
  
  # Contar árboles vivos actuales
  n_vivos <- sum(es_arbol_vivo(arboles_rodal$dominancia))
  
  # Calcular número de reclutas según tasa
  n_reclutas <- round(n_vivos * config$tasa_reclutamiento)
  
  # Mínimo 0, máximo razonable (evitar explosión demográfica)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.1))  # Máx 10% del rodal
  
  return(n_reclutas)
}

# ==============================================================================
# 2. DETERMINAR COMPOSICIÓN DE ESPECIES DE RECLUTAS
# ==============================================================================

calcular_composicion_reclutas <- function(arboles_rodal, n_reclutas, config = CONFIG) {
  
  # Cargar funciones core si no están
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  # Obtener composición actual de especies (solo vivos)
  composicion_actual <- arboles_rodal %>%
    filtrar_arboles_vivos() %>%
    filter(genero_grupo %in% config$generos) %>%
    count(nombre_cientifico, genero_grupo, name = "n_actual") %>%
    mutate(proporcion = n_actual / sum(n_actual))
  
  # Si no hay árboles vivos, distribución equitativa por género
  if (nrow(composicion_actual) == 0) {
    warning("No hay árboles vivos para determinar composición. Usando distribución por defecto.")
    
    # Usar ESPECIES de CONFIG
    especies_disponibles <- config$especies %>%
      filter(genero %in% config$generos)
    
    composicion_actual <- especies_disponibles %>%
      group_by(genero) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        proporcion = 1 / n(),
        n_actual = 1
      ) %>%
      rename(nombre_cientifico = nombre_cientifico, genero_grupo = genero)
  }
  
  # Asignar número de reclutas por especie
  reclutas_por_especie <- composicion_actual %>%
    mutate(
      n_reclutas_esperado = n_reclutas * proporcion,
      n_reclutas = round(n_reclutas_esperado)
    )
  
  # Ajustar para que sume exactamente n_reclutas
  diferencia <- n_reclutas - sum(reclutas_por_especie$n_reclutas)
  
  if (diferencia != 0) {
    # Agregar/quitar la diferencia a la especie más abundante
    idx_max <- which.max(reclutas_por_especie$n_reclutas)
    reclutas_por_especie$n_reclutas[idx_max] <- 
      reclutas_por_especie$n_reclutas[idx_max] + diferencia
  }
  
  return(reclutas_por_especie)
}

# ==============================================================================
# 3. GENERAR ÁRBOLES RECLUTAS
# ==============================================================================

generar_reclutas <- function(rodal_id, composicion_reclutas, config = CONFIG, año_actual) {
  
  if (sum(composicion_reclutas$n_reclutas) == 0) {
    return(tibble())
  }
  
  # Generar árboles por especie
  reclutas_list <- list()
  id_counter <- 1
  
  for (i in 1:nrow(composicion_reclutas)) {
    
    especie_info <- composicion_reclutas[i, ]
    n <- especie_info$n_reclutas
    
    if (n > 0) {
      
      # Generar diámetros aleatorios en rango de ingreso
      diametros <- runif(n, 
                         min = config$reclut_d_min, 
                         max = config$reclut_d_max)
      
      # Altura inicial según género
      altura_base <- config$reclut_altura[[especie_info$genero_grupo]]
      
      if (is.null(altura_base)) {
        altura_base <- 3.0  # Default
      }
      
      # Altura con variación ±20%
      alturas <- altura_base * runif(n, min = 0.8, max = 1.2)
      
      # Buscar parámetros alométricos usando ECUACIONES_VOLUMEN de CONFIG
      parametros <- config$ecuaciones_volumen %>%
        filter(nombre_cientifico == especie_info$nombre_cientifico)
      
      # Si no se encuentra la especie exacta, buscar otra del mismo género
      if (nrow(parametros) == 0) {
        genero <- especie_info$genero_grupo
        parametros <- config$ecuaciones_volumen %>%
          filter(str_detect(nombre_cientifico, genero)) %>%
          slice(1)
        
        if (nrow(parametros) > 0) {
          warning(sprintf(
            "No hay ecuación para '%s', usando '%s' del mismo género",
            especie_info$nombre_cientifico,
            parametros$nombre_cientifico[1]
          ))
        }
      }
      
      # Extraer parámetros o usar defaults
      if (nrow(parametros) > 0) {
        tipo_alom <- parametros$tipo[1]
        a_param <- parametros$a[1]
        b_param <- parametros$b[1]
        c_param <- parametros$c[1]
      } else {
        tipo_alom <- "potencia"
        a_param <- 0.00004
        b_param <- 1.90
        c_param <- 1.00
        warning(sprintf(
          "⚠ No se encontró ecuación para '%s' ni para su género. Usando defaults genéricos.",
          especie_info$nombre_cientifico
        ))
      }
      
      # Crear tibble de reclutas
      reclutas_especie <- tibble(
        arbol_id = paste0("RECLUTA_R", rodal_id, "_A", año_actual, "_", 
                          str_replace_all(especie_info$nombre_cientifico, " ", "_"), "_", 
                          sprintf("%03d", 1:n)),
        rodal = rodal_id,
        nombre_cientifico = especie_info$nombre_cientifico,
        genero_grupo = especie_info$genero_grupo,
        dominancia = config$reclut_dominancia,
        diametro_normal = diametros,
        altura_total = alturas,
        area_basal = pi * (diametros / 200)^2,
        tipo = tipo_alom,
        a = a_param,
        b = b_param,
        c = c_param,
        año_reclutamiento = año_actual,
        es_recluta = TRUE
      )
      
      # Calcular volumen usando core_calculos
      if (!exists("calcular_volumen_arbol")) {
        source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
      }
      
      reclutas_list[[i]] <- reclutas_especie %>%
        mutate(
          volumen_m3 = pmap_dbl(
            list(diametro_normal, altura_total, tipo, a, b, c),
            calcular_volumen_arbol
          )
        )
    }
  }
  
  # Combinar todos los reclutas
  if (length(reclutas_list) > 0) {
    reclutas_df <- bind_rows(reclutas_list)
    return(reclutas_df)
  } else {
    return(tibble())
  }
}

# ==============================================================================
# 4. APLICAR RECLUTAMIENTO A TODA LA POBLACIÓN
# ==============================================================================

aplicar_reclutamiento <- function(arboles_df, config = CONFIG, año_actual) {
  
  cat(sprintf("\n[AÑO %d] Aplicando reclutamiento...\n", año_actual))
  
  # Cargar funciones core si no están
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  # Contar árboles vivos totales antes
  n_vivos_inicial <- nrow(filtrar_arboles_vivos(arboles_df))
  
  cat(sprintf("  Árboles vivos antes: %d\n", n_vivos_inicial))
  
  # Generar reclutas por rodal
  reclutas_total <- tibble()
  
  rodales_unicos <- unique(arboles_df$rodal)
  
  for (rodal_id in rodales_unicos) {
    
    # Filtrar árboles del rodal
    arboles_rodal <- arboles_df %>% filter(rodal == rodal_id)
    
    # Calcular número de reclutas
    n_reclutas <- calcular_n_reclutas(arboles_rodal, config)
    
    if (n_reclutas > 0) {
      
      # Determinar composición
      composicion <- calcular_composicion_reclutas(arboles_rodal, n_reclutas, config)
      
      # Generar reclutas
      reclutas_rodal <- generar_reclutas(rodal_id, composicion, config, año_actual)
      
      if (nrow(reclutas_rodal) > 0) {
        reclutas_total <- bind_rows(reclutas_total, reclutas_rodal)
      }
    }
  }
  
  # Reportar
  n_reclutas_total <- nrow(reclutas_total)
  
  if (n_reclutas_total > 0) {
    cat(sprintf("  Reclutas generados:  %d (%.2f%% de vivos)\n", 
                n_reclutas_total, 
                (n_reclutas_total / n_vivos_inicial) * 100))
    
    # Distribución por género
    dist_genero <- reclutas_total %>%
      count(genero_grupo, name = "n_reclutas")
    
    cat("\n  Distribución por género:\n")
    print(dist_genero)
    
    # Agregar columnas faltantes para compatibilidad
    reclutas_total <- reclutas_total %>%
      mutate(
        incremento_d_cm = 0,
        incremento_h_m = 0,
        incremento_vol_m3 = 0,
        murio_este_año = FALSE,
        dominancia_original = NA_real_,
        año_muerte = NA_real_,
        probabilidad_muerte = NA_real_
      )
    
    # Unir con población existente
    arboles_con_reclutas <- bind_rows(arboles_df, reclutas_total)
    
  } else {
    cat("  Reclutas generados:  0\n")
    arboles_con_reclutas <- arboles_df
  }
  
  # Contar total después
  n_vivos_final <- sum(es_arbol_vivo(arboles_con_reclutas$dominancia))
  cat(sprintf("  Árboles vivos después: %d\n", n_vivos_final))
  
  return(arboles_con_reclutas)
}

# ==============================================================================
# 5. VALIDACIÓN DE RECLUTAMIENTO (usa función compartida)
# ==============================================================================

# La función validar_reclutamiento() ahora está en utils_validacion.R
# para evitar duplicación de código

# ==============================================================================
# 6. REPORTE DE RECLUTAMIENTO ACUMULADO
# ==============================================================================

reporte_reclutamiento_acumulado <- function(arboles_df) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        REPORTE DE RECLUTAMIENTO ACUMULADO                 ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!"es_recluta" %in% names(arboles_df)) {
    cat("No hay información de reclutamiento disponible.\n\n")
    return(NULL)
  }
  
  reclutas <- arboles_df %>% filter(es_recluta == TRUE)
  
  if (nrow(reclutas) == 0) {
    cat("No ha habido reclutamiento hasta el momento.\n\n")
    return(NULL)
  }
  
  # Por género
  cat("[RECLUTAMIENTO POR GÉNERO]\n")
  por_genero <- reclutas %>%
    group_by(genero_grupo) %>%
    summarise(
      n_reclutas = n(),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      h_media = mean(altura_total, na.rm = TRUE),
      .groups = "drop"
    )
  print(por_genero)
  
  # Por año
  if ("año_reclutamiento" %in% names(reclutas)) {
    cat("\n[RECLUTAMIENTO POR AÑO]\n")
    por_año <- reclutas %>%
      count(año_reclutamiento, name = "n_reclutas") %>%
      arrange(año_reclutamiento)
    print(por_año)
  }
  
  # Por rodal
  cat("\n[RECLUTAMIENTO POR RODAL]\n")
  por_rodal <- reclutas %>%
    count(rodal, name = "n_reclutas") %>%
    arrange(desc(n_reclutas))
  print(por_rodal)
  
  cat("\n")
  
  return(por_genero)
}

cat("\n✓ Módulo de reclutamiento cargado\n")
cat("  Usa ESPECIES y ECUACIONES_VOLUMEN de CONFIG\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_reclutamiento(arboles, CONFIG, año)\n")
cat("    - validar_reclutamiento(antes, despues, CONFIG)\n")
cat("    - reporte_reclutamiento_acumulado(arboles)\n\n")