# ==============================================================================
# ANÁLISIS DASOMÉTRICO CON INTERVALOS DE CONFIANZA 95%
# NOM-152-SEMARNAT-2023 Sección 4.9.2
# ==============================================================================

library(tidyverse)
library(xtable)

# ==============================================================================
# FUNCIÓN PRINCIPAL: ANÁLISIS DASOMÉTRICO CON IC 95%
# ==============================================================================

#' Análisis dasométrico con intervalos de confianza al 95%
#' 
#' Genera dos tablas:
#' 1. Por UMM y especie (Pinus/Quercus especies + línea de muertos por género)
#' 2. Por UMM y género (Pinus/Quercus totales)
#' 
#' @param arboles_df DataFrame con árboles del inventario
#' @param config Objeto CONFIG con parámetros
#' @param error_muestreo Error de muestreo (default 0.10 = 10%)
#' @param exportar_csv_flag Exportar CSV (TRUE/FALSE)
#' @param exportar_latex Exportar tabla LaTeX (TRUE/FALSE)
#' @return Lista con resultados de ambas tablas
analizar_dasometrico_con_ic <- function(arboles_df, 
                                         config = CONFIG,
                                         error_muestreo = 0.10,
                                         exportar_csv_flag = TRUE,
                                         exportar_latex = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      ANÁLISIS DASOMÉTRICO CON IC 95% (NOM-152 4.9.2)     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar funciones necesarias
  if (!exists("filtrar_arboles_vivos")) {
    source("modelov5/15_core_calculos.R")
  }
  
  # Separar vivos y muertos (excluir tocones = dominancia 9)
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  muertos <- arboles_df %>% 
    filter(dominancia == 8)  # Solo muertos, NO tocones (9)
  
  # Número total de sitios
  n_sitios_total <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Error de muestreo: %.0f%%\n", error_muestreo * 100))
  cat(sprintf("Intervalo de confianza: 95%%\n"))
  cat(sprintf("Sitios totales: %d\n", n_sitios_total))
  cat(sprintf("Árboles vivos: %d\n", nrow(vivos)))
  cat(sprintf("Árboles muertos: %d\n\n", nrow(muertos)))
  
  # ==============================================================================
  # FUNCIÓN AUXILIAR: CALCULAR IC 95%
  # ==============================================================================
  
  calcular_ic95 <- function(valores) {
    n <- length(valores)
    media <- mean(valores, na.rm = TRUE)
    desv_std <- sd(valores, na.rm = TRUE)
    
    # Error estándar
    error_std <- desv_std / sqrt(n)
    
    # Valor t para IC 95%
    t_value <- qt(0.975, df = n - 1)
    
    # Margen de error
    margen_error <- t_value * error_std
    
    # IC
    ic_inf <- media - margen_error
    ic_sup <- media + margen_error
    
    # Asegurar que IC inferior no sea negativo para variables que no pueden serlo
    ic_inf <- max(0, ic_inf)
    
    list(
      media = media,
      ic_inf = ic_inf,
      ic_sup = ic_sup,
      n = n
    )
  }
  
  # ==============================================================================
  # MÉTRICAS POR SITIO - VIVOS
  # ==============================================================================
  
  cat("[1/4] Calculando métricas por sitio (vivos)...\n")
  
  # Por especie y sitio
  metricas_sitio_especie <- vivos %>%
    group_by(muestreo, rodal, genero_grupo, nombre_cientifico) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      vol_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_m2, config$area_parcela_ha),
      vol_m3ha = expandir_a_hectarea(vol_m3, config$area_parcela_ha)
    )
  
  # Por género y sitio (para tabla 2)
  metricas_sitio_genero <- vivos %>%
    group_by(muestreo, rodal, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      vol_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_m2, config$area_parcela_ha),
      vol_m3ha = expandir_a_hectarea(vol_m3, config$area_parcela_ha)
    )
  
  # ==============================================================================
  # MÉTRICAS POR SITIO - MUERTOS (SOLO DENSIDAD Y AB)
  # ==============================================================================
  
  cat("[2/4] Calculando métricas por sitio (muertos)...\n")
  
  metricas_sitio_muertos <- muertos %>%
    group_by(muestreo, rodal, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_m2, config$area_parcela_ha)
    )
  
  # ==============================================================================
  # TABLA 1: POR UMM Y ESPECIE (INCLUYENDO MUERTOS)
  # ==============================================================================
  
  cat("[3/4] Generando Tabla 1 (por UMM y especie)...\n")
  
  # Tabla de especies vivas
  tabla1_vivos <- metricas_sitio_especie %>%
    group_by(rodal, genero_grupo, nombre_cientifico) %>%
    summarise(
      # Densidad
      densidad = list(calcular_ic95(densidad_ha)),
      # Área basal
      ab = list(calcular_ic95(ab_m2ha)),
      # Volumen
      vol = list(calcular_ic95(vol_m3ha)),
      n_sitios = n(),
      .groups = "drop"
    ) %>%
    mutate(
      tipo = "Vivo",
      # Extraer valores
      dens_media = map_dbl(densidad, ~.x$media),
      dens_ic_inf = map_dbl(densidad, ~.x$ic_inf),
      dens_ic_sup = map_dbl(densidad, ~.x$ic_sup),
      ab_media = map_dbl(ab, ~.x$media),
      ab_ic_inf = map_dbl(ab, ~.x$ic_inf),
      ab_ic_sup = map_dbl(ab, ~.x$ic_sup),
      vol_media = map_dbl(vol, ~.x$media),
      vol_ic_inf = map_dbl(vol, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol, ~.x$ic_sup)
    ) %>%
    select(rodal, genero_grupo, nombre_cientifico, tipo, 
           dens_media, dens_ic_inf, dens_ic_sup,
           ab_media, ab_ic_inf, ab_ic_sup,
           vol_media, vol_ic_inf, vol_ic_sup,
           n_sitios)
  
  # Tabla de muertos (solo densidad y AB, SIN volumen)
  tabla1_muertos <- metricas_sitio_muertos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      densidad = list(calcular_ic95(densidad_ha)),
      ab = list(calcular_ic95(ab_m2ha)),
      n_sitios = n(),
      .groups = "drop"
    ) %>%
    mutate(
      tipo = "Muerto",
      nombre_cientifico = paste(genero_grupo, "(muertos)"),
      dens_media = map_dbl(densidad, ~.x$media),
      dens_ic_inf = map_dbl(densidad, ~.x$ic_inf),
      dens_ic_sup = map_dbl(densidad, ~.x$ic_sup),
      ab_media = map_dbl(ab, ~.x$media),
      ab_ic_inf = map_dbl(ab, ~.x$ic_inf),
      ab_ic_sup = map_dbl(ab, ~.x$ic_sup),
      vol_media = NA_real_,
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_
    ) %>%
    select(rodal, genero_grupo, nombre_cientifico, tipo,
           dens_media, dens_ic_inf, dens_ic_sup,
           ab_media, ab_ic_inf, ab_ic_sup,
           vol_media, vol_ic_inf, vol_ic_sup,
           n_sitios)
  
  # Combinar vivos y muertos
  tabla1_completa <- bind_rows(tabla1_vivos, tabla1_muertos) %>%
    arrange(rodal, genero_grupo, desc(tipo), nombre_cientifico)
  
  # Calcular subtotales por UMM
  subtotales_umm <- tabla1_vivos %>%
    group_by(rodal) %>%
    summarise(
      genero_grupo = "TOTAL",
      nombre_cientifico = "Subtotal UMM",
      tipo = "Vivo",
      dens_media = sum(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = sum(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = sum(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      n_sitios = first(n_sitios),
      .groups = "drop"
    )
  
  # Total general
  total_general_1 <- tabla1_vivos %>%
    summarise(
      rodal = "TOTAL",
      genero_grupo = "TOTAL",
      nombre_cientifico = "Total General",
      tipo = "Vivo",
      dens_media = mean(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = mean(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = mean(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      n_sitios = n_sitios_total
    )
  
  tabla1_final <- bind_rows(
    tabla1_completa,
    subtotales_umm,
    total_general_1
  ) %>%
    arrange(rodal, genero_grupo, desc(tipo))
  
  # ==============================================================================
  # TABLA 2: POR UMM Y GÉNERO (PINUS/QUERCUS TOTALES)
  # ==============================================================================
  
  cat("[4/4] Generando Tabla 2 (por UMM y género)...\n")
  
  # Tabla por género (solo vivos)
  tabla2_genero <- metricas_sitio_genero %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      densidad = list(calcular_ic95(densidad_ha)),
      ab = list(calcular_ic95(ab_m2ha)),
      vol = list(calcular_ic95(vol_m3ha)),
      n_sitios = n(),
      .groups = "drop"
    ) %>%
    mutate(
      dens_media = map_dbl(densidad, ~.x$media),
      dens_ic_inf = map_dbl(densidad, ~.x$ic_inf),
      dens_ic_sup = map_dbl(densidad, ~.x$ic_sup),
      ab_media = map_dbl(ab, ~.x$media),
      ab_ic_inf = map_dbl(ab, ~.x$ic_inf),
      ab_ic_sup = map_dbl(ab, ~.x$ic_sup),
      vol_media = map_dbl(vol, ~.x$media),
      vol_ic_inf = map_dbl(vol, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol, ~.x$ic_sup)
    ) %>%
    select(-densidad, -ab, -vol)
  
  # Subtotales por UMM
  subtotales_umm_2 <- tabla2_genero %>%
    group_by(rodal) %>%
    summarise(
      genero_grupo = "Subtotal",
      dens_media = sum(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = sum(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = sum(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      n_sitios = first(n_sitios),
      .groups = "drop"
    )
  
  # Total general
  total_general_2 <- tabla2_genero %>%
    summarise(
      rodal = "TOTAL",
      genero_grupo = "TOTAL",
      dens_media = mean(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = mean(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = mean(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      n_sitios = n_sitios_total
    )
  
  tabla2_final <- bind_rows(
    tabla2_genero,
    subtotales_umm_2,
    total_general_2
  ) %>%
    arrange(rodal, genero_grupo)
  
  # ==============================================================================
  # EXPORTAR CSV
  # ==============================================================================
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    
    dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
    
    write.csv(tabla1_final, 
              "resultados/analisis_dasometrico_por_especie.csv", 
              row.names = FALSE)
    cat("  ✓ CSV: analisis_dasometrico_por_especie.csv\n")
    
    write.csv(tabla2_final, 
              "resultados/analisis_dasometrico_por_genero.csv", 
              row.names = FALSE)
    cat("  ✓ CSV: analisis_dasometrico_por_genero.csv\n")
  }
  
  # ==============================================================================
  # EXPORTAR LATEX
  # ==============================================================================
  
  if (exportar_latex) {
    cat("\n[GENERANDO TABLAS LATEX]\n")
    
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    # -------------------------------------------------------------------------
    # TABLA 1 LATEX: POR ESPECIE
    # -------------------------------------------------------------------------
    
    tabla1_latex <- tabla1_final %>%
      mutate(
        UMM = as.character(rodal),
        Especie = nombre_cientifico,
        # Formatear densidad
        Densidad = ifelse(
          is.na(dens_ic_inf),
          sprintf("%.1f", dens_media),
          sprintf("%.1f (%.1f -- %.1f)", dens_media, dens_ic_inf, dens_ic_sup)
        ),
        # Formatear AB
        `AB (m²/ha)` = ifelse(
          is.na(ab_ic_inf),
          sprintf("%.2f", ab_media),
          sprintf("%.2f (%.2f -- %.2f)", ab_media, ab_ic_inf, ab_ic_sup)
        ),
        # Formatear volumen (NA para muertos)
        `Vol (m³/ha)` = ifelse(
          is.na(vol_media),
          "--",
          ifelse(
            is.na(vol_ic_inf),
            sprintf("%.2f", vol_media),
            sprintf("%.2f (%.2f -- %.2f)", vol_media, vol_ic_inf, vol_ic_sup)
          )
        )
      ) %>%
      select(UMM, Especie, Densidad, `AB (m²/ha)`, `Vol (m³/ha)`)
    
    latex1_content <- c(
      "% ============================================================",
      "% TABLA 1: Análisis Dasométrico por Especie",
      "% Incluye árboles muertos (sin tocones)",
      "% ============================================================",
      "\\begin{landscape}",
      "\t\\begin{table}[H]",
      "\t\t\\centering",
      "\t\t\\caption{Análisis dasométrico por especie y UMM con IC 95\\% (Sección 4.9.2)}",
      "\t\t\\label{tab:analisis_dasometrico_especie}",
      "\t\t\\tiny",
      "\t\t\\begin{tabular}{lp{5cm}ccc}",
      "\t\t\t\\toprule",
      "\t\t\t\\textbf{UMM} & \\textbf{Especie} & ",
      "\t\t\t\\shortstack{\\textbf{Densidad} \\\\ \\textbf{(árb/ha)} \\\\ \\textbf{Media (IC 95\\%)}} &",
      "\t\t\t\\shortstack{\\textbf{Área Basal} \\\\ \\textbf{(m²/ha)} \\\\ \\textbf{Media (IC 95\\%)}} &",
      "\t\t\t\\shortstack{\\textbf{Volumen} \\\\ \\textbf{(m³/ha)} \\\\ \\textbf{Media (IC 95\\%)}} \\\\",
      "\t\t\t\\midrule"
    )
    
    # Agregar filas
    umm_actual <- NULL
    for (i in 1:nrow(tabla1_latex)) {
      fila <- tabla1_latex[i, ]
      
      # Separador entre UMMs
      if (!is.null(umm_actual) && fila$UMM != umm_actual && 
          fila$UMM != "TOTAL" && !grepl("Subtotal", fila$Especie)) {
        latex1_content <- c(latex1_content, "\t\t\t\\midrule")
      }
      
      # Formato según tipo de fila
      if (fila$UMM == "TOTAL" || grepl("Subtotal|Total", fila$Especie)) {
        linea <- sprintf(
          "\t\t\t\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
          fila$UMM, fila$Especie, fila$Densidad, fila$`AB (m²/ha)`, fila$`Vol (m³/ha)`
        )
      } else if (grepl("muerto", fila$Especie, ignore.case = TRUE)) {
        linea <- sprintf(
          "\t\t\t%s & \\textit{%s} (muertos) & %s & %s & %s \\\\",
          fila$UMM, gsub(" \\(muertos\\)", "", fila$Especie), 
          fila$Densidad, fila$`AB (m²/ha)`, fila$`Vol (m³/ha)`
        )
      } else {
        linea <- sprintf(
          "\t\t\t%s & \\textit{%s} & %s & %s & %s \\\\",
          fila$UMM, fila$Especie, fila$Densidad, fila$`AB (m²/ha)`, fila$`Vol (m³/ha)`
        )
      }
      
      latex1_content <- c(latex1_content, linea)
      umm_actual <- fila$UMM
    }
    
    latex1_content <- c(
      latex1_content,
      "\t\t\t\\bottomrule",
      "\t\t\\end{tabular}",
      "\t\t\\begin{minipage}{\\textwidth}",
      "\t\t\t\\small",
      sprintf("\t\t\t\\textbf{Nota:} Error de muestreo: %.0f\\%%. ", error_muestreo * 100),
      "\t\t\tIC 95\\% según NOM-152-SEMARNAT-2023.",
      sprintf("\t\t\tSitios muestreados: %d. ", n_sitios_total),
      "\t\t\tMuertos no incluyen tocones. Volumen no aplica a muertos.",
      "\t\t\\end{minipage}",
      "\t\\end{table}",
      "\\end{landscape}"
    )
    
    writeLines(latex1_content, "tablas_latex/analisis_dasometrico_por_especie.tex")
    cat("  ✓ LaTeX: analisis_dasometrico_por_especie.tex\n")
    
    # -------------------------------------------------------------------------
    # TABLA 2 LATEX: POR GÉNERO
    # -------------------------------------------------------------------------
    
    tabla2_latex <- tabla2_final %>%
      mutate(
        UMM = as.character(rodal),
        Género = genero_grupo,
        Densidad = ifelse(
          is.na(dens_ic_inf),
          sprintf("%.1f", dens_media),
          sprintf("%.1f (%.1f -- %.1f)", dens_media, dens_ic_inf, dens_ic_sup)
        ),
        `AB (m²/ha)` = ifelse(
          is.na(ab_ic_inf),
          sprintf("%.2f", ab_media),
          sprintf("%.2f (%.2f -- %.2f)", ab_media, ab_ic_inf, ab_ic_sup)
        ),
        `Vol (m³/ha)` = ifelse(
          is.na(vol_ic_inf),
          sprintf("%.2f", vol_media),
          sprintf("%.2f (%.2f -- %.2f)", vol_media, vol_ic_inf, vol_ic_sup)
        )
      ) %>%
      select(UMM, Género, Densidad, `AB (m²/ha)`, `Vol (m³/ha)`)
    
    latex2_content <- c(
      "% ============================================================",
      "% TABLA 2: Análisis Dasométrico por Género",
      "% Solo árboles vivos",
      "% ============================================================",
      "\\begin{landscape}",
      "\t\\begin{table}[H]",
      "\t\t\\centering",
      "\t\t\\caption{Análisis dasométrico por género y UMM con IC 95\\% (Sección 4.9.2)}",
      "\t\t\\label{tab:analisis_dasometrico_genero}",
      "\t\t\\small",
      "\t\t\\begin{tabular}{llccc}",
      "\t\t\t\\toprule",
      "\t\t\t\\textbf{UMM} & \\textbf{Género} & ",
      "\t\t\t\\shortstack{\\textbf{Densidad} \\\\ \\textbf{(árb/ha)} \\\\ \\textbf{Media (IC 95\\%)}} &",
      "\t\t\t\\shortstack{\\textbf{Área Basal} \\\\ \\textbf{(m²/ha)} \\\\ \\textbf{Media (IC 95\\%)}} &",
      "\t\t\t\\shortstack{\\textbf{Volumen} \\\\ \\textbf{(m³/ha)} \\\\ \\textbf{Media (IC 95\\%)}} \\\\",
      "\t\t\t\\midrule"
    )
    
    # Agregar filas
    umm_actual <- NULL
    for (i in 1:nrow(tabla2_latex)) {
      fila <- tabla2_latex[i, ]
      
      # Separador entre UMMs
      if (!is.null(umm_actual) && fila$UMM != umm_actual && 
          fila$UMM != "TOTAL" && fila$Género != "Subtotal") {
        latex2_content <- c(latex2_content, "\t\t\t\\midrule")
      }
      
      # Formato
      if (fila$UMM == "TOTAL" || fila$Género %in% c("Subtotal", "TOTAL")) {
        linea <- sprintf(
          "\t\t\t\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
          fila$UMM, fila$Género, fila$Densidad, fila$`AB (m²/ha)`, fila$`Vol (m³/ha)`
        )
      } else {
        linea <- sprintf(
          "\t\t\t%s & \\textit{%s} & %s & %s & %s \\\\",
          fila$UMM, fila$Género, fila$Densidad, fila$`AB (m²/ha)`, fila$`Vol (m³/ha)`
        )
      }
      
      latex2_content <- c(latex2_content, linea)
      umm_actual <- fila$UMM
    }
    
    latex2_content <- c(
      latex2_content,
      "\t\t\t\\bottomrule",
      "\t\t\\end{tabular}",
      "\t\t\\begin{minipage}{\\textwidth}",
      "\t\t\t\\small",
      sprintf("\t\t\t\\textbf{Nota:} Error de muestreo: %.0f\\%%. ", error_muestreo * 100),
      "\t\t\tIC 95\\% según NOM-152-SEMARNAT-2023.",
      sprintf("\t\t\tSitios muestreados: %d. ", n_sitios_total),
      "\t\t\tSolo árboles vivos.",
      "\t\t\\end{minipage}",
      "\t\\end{table}",
      "\\end{landscape}"
    )
    
    writeLines(latex2_content, "tablas_latex/analisis_dasometrico_por_genero.tex")
    cat("  ✓ LaTeX: analisis_dasometrico_por_genero.tex\n")
  }
  
  # ==============================================================================
  # RESUMEN EN CONSOLA
  # ==============================================================================
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMEN ANÁLISIS DASOMÉTRICO                 ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("TABLA 1: Por Especie (primeras 10 filas):\n")
  cat("══════════════════════════════════════════════════════════\n")
  print(tabla1_final %>% 
          head(10) %>%
          select(rodal, nombre_cientifico, tipo, dens_media, ab_media, vol_media))
  
  cat("\n\nTABLA 2: Por Género (primeras 10 filas):\n")
  cat("══════════════════════════════════════════════════════════\n")
  print(tabla2_final %>% 
          head(10) %>%
          select(rodal, genero_grupo, dens_media, ab_media, vol_media))
  
  cat("\n✓ Análisis dasométrico completado\n\n")
  
  # Retornar resultados
  return(list(
    tabla1_especie = tabla1_final,
    tabla2_genero = tabla2_final,
    parametros = list(
      error_muestreo = error_muestreo,
      n_sitios_total = n_sitios_total,
      n_vivos = nrow(vivos),
      n_muertos = nrow(muertos),
      ic_nivel = 0.95
    )
  ))
}

cat("\n✓ Función analizar_dasometrico_con_ic() cargada\n")
cat("  TABLA 1: Por especie (vivos + línea de muertos por género)\n")
cat("  TABLA 2: Por género (solo vivos)\n")
cat("  - Muertos: solo densidad y AB, NO volumen\n")
cat("  - NO incluye tocones (dominancia 9)\n")
cat("  - Todas las variables con IC 95%\n")
cat("  - Error de muestreo: 10%\n\n")
