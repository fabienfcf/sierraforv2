# ==============================================================================
# TABLAS PARA PMF - SECCIONES 7.1, 7.2, 7.3
# ==============================================================================

library(tidyverse)
library(xtable)

# Función para formatear números con 3 cifras significativas
formatear_3_significativas <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ {
      # Primero reemplazar NAs
      x <- ifelse(is.na(.) | . == 0, 0, .)
      
      # Aplicar 3 cifras significativas
      formatted <- signif(x, 3)
      
      # Formatear adecuadamente según el tipo de número
      ifelse(formatted == round(formatted), 
             as.character(round(formatted)),  # Enteros sin decimales
             # Para decimales, determinar cuántos dígitos mostrar
             ifelse(abs(formatted) >= 100, sprintf("%.0f", formatted),
                    ifelse(abs(formatted) >= 10, sprintf("%.1f", formatted),
                           ifelse(abs(formatted) >= 1, sprintf("%.2f", formatted),
                                  sprintf("%.3f", formatted)))))
    }))
}

# ==============================================================================
# TABLA 5 COMPLETA: EXISTENCIAS POR UNIDAD MÍNIMA DE MANEJO
# Incluye: Existencias Reales, Intensidad de Corta, Residuales y Posibilidad
# ==============================================================================
# 
# Esta función genera la Tabla 5 completa según NOM-152-SEMARNAT-2023
# integrando resultados del cálculo ICA
#
# ==============================================================================

generar_tabla_5_completa_con_ica <- function(arboles_df, 
                                             ica_por_rodal, 
                                             ica_por_genero_rodal,
                                             config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  TABLA 5: Existencias por Unidad Mínima de Manejo        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # ==========================================================================
  # PASO 1: CALCULAR EXISTENCIAS REALES POR ESPECIE Y RODAL
  # ==========================================================================
  
  cat("[1/6] Calculando Existencias Reales por especie...\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)

  # Por especie (Pinus y Quercus)
  por_especie <- vivos %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    group_by(rodal, especie = genero_grupo) %>%
    summarise(
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_corta_ha),  # ✅ Superficie aprovechable
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      # Expandir a hectárea
      vol_ha_m3 = vol_muestreado_m3 / area_total_ha,
      ab_ha_m2 = ab_muestreada_m2 / area_total_ha,
      # Volumen total por rodal (usando superficie aprovechable)
      vol_umm_m3 = vol_ha_m3 * superficie_ha
    )
  
  # Separar Quercus y Pinus
  quercus <- por_especie %>%
    filter(especie == "Quercus") %>%
    select(rodal, 
           ER_Quercus_ha = vol_ha_m3,
           ER_Quercus_umm = vol_umm_m3,
           AB_Quercus_ha = ab_ha_m2)
  
  pinus <- por_especie %>%
    filter(especie == "Pinus") %>%
    select(rodal, 
           ER_Pinus_ha = vol_ha_m3,
           ER_Pinus_umm = vol_umm_m3,
           AB_Pinus_ha = ab_ha_m2)
  
  # Total por rodal
  por_rodal_total <- vivos %>%
    group_by(rodal) %>%
    summarise(
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_corta_ha),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      ER_total_ha = vol_muestreado_m3 / area_total_ha,
      ER_total_umm = ER_total_ha * superficie_ha,
      AB_total_ha = ab_muestreada_m2 / area_total_ha
    )
  
  # ==========================================================================
  # PASO 2: EXTRAER INTENSIDAD DE CORTA DEL ICA
  # ==========================================================================
  
  cat("[2/6] Extrayendo Intensidad de Corta del ICA...\n")
  
  # Intensidad de corta por rodal (promedio ponderado por volumen)
  intensidad_corta <- ica_por_genero_rodal %>%
    group_by(rodal) %>%
    summarise(
      # Intensidad de corta promedio ponderada por ER
      IntCor_rel_IC = weighted.mean(IntCor_rel_IC, ER_rodal_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      IntCor_pct = IntCor_rel_IC * 100  # Convertir a porcentaje
    )
  
  # ==========================================================================
  # PASO 3: CALCULAR POSIBILIDAD (VOLUMEN DE CORTA)
  # ==========================================================================
  
  cat("[3/6] Calculando Posibilidad (volumen de corta)...\n")
  
  # Posibilidad por especie
  posibilidad_especie <- ica_por_genero_rodal %>%
    select(rodal, genero, 
           VC_ha_m3, 
           VC_rodal_m3 = VC_rodal_m3) %>%
    pivot_wider(
      names_from = genero,
      values_from = c(VC_ha_m3, VC_rodal_m3),
      names_glue = "{.value}_{genero}"
    )
  
  # Posibilidad total por rodal
  posibilidad_total <- ica_por_rodal %>%
    select(rodal, 
           VC_total_ha = VC_ha_m3,
           VC_total_umm = VC_rodal_m3)
  
  # ==========================================================================
  # PASO 4: CALCULAR RESIDUALES (ER - VC)
  # ==========================================================================
  
  cat("[4/6] Calculando volúmenes residuales...\n")
  
  # Residuales por especie (necesitamos hacer join con ER)
  residuales_quercus <- quercus %>%
    left_join(
      posibilidad_especie %>% select(rodal, VC_ha_m3_Quercus),
      by = "rodal"
    ) %>%
    mutate(
      Residual_Quercus_ha = ER_Quercus_ha - coalesce(VC_ha_m3_Quercus, 0),
      AB_Residual_Quercus = AB_Quercus_ha * (Residual_Quercus_ha / ER_Quercus_ha)
    ) %>%
    select(rodal, Residual_Quercus_ha, AB_Residual_Quercus)
  
  residuales_pinus <- pinus %>%
    left_join(
      posibilidad_especie %>% select(rodal, VC_ha_m3_Pinus),
      by = "rodal"
    ) %>%
    mutate(
      Residual_Pinus_ha = ER_Pinus_ha - coalesce(VC_ha_m3_Pinus, 0),
      AB_Residual_Pinus = AB_Pinus_ha * (Residual_Pinus_ha / ER_Pinus_ha)
    ) %>%
    select(rodal, Residual_Pinus_ha, AB_Residual_Pinus)
  
  # Residuales totales
  residuales_total <- por_rodal_total %>%
    left_join(posibilidad_total, by = "rodal") %>%
    mutate(
      Residual_total_ha = ER_total_ha - VC_total_ha,
      AB_Residual_total = AB_total_ha * (Residual_total_ha / ER_total_ha)
    ) %>%
    select(rodal, Residual_total_ha, AB_Residual_total)
  
  # ==========================================================================
  # PASO 5: ENSAMBLAR TABLA COMPLETA
  # ==========================================================================
  
  cat("[5/6] Ensamblando tabla final...\n")
  
  tabla_5 <- por_rodal_total %>%
    select(rodal, superficie_ha) %>%
    # Existencias Reales - Quercus
    left_join(quercus, by = "rodal") %>%
    # Existencias Reales - Pinus
    left_join(pinus, by = "rodal") %>%
    # Existencias Reales - Total
    left_join(
      por_rodal_total %>% select(rodal, ER_total_ha, ER_total_umm, AB_total_ha),
      by = "rodal"
    ) %>%
    # Intensidad de Corta
    left_join(intensidad_corta, by = "rodal") %>%
    # Residuales - Quercus
    left_join(residuales_quercus, by = "rodal") %>%
    # Residuales - Pinus
    left_join(residuales_pinus, by = "rodal") %>%
    # Residuales - Total
    left_join(residuales_total, by = "rodal") %>%
    # Posibilidad
    left_join(posibilidad_especie, by = "rodal") %>%
    left_join(posibilidad_total, by = "rodal") %>%
    # Renombrar columnas según formato NOM-152
    select(
      No. = rodal,
      `Superficie (ha)` = superficie_ha,
      
      # EXISTENCIAS REALES - Quercus
      `ER Quercus m³/ha` = ER_Quercus_ha,
      `ER Quercus m³ UMM` = ER_Quercus_umm,
      `AB Quercus m²/ha` = AB_Quercus_ha,
      
      # EXISTENCIAS REALES - Pinus
      `ER Pinus m³/ha` = ER_Pinus_ha,
      `ER Pinus m³ UMM` = ER_Pinus_umm,
      `AB Pinus m²/ha` = AB_Pinus_ha,
      
      # EXISTENCIAS REALES - Total
      `ER Total m³/ha` = ER_total_ha,
      `ER Total m³ UMM` = ER_total_umm,
      `AB Total m²/ha` = AB_total_ha,
      
      # INTENSIDAD DE CORTA
      `Intensidad Corta (%) UMM` = IntCor_pct,
      
      # RESIDUALES - Quercus
      `Residual Quercus m³/ha` = Residual_Quercus_ha,
      `AB Residual Quercus m²/ha` = AB_Residual_Quercus,
      
      # RESIDUALES - Pinus
      `Residual Pinus m³/ha` = Residual_Pinus_ha,
      `AB Residual Pinus m²/ha` = AB_Residual_Pinus,
      
      # RESIDUALES - Total
      `Residual Total m³/ha` = Residual_total_ha,
      `AB Residual Total m²/ha` = AB_Residual_total,
      
      # POSIBILIDAD - Quercus
      `Posibilidad Quercus m³/ha` = VC_ha_m3_Quercus,
      `Posibilidad Quercus m³ UMM` = VC_rodal_m3_Quercus,
      
      # POSIBILIDAD - Pinus
      `Posibilidad Pinus m³/ha` = VC_ha_m3_Pinus,
      `Posibilidad Pinus m³ UMM` = VC_rodal_m3_Pinus,
      
      # POSIBILIDAD - Total
      `Posibilidad Total m³/ha` = VC_total_ha,
      `Posibilidad Total m³ UMM` = VC_total_umm
    )
  
  # Reemplazar NA por 0
  tabla_5 <- tabla_5 %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # ==========================================================================
  # PASO 6: AGREGAR SUBTOTALES Y TOTAL
  # ==========================================================================
  
  cat("[6/6] Agregando subtotales...\n")
  
  # Calcular subtotales
  subtotal <- tabla_5 %>%
    summarise(
      `No.` = "Subtotal",
      `Superficie (ha)` = sum(`Superficie (ha)`, na.rm = TRUE),
      across(starts_with("ER") & ends_with("m³ UMM"), ~ sum(., na.rm = TRUE)),
      across(starts_with("ER") & ends_with("m³/ha"), ~ weighted.mean(., `Superficie (ha)`, na.rm = TRUE)),
      across(starts_with("AB") & !contains("Residual"), ~ weighted.mean(., `Superficie (ha)`, na.rm = TRUE)),
      `Intensidad Corta (%) UMM` = weighted.mean(`Intensidad Corta (%) UMM`, `Superficie (ha)`, na.rm = TRUE),
      across(starts_with("Residual") & ends_with("m³/ha"), ~ weighted.mean(., `Superficie (ha)`, na.rm = TRUE)),
      across(starts_with("AB Residual"), ~ weighted.mean(., `Superficie (ha)`, na.rm = TRUE)),
      across(starts_with("Posibilidad") & ends_with("m³ UMM"), ~ sum(., na.rm = TRUE)),
      across(starts_with("Posibilidad") & ends_with("m³/ha"), ~ weighted.mean(., `Superficie (ha)`, na.rm = TRUE))
    )
  
  total <- subtotal %>%
    mutate(`No.` = "Total")
  
  # Ensamblar tabla final
  tabla_5_final <- bind_rows(tabla_5, subtotal, total)
  
  cat("\n✓ Tabla 5 generada exitosamente\n")
  cat(sprintf("  Rodales: %d\n", nrow(tabla_5)))
  cat(sprintf("  Columnas: %d\n", ncol(tabla_5_final)))
  
  return(tabla_5_final)
}

# ==============================================================================
# FUNCIÓN PARA EXPORTAR TABLA 5 A LaTeX
# ==============================================================================

exportar_tabla_5_latex <- function(tabla_5, archivo = "tablas_latex/tabla_5_existencias_umm.tex") {
  
  cat("\n[EXPORTANDO] Tabla 5 a LaTeX...\n")
  
  # Crear directorio si no existe
  dir.create(dirname(archivo), showWarnings = FALSE, recursive = TRUE)
  
  # Formatear números a 3 cifras significativas
  tabla_5_latex <- tabla_5 %>%
    mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 2)))
  
  # Crear tabla xtable
  xt <- xtable::xtable(
    tabla_5_latex,
    caption = "Existencias por unidad mínima de manejo (Tabla 5 - Sección 11.1.4)",
    label = "tab:existencias_umm",
    align = c("l", "c", rep("r", ncol(tabla_5_latex) - 1))
  )
  
  # Exportar
  print(xt,
        file = archivo,
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity,
        scalebox = 0.7,  # Escalar tabla para que quepa en página
        table.placement = "H")
  
  cat(sprintf("  ✓ Exportado a: %s\n\n", archivo))
}

# ==============================================================================
# WORKFLOW COMPLETO
# ==============================================================================

generar_tabla_5_workflow <- function(arboles_analisis, config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║       WORKFLOW: GENERAR TABLA 5 COMPLETA                  ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # 1. Calcular ICA
  cat("[1/3] Calculando ICA (10 años sin cortes)...\n")
  resultado_ica <- calcular_ica_sin_cortes(arboles_analisis, años = 10, config = config)
  
  # 2. Generar Tabla 5
  cat("\n[2/3] Generando Tabla 5 completa...\n")
  tabla_5 <- generar_tabla_5_completa_con_ica(
    arboles_analisis,
    resultado_ica$ica_por_rodal,
    resultado_ica$ica_por_genero_rodal,
    config
  )
  
  # 3. Exportar
  cat("\n[3/3] Exportando tabla...\n")
  
  # CSV
  write.csv(tabla_5, "resultados/tabla_5_existencias_umm.csv", row.names = FALSE)
  cat("  ✓ CSV: resultados/tabla_5_existencias_umm.csv\n")
  
  # LaTeX
  exportar_tabla_5_latex(tabla_5, "tablas_latex/tabla_5_existencias_umm.tex")
  
  # También guardar el objeto ICA completo
  saveRDS(resultado_ica, "resultados/resultado_ica_completo.rds")
  cat("  ✓ RDS: resultados/resultado_ica_completo.rds\n")
  
  cat("\n✓ Workflow completado\n\n")
  
  return(list(
    tabla_5 = tabla_5,
    ica = resultado_ica
  ))
}


# ==============================================================================
# TABLA 6: EXISTENCIAS POR CATEGORÍA DIAMÉTRICA POR UMM
# ==============================================================================

tabla_6_existencias_por_categoria <- function(arboles_df, config = CONFIG) {
  
  cat("\n[TABLA 6] Existencias por categoría diamétrica...\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal, 
      breaks = config$clases_d,
      formato = "rango"
    ))
  
  # Por rodal y clase diamétrica
  por_rodal_clase <- vivos %>%
    group_by(rodal, clase_d) %>%
    summarise(
      n_arboles = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      vol_ha_m3 = expandir_a_hectarea(vol_muestreado_m3, area_total_ha),
      vol_total_m3 = vol_ha_m3 * superficie_ha,
      densidad_ha = expandir_a_hectarea(n_arboles, area_total_ha)
    )
  
  # Pivotar para formato de tabla
  tabla_6 <- por_rodal_clase %>%
    select(rodal, clase_d, vol_total_m3) %>%
    pivot_wider(
      names_from = clase_d,
      values_from = vol_total_m3,
      values_fill = 0,
      names_sort = TRUE
    ) %>%
    mutate(UMM = as.character(rodal)) %>%
    select(-rodal) %>%
    select(UMM, everything())
  
  # Agregar totales por clase
  totales <- tabla_6 %>%
    summarise(
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
    ) %>%
    mutate(UMM = "TOTAL")
  
  tabla_6 <- bind_rows(tabla_6, totales) %>%
    select(UMM, everything())
  
  # APLICAR FORMATEO FINAL con 3 cifras significativas
  tabla_6_formateada <- formatear_3_significativas(tabla_6)
  
  return(tabla_6_formateada)
}

# ==============================================================================
# TABLA 7: DENSIDADES E INCREMENTOS
# ==============================================================================

tabla_7_densidades_incrementos <- function(arboles_inicial, arboles_final, 
                                           ica_calculado, años = 10,
                                           config = CONFIG) {
  
  cat("\n[TABLA 7] Densidades e incrementos...\n")
  
  # Métricas iniciales
  inicial <- calcular_metricas_estado(arboles_inicial, config)
  
  # Métricas finales
  final <- calcular_metricas_estado(arboles_final, config)
  
  # Calcular cambios
  tabla_7 <- inicial %>%
    select(rodal, n_vivos_ini = n_vivos, vol_ha_ini = vol_ha_m3, 
           ab_ha_ini = ab_ha_m2, densidad_ha_ini = densidad_ha) %>%
    left_join(
      final %>%
        select(rodal, n_vivos_fin = n_vivos, vol_ha_fin = vol_ha_m3,
               ab_ha_fin = ab_ha_m2, densidad_ha_fin = densidad_ha),
      by = "rodal"
    ) %>%
    mutate(
      # Densidad promedio
      `N árboles/ha` = (densidad_ha_ini + densidad_ha_fin) / 2,
      
      # Área basal promedio
      `AB (m²/ha)` = (ab_ha_ini + ab_ha_fin) / 2,
      
      # ICA (del cálculo previo)
      `ICA (m³/ha/año)` = NA_real_,
      
      # IMA (volumen final / años)
      `IMA (m³/ha/año)` = vol_ha_fin / años,
      
      # Tiempo de paso (años para duplicar volumen a tasa ICA)
      `Tiempo paso (años)` = NA_real_
    )
  
  # Integrar ICA del cálculo
  if (!is.null(ica_calculado)) {
    tabla_7 <- tabla_7 %>%
      left_join(
        ica_calculado %>%
          select(rodal, ICA_m3_ha, ICA_rel_i),
        by = "rodal"
      ) %>%
      mutate(
        `ICA (m³/ha/año)` = ICA_m3_ha,
        
        # Tiempo de paso: ln(2) / ln(1 + i)
        `Tiempo paso (años)` = ifelse(ICA_rel_i > 0,
                                      log(2) / log(1 + ICA_rel_i),
                                      NA_real_)
      ) %>%
      select(-ICA_m3_ha, -ICA_rel_i)
  }
  
  tabla_7 <- tabla_7 %>%
    mutate(UMM = as.character(rodal)) %>%
    select(
      UMM,
      `N árboles/ha`,
      `AB (m²/ha)`,
      `Tiempo paso (años)`,
      `ICA (m³/ha/año)`,
      `IMA (m³/ha/año)`
    )
  
  # Agregar promedios
  promedios <- tabla_7 %>%
    summarise(
      UMM = "PROMEDIO",
      across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
    )
  
  tabla_7 <- bind_rows(tabla_7, promedios)
  
  # APLICAR FORMATEO FINAL con 3 cifras significativas
  tabla_7_formateada <- formatear_3_significativas(tabla_7)
  
  return(tabla_7_formateada)
}

# ==============================================================================
# EXPORTAR TODAS LAS TABLAS EN LATEX
# ==============================================================================

exportar_todas_tablas_pmf <- function(arboles_inicial, arboles_final = NULL,
                                      ica_calculado = NULL,
                                      vol_aprovechamiento = NULL,
                                      años = 10,
                                      directorio = "tablas_latex") {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          EXPORTANDO TABLAS PMF A LATEX                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Tabla 5: Existencias y aprovechamiento
  t5 <- tabla_5_existencias_aprovechamiento(arboles_inicial, CONFIG, vol_aprovechamiento)
  
  # Usar SANITIZE = FALSE porque ya están formateados como strings
  xt5 <- xtable(t5,
                caption = "Existencias reales y volumen de aprovechamiento por UMM (Sección 7.1)",
                label = "tab:existencias_aprovechamiento")
  
  print(xt5,
        file = file.path(directorio, "tabla_existencias_volumen_aprovechamiento_umm.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = function(x) x)  # NO sanitizar, ya viene formateado
  
  cat("  ✓ tabla_existencias_volumen_aprovechamiento_umm.tex\n")
  
  # Tabla 6: Categorías diamétricas
  t6 <- tabla_6_existencias_por_categoria(arboles_inicial, CONFIG)
  
  xt6 <- xtable(t6,
                caption = "Existencias por categoría diamétrica por UMM (m³)",
                label = "tab:existencias_diametricas")
  
  print(xt6,
        file = file.path(directorio, "tabla_existencias_categoria_diametrica_umm.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = function(x) x,
        scalebox = 0.8)
  
  cat("  ✓ tabla_existencias_categoria_diametrica_umm.tex\n")
  
  # Tabla 7: Densidades e incrementos (si hay datos finales)
  t7 <- NULL
  
  if (!is.null(arboles_final)) {
    t7 <- tabla_7_densidades_incrementos(arboles_inicial, arboles_final, 
                                         ica_calculado, años, CONFIG)
    
    xt7 <- xtable(t7,
                  caption = "Densidades e incrementos por UMM (Sección 7.3)",
                  label = "tab:densidades_incrementos")
    
    print(xt7,
          file = file.path(directorio, "tabla_densidades_incrementos_umm.tex"),
          include.rownames = FALSE,
          caption.placement = "top",
          booktabs = TRUE,
          sanitize.text.function = function(x) x)
    
    cat("  ✓ tabla_densidades_incrementos_umm.tex\n")
  }
  
  cat("\n✓ Todas las tablas PMF exportadas exitosamente\n\n")
  
  return(list(
    tabla_5 = t5,
    tabla_6 = t6,
    tabla_7 = t7
  ))
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de tablas PMF cargado\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Funciones disponibles:\n")
cat("  • tabla_5_existencias_aprovechamiento()\n")
cat("  • tabla_6_existencias_por_categoria()\n")
cat("  • tabla_7_densidades_incrementos()\n")
cat("  • exportar_todas_tablas_pmf()\n")
cat("══════════════════════════════════════════════════════════════\n\n")