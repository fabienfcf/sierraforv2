# ==============================================================================
# SIMULACIÓN 10 ANOS - CORREGIDA
# ==============================================================================

# Al inicio de 30_SIMULACION_10ANOS_COMPLETA.R
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)


# Verificar/crear directorios
dirs <- c("datos_intermedios", "resultados", "graficos", "tablas_latex")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Cargar módulos
cat("\n[1/6] Cargando módulos...\n")
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/14_optimizador_cortas.R"))  # ← Versión corregida

library(xtable)
library(gridExtra)
library(patchwork)

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("\n[2/6] Cargando datos iniciales...\n")
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("  Población inicial: %d árboles\n", nrow(arboles_inicial)))

# ==============================================================================
# SIMULACIÓN
# ==============================================================================

cat("\n[3/6] Simulando 10 anos...\n\n")

arboles_actual <- arboles_inicial
historial_completo <- list()
historial_metricas <- list()
registro_cortas <- list()

# Estado inicial
historial_completo[[1]] <- arboles_actual %>% mutate(ano_simulacion = 0)
historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
  mutate(ano_simulacion = 0)

# ==============================================================================
# SIMULATION LOOP - ADAPTÉE POUR NOUVELLE LOGIQUE
# ==============================================================================

# Simulación ano por ano
for (ano in 1:PERIODO_SIMULACION) {
  
  cat(sprintf("═══ ANO %d ═══\n", ano))
  
  # 1. CRECIMIENTO
  cat(sprintf("\n[ANO %d] Crecimiento...\n", ano))
  arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, CONFIG, ano)
  
  # 2. MORTALIDAD
  cat(sprintf("\n[ANO %d] Mortalidad...\n", ano))
  arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, CONFIG, ano)
  
  # 3. RECLUTAMIENTO
  cat(sprintf("\n[ANO %d] Reclutamiento...\n", ano))
  arboles_actual <- aplicar_reclutamiento(arboles_actual, CONFIG, ano)
  
  # 4. CORTAS
  rodales_cortar <- PROGRAMA_CORTAS %>%
    filter(ano_corta == ano) %>%
    pull(rodal)
  
  if (length(rodales_cortar) > 0) {
    
    cat(sprintf("\n[ANO %d] 🪓 CORTAS PROGRAMADAS\n", ano))
    cat(sprintf("  Rodales: %s\n\n", paste(rodales_cortar, collapse = ", ")))
    
    for (rodal_id in rodales_cortar) {
      
      # ✅ CAMBIO 1: Obtener configuración completa del rodal
      config_rodal <- PROGRAMA_CORTAS %>% 
        filter(rodal == rodal_id, ano_corta == ano) %>%
        slice(1)  # Por si hay duplicados
      
      # ✅ CAMBIO 2: Crear configuración con NUEVOS parámetros
      corte_config <- configurar_corte(
        metodo = config_rodal$metodo,
        intensidad_pct = config_rodal$intensidad_pct,
        proteger_maduros_pinus = config_rodal$proteger_maduros_pinus,      # NUEVO
        proteger_maduros_quercus = config_rodal$proteger_maduros_quercus,  # NUEVO
        proporcion_quercus = config_rodal$proporcion_quercus,     
        q_factor = Q_FACTOR,
        tolerancia = TOLERANCIA_EQUILIBRIO
      )
      
      # Filtrar árboles del rodal
      arboles_rodal <- arboles_actual %>% filter(rodal == rodal_id)
      arboles_rodal_inicial <- arboles_inicial %>% filter(rodal == rodal_id)
      
      # ✅ CAMBIO 3: Árbol año anterior (simplificado, ya no necesario para ICA)
      # El ICA viene del CSV, no se calcula dinámicamente
      arboles_rodal_ano_anterior <- NULL
      
      cat(sprintf("  ──── Rodal %d ────\n", rodal_id))
      
      # Calcular plan de cortas
      plan_cortas <- tryCatch({
        calcular_plan_cortas(
          arboles_rodal,
          CONFIG,
          arboles_rodal_inicial,
          arboles_rodal_ano_anterior,
          corte_config,
          ano_actual = ano
        )
      }, error = function(e) {
        cat(sprintf("  ❌ Error: %s\n", e$message))
        list(arboles_marcados = tibble(), resumen = tibble())
      })
      
      # ✅ Registrar corta CORRECTAMENTE
      if (nrow(plan_cortas$arboles_marcados) > 0) {
        
        # Asegurarse que las columnas existen
        arboles_cortados <- plan_cortas$arboles_marcados %>%
          mutate(
            ano_corta = ano,
            rodal_cortado = rodal_id,
            metodo_corta = config_rodal$metodo
          )
        
        # Verificar columnas críticas
        if (!"volumen_m3" %in% names(arboles_cortados)) {
          warning("⚠️ Columna 'volumen_m3' faltante en árboles cortados")
          arboles_cortados <- arboles_cortados %>%
            mutate(volumen_m3 = 0)
        }
        
        registro_cortas[[length(registro_cortas) + 1]] <- arboles_cortados
      }
      
      # Aplicar corta
      arboles_actual <- aplicar_cortas(arboles_actual, plan_cortas, ano_corta = ano)
      
      cat("\n")
    }
  }
  
  # 5. GUARDAR ESTADO
  historial_completo[[ano + 1]] <- arboles_actual %>% mutate(ano_simulacion = ano)
  historial_metricas[[ano + 1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
    mutate(ano_simulacion = ano)
}

# Consolidar
df_historial <- bind_rows(historial_completo)
df_metricas <- bind_rows(historial_metricas)

# ✅ Consolidar cortas con manejo seguro
if (length(registro_cortas) > 0) {
  df_cortas <- bind_rows(registro_cortas)
  
  # Verificar y limpiar
  if (!"rodal_cortado" %in% names(df_cortas)) {
    df_cortas <- df_cortas %>% mutate(rodal_cortado = rodal)
  }
  if (!"volumen_m3" %in% names(df_cortas)) {
    df_cortas <- df_cortas %>% mutate(volumen_m3 = 0)
  }
} else {
  df_cortas <- tibble()
}

cat("\n✓ Simulación completada\n")

# ==============================================================================
# GRÁFICOS
# ==============================================================================
cat("\n[4/6] Generando gráficos...\n")

# ==============================================================================
# MÉTRICAS POR GÉNERO DESDE ARBOLES INDIVIDUALES
# ==============================================================================

# Calcular métricas detalladas por género desde df_historial
metricas_por_genero <- df_historial %>%
  filter(es_arbol_vivo(dominancia)) %>%
  group_by(rodal, ano_simulacion, genero_grupo) %>%
  summarise(
    n_arboles = n(),
    dg_cm = sqrt(mean(diametro_normal^2, na.rm = TRUE)),
    d_sd_cm = sd(diametro_normal, na.rm = TRUE),
    h_media_m = mean(altura_total, na.rm = TRUE),
    vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
    # ✅ Usar columna existente (es la misma para todos los árboles de la UMM)
    num_sitios_total = first(num_muestreos_realizados),
    .groups = "drop"
  ) %>%
  mutate(
    area_muestreada_ha = num_sitios_total * CONFIG$area_parcela_ha,
    vol_ha_m3 = vol_total_m3 / area_muestreada_ha,
    densidad_ha = n_arboles / area_muestreada_ha
  )

# Pivotar para tener columnas por género
metricas_pivotadas <- metricas_por_genero %>%
  pivot_wider(
    id_cols = c(rodal, ano_simulacion),
    names_from = genero_grupo,
    values_from = c(densidad_ha, n_arboles, dg_cm, d_sd_cm, h_media_m, vol_ha_m3),
    names_glue = "{genero_grupo}_{.value}"
  ) %>%
  rename_with(
    ~str_replace(., "Pinus_", "pinus_") %>% 
      str_replace(., "Quercus_", "quercus_"),
    starts_with(c("Pinus_", "Quercus_"))
  )

# Combinar con métricas agregadas (volumen, densidad total)
evolucion_rodal <- df_metricas %>%
  group_by(rodal, ano_simulacion) %>%
  summarise(
    vol_ha = sum(vol_ha_m3, na.rm = TRUE),
    densidad_ha = sum(densidad_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(metricas_pivotadas, by = c("rodal", "ano_simulacion"))

# Mostrar primeras filas
cat("\n✓ Métricas por género calculadas\n")
cat("\nColumnas disponibles:\n")
print(names(evolucion_rodal))

cat("\nPrimeras filas (ejemplo):\n")
evolucion_rodal %>% 
  filter(ano_simulacion %in% c(0, 5, 10)) %>%
  select(rodal, ano_simulacion, vol_ha, 
         pinus_vol_ha_m3, quercus_vol_ha_m3,  # ← NUEVAS COLUMNAS
         pinus_dg_cm, quercus_dg_cm) %>%
  head(6) %>%
  print()

p_volumen <- ggplot(evolucion_rodal, aes(x = ano_simulacion, y = vol_ha, 
                                         color = factor(rodal), 
                                         group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(data = PROGRAMA_CORTAS, 
             aes(xintercept = ano_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución del volumen por rodal (10 anos)",
    subtitle = "Líneas verticales = anos de corta",
    x = "Ano", y = "Volumen (m³/ha)", color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_densidad <- ggplot(evolucion_rodal, aes(x = ano_simulacion, y = densidad_ha, 
                                          color = factor(rodal), 
                                          group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(data = PROGRAMA_CORTAS, 
             aes(xintercept = ano_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución de la densidad por rodal",
    x = "Ano", y = "Densidad (árboles/ha)", color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_combined <- p_volumen / p_densidad
ggsave("graficos/evolucion_10anos_rodales.png", p_combined, 
       width = 12, height = 10, dpi = 300)

cat("  ✓ Gráfico guardado\n")

# ==============================================================================
# TABLAS DE CORTE
# ==============================================================================

cat("\n[5/6] Calculando intensidad de corte...\n")

if (nrow(df_cortas) > 0) {
  cat(sprintf("  Árboles cortados: %d\n", nrow(df_cortas)))
  cat(sprintf("  Volumen cortado: %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
} else {
  cat("  ℹ️ No se registraron cortas\n")
}

# ==============================================================================
# EXPORTACIÓN LATEX
# ==============================================================================

cat("\n[6/6] Exportando tablas LaTeX...\n")

# Tabla 1: Inventario inicial
tabla_inicial <- arboles_inicial %>%
  filtrar_arboles_vivos() %>%
  group_by(rodal, genero_grupo) %>%
  summarise(
    n_arboles = n(),
    Vol_m3 = sum(volumen_m3, na.rm = TRUE),
    D_medio_cm = mean(diametro_normal, na.rm = TRUE),
    Dg_cm = sqrt(sum(diametro_normal^2) / n()),  # ← AHORA
    .groups = "drop"
  )

xtable_inicial <- xtable(tabla_inicial,
                         caption = "Inventario inicial por rodal y género",
                         label = "tab:inventario_inicial")

print(xtable_inicial,
      file = "tablas_latex/01_inventario_inicial.tex",
      include.rownames = FALSE,
      floating = TRUE,
      booktabs = TRUE)

cat("  ✓ Tablas exportadas\n")

# ==============================================================================
# EXPORTAR TABLAS DE CORTAS - CON COLUMNAS POR HA Y TOTAL UMM
# ==============================================================================

if (nrow(df_cortas) > 0) {
  
  cat("\n[EXPORTACION ADICIONAL] Tablas de cortas...\n")
  
  # ============================================================================
  # CSV 1: Resumen por rodal y genero - CON COLUMNAS POR HA Y TOTAL
  # ============================================================================
  
  resumen_corte_rodal <- df_cortas %>%
    group_by(rodal_cortado, ano_corta, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      dg_cm = sqrt(sum(diametro_normal^2) / n()),  # ← AHORA
      h_media_m = mean(altura_total, na.rm = TRUE),
      superficie_corta_ha = first(superficie_corta_ha),
      num_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = num_muestreos * CONFIG$area_parcela_ha,
      n_arboles_por_ha = n_arboles / area_muestreada_ha,
      n_arboles_umm = n_arboles_por_ha * superficie_corta_ha,
      vol_m3_ha = vol_cortado_m3 / area_muestreada_ha,
      vol_total_umm_m3 = vol_m3_ha * superficie_corta_ha
    )
  
  write_csv(resumen_corte_rodal, "resultados/cortas_resumen_rodal_genero.csv")
  cat("  OK cortas_resumen_rodal_genero.csv\n")
  
  # ============================================================================
  # CSV 2: Distribucion diametrica - CON COLUMNAS POR HA Y TOTAL
  # ============================================================================
  
  intensidad_corte <- df_cortas %>%
    mutate(
      clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
    ) %>%
    group_by(rodal_cortado, ano_corta, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      superficie_corta_ha = first(superficie_corta_ha),
      num_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = num_muestreos * CONFIG$area_parcela_ha,
      n_arboles_por_ha = n_arboles / area_muestreada_ha,
      n_arboles_umm = n_arboles_por_ha * superficie_corta_ha,
      vol_m3_ha = vol_cortado_m3 / area_muestreada_ha,
      vol_total_umm_m3 = vol_m3_ha * superficie_corta_ha
    )
  
  write_csv(intensidad_corte, "resultados/cortas_distribucion_diametrica.csv")
  cat("  OK cortas_distribucion_diametrica.csv\n")
  
  # ============================================================================
  # CSV 3: Detalle completo arbol por arbol
  # ============================================================================
  
  df_cortas_export <- df_cortas %>%
    select(
      arbol_id, rodal_cortado, ano_corta, genero_grupo, 
      diametro_normal, altura_total, volumen_m3, 
      dominancia, metodo_corta
    ) %>%
    mutate(
      clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
    )
  
  write_csv(df_cortas_export, "resultados/cortas_detalle_completo.csv")
  cat("  OK cortas_detalle_completo.csv\n")
  
  # ============================================================================
  # LaTeX 1: Resumen por rodal (SIN pivot_wider)
  # ============================================================================
  
  # En lugar de pivot_wider, mantener formato largo
  tabla_resumen <- resumen_corte_rodal %>%
    arrange(rodal_cortado, ano_corta, genero_grupo) %>%
    select(rodal_cortado, ano_corta, genero_grupo, 
           n_arboles, vol_cortado_m3, dg_cm)
  
  xtable_resumen <- xtable(
    tabla_resumen,
    caption = "Resumen de cortas por rodal y género",
    label = "tab:cortas_resumen",
    digits = c(0, 0, 0, 0, 0, 2, 2)  # 7 elementos para 6 columnas + rownames
  )
  
  print(xtable_resumen,
        file = "tablas_latex/03_intensidad_corte_rodal.tex",
        include.rownames = FALSE,
        floating = TRUE,
        booktabs = TRUE,
        sanitize.text.function = function(x) x)
  
  cat("  ✓ 03_intensidad_corte_rodal.tex\n")
  
  # ============================================================================
  # LaTeX 2: Distribución diamétrica por género
  # ============================================================================
  
  tabla_clase <- intensidad_corte %>%
    group_by(genero_grupo, clase_d) %>%
    summarise(
      n_total = sum(n_arboles),
      vol_total_m3 = sum(vol_cortado_m3),
      .groups = "drop"
    ) %>%
    arrange(genero_grupo, clase_d)
  
  xtable_clase <- xtable(
    tabla_clase,
    caption = "Distribución de cortas por género y clase diamétrica",
    label = "tab:cortas_clase_diametrica",
    digits = c(0, 0, 0, 0, 2)  # 5 elementos para 4 columnas
  )
  
  print(xtable_clase,
        file = "tablas_latex/04_corte_por_clase_diametrica.tex",
        include.rownames = FALSE,
        floating = TRUE,
        booktabs = TRUE,
        sanitize.text.function = function(x) x)
  
  cat("  ✓ 04_corte_por_clase_diametrica.tex\n")
  
  # ============================================================================
  # LaTeX 3: Tablas individuales por rodal
  # ============================================================================
  
  for (rodal_id in sort(unique(df_cortas$rodal_cortado))) {
    
    cortas_rodal <- intensidad_corte %>%
      filter(rodal_cortado == rodal_id) %>%
      select(ano_corta, genero_grupo, clase_d, n_arboles, vol_cortado_m3) %>%
      arrange(ano_corta, genero_grupo, clase_d)
    
    if (nrow(cortas_rodal) > 0) {
      
      # Calcular dinámicamente el número de columnas
      n_cols <- ncol(cortas_rodal)
      
      xtable_rodal <- xtable(
        cortas_rodal,
        caption = sprintf("Programa de cortas - Rodal %d", rodal_id),
        label = sprintf("tab:corta_rodal_%02d", rodal_id),
        digits = c(0, rep(0, n_cols - 1), 2)  # Última columna con 2 decimales
      )
      
      print(xtable_rodal,
            file = sprintf("tablas_latex/05_corta_rodal_%02d.tex", rodal_id),
            include.rownames = FALSE,
            floating = TRUE,
            booktabs = TRUE,
            sanitize.text.function = function(x) x)
      
      cat(sprintf("  ✓ 05_corta_rodal_%02d.tex\n", rodal_id))
    }
  }
  
  # ============================================================================
  # BONUS: Resumen consolidado total
  # ============================================================================
  
  resumen_total <- df_cortas %>%
    group_by(genero_grupo) %>%
    summarise(
      n_arboles_total = n(),
      vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
      dg_cm = sqrt(sum(diametro_normal^2) / n()),  # ← AHORA
      .groups = "drop"
    )
  
  write_csv(resumen_total, "resultados/cortas_resumen_total.csv")
  cat("  ✓ cortas_resumen_total.csv\n")
  
  cat("\n✓ Exportación de tablas de cortas completada\n\n")
  
  # Mostrar resumen en consola
  cat("RESUMEN DE CORTAS:\n")
  cat("═══════════════════════════════════════════════════════════\n")
  print(resumen_total)
  cat("\n")
}

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

saveRDS(df_historial, "resultados/historial_completo_10anos.rds")
saveRDS(df_metricas, "resultados/metricas_10anos.rds")
saveRDS(df_cortas, "resultados/registro_cortas.rds")

write_csv(evolucion_rodal, "resultados/evolucion_rodal_10anos.csv")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║         ✓ SIMULACIÓN COMPLETADA                          ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("RESULTADOS:\n")
cat(sprintf("  • Anos simulados:     10\n"))
cat(sprintf("  • Rodales:            %d\n", n_distinct(arboles_inicial$rodal)))

if (nrow(df_cortas) > 0) {
  cat(sprintf("  • Árboles cortados:   %d\n", nrow(df_cortas)))
  cat(sprintf("  • Volumen cortado:    %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
} else {
  cat("  • Sin cortas registradas\n")
}

cat("\nARCHIVOS:\n")
cat("  📁 resultados/\n")
cat("  📁 graficos/\n")
cat("  📁 tablas_latex/\n\n")