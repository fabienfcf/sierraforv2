# ==============================================================================
# 09_REPORTES_POR_RODAL.R
# Genera:
# 1. Plot distribución diamétrica Pinus/Quercus
# 2. PDF por rodal con: densidad especies, volumen especies, regeneración
# ==============================================================================

library(tidyverse)
library(gridExtra)
library(grid)

# Verificar dependencias
if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta: source('modelov5/01_parametros_configuracion.R')")
}

if (!exists("expandir_a_hectarea")) {
  source("modelov5/15_core_calculos.R")
}

# ==============================================================================
# 1. DISTRIBUCIÓN DIAMÉTRICA - SOLO PINUS Y QUERCUS
# ==============================================================================

plot_distribucion_diametrica_pinus_quercus <- function(arboles_df, config = CONFIG, 
                                                       exportar = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   DISTRIBUCIÓN DIAMÉTRICA - Solo Pinus y Quercus          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios <- n_distinct(vivos$muestreo)
  
  # FILTRAR SOLO PINUS Y QUERCUS
  dist_diametrica <- vivos %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal, 
      breaks = config$clases_d,
      formato = "rango"
    )) %>%
    filter(!is.na(clase_d)) %>%
    count(clase_d, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha)
    ) %>%
    group_by(genero_grupo) %>%
    mutate(
      proporcion_pct = (densidad_ha / sum(densidad_ha)) * 100
    ) %>%
    ungroup()
  
  cat("[DISTRIBUCIÓN DIAMÉTRICA - Solo Pinus y Quercus]\n")
  print(dist_diametrica)
  
  # PLOT CON PALETA ACCESIBLE PARA DALTÓNICOS
  p <- ggplot(dist_diametrica, 
              aes(x = clase_d, y = densidad_ha, fill = genero_grupo)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    scale_fill_manual(
      values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")  # Colores daltónicos
    ) +
    labs(
      title = "Distribución Diamétrica - Pinus y Quercus",
      subtitle = sprintf("Densidad/ha - Parcela: %.2f ha - n sitios: %d", 
                         config$area_parcela_ha, n_sitios),
      x = "Clase Diamétrica (cm)",
      y = "Densidad (árboles/ha)",
      fill = "Género"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
  
  if (exportar) {
    dir.create("graficos", showWarnings = FALSE, recursive = TRUE)
    ggsave("graficos/distribucion_diametrica_pinus_quercus.png", p,
           width = 10, height = 6, dpi = 300)
    cat("\n✓ Gráfico guardado: graficos/distribucion_diametrica_pinus_quercus.png\n")
  }
  
  return(list(
    tabla = dist_diametrica,
    grafico = p
  ))
}

# ==============================================================================
# 2. ANÁLISIS POR RODAL - PREPARAR DATOS
# ==============================================================================

preparar_datos_rodal <- function(rodal_id, arboles_df, f05, f01, config = CONFIG) {
  
  # ══════════════════════════════════════════════════════════════
  # A. DENSIDAD POR ESPECIE (todas las especies del rodal)
  # ══════════════════════════════════════════════════════════════
  
  vivos_rodal <- filtrar_arboles_vivos(arboles_df) %>%
    filter(rodal == rodal_id)
  
  n_sitios_rodal <- n_distinct(vivos_rodal$muestreo)
  
  densidad_especies <- vivos_rodal %>%
    count(nombre_cientifico, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios_rodal, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / sum(n_arboles_obs)) * 100
    ) %>%
    arrange(desc(densidad_ha))
  
  # ══════════════════════════════════════════════════════════════
  # B. VOLUMEN POR ESPECIE - SOLO PINUS Y QUERCUS
  # ══════════════════════════════════════════════════════════════
  
  volumen_especies <- vivos_rodal %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    group_by(genero_grupo, nombre_cientifico) %>%
    summarise(
      n_arboles_obs = n(),
      volumen_total_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios_rodal, config$area_parcela_ha),
      volumen_m3ha = expandir_a_hectarea(volumen_total_m3 / n_sitios_rodal, config$area_parcela_ha),
      vol_medio_m3 = volumen_total_m3 / n_arboles_obs
    ) %>%
    arrange(genero_grupo, desc(volumen_m3ha))
  
  # ══════════════════════════════════════════════════════════════
  # C. REGENERACIÓN POR CLASE DE ALTURA - SOLO PINUS Y QUERCUS
  # ══════════════════════════════════════════════════════════════
  
  # Sitios del rodal
  sitios_rodal <- f01 %>%
    filter(rodal == rodal_id) %>%
    pull(muestreo)
  
  n_sitios_rodal_f01 <- length(sitios_rodal)
  
  if (!exists("ESPECIES")) {
    stop("❌ ESPECIES no está cargado")
  }
  
  # Regeneración observada
  regen_obs <- f05 %>%
    filter(muestreo %in% sitios_rodal) %>%
    left_join(ESPECIES %>% select(codigo, nombre_cientifico, genero), 
              by = c("especie" = "codigo")) %>%
    filter(genero %in% c("Pinus", "Quercus"))
  
  # Expandir a formato largo por clase de altura
  if (nrow(regen_obs) > 0) {
    regen_por_clase <- regen_obs %>%
      select(muestreo, genero, nombre_cientifico, 
             frec_025_150, frec_151_275, frec_276_400) %>%
      pivot_longer(
        cols = starts_with("frec_"),
        names_to = "clase_altura",
        values_to = "frecuencia"
      ) %>%
      mutate(
        clase_altura = recode(
          clase_altura,
          "frec_025_150" = "0.25-1.50 m",
          "frec_151_275" = "1.51-2.75 m",
          "frec_276_400" = "2.76-4.00 m"
        ),
        densidad_ha = expandir_a_hectarea(frecuencia, config$area_parcela_regeneracion_ha)
      ) %>%
      group_by(genero, clase_altura) %>%
      summarise(
        n_sitios_obs = n_distinct(muestreo),
        densidad_media_ha = mean(densidad_ha),
        densidad_total_ha = sum(densidad_ha) / n_sitios_rodal_f01,  # Promedio sobre TODOS los sitios
        .groups = "drop"
      )
  } else {
    # Si no hay regeneración observada, crear tabla vacía
    regen_por_clase <- tibble(
      genero = character(),
      clase_altura = character(),
      n_sitios_obs = numeric(),
      densidad_media_ha = numeric(),
      densidad_total_ha = numeric()
    )
  }
  
  return(list(
    rodal_id = rodal_id,
    n_sitios = n_sitios_rodal,
    densidad_especies = densidad_especies,
    volumen_especies = volumen_especies,
    regeneracion = regen_por_clase
  ))
}

# ==============================================================================
# 3. GENERAR PLOTS PARA UN RODAL
# ==============================================================================

crear_plots_rodal <- function(datos_rodal) {
  
  rodal_id <- datos_rodal$rodal_id
  
  # ══════════════════════════════════════════════════════════════
  # PLOT 1: DENSIDAD POR ESPECIE (top 10)
  # ══════════════════════════════════════════════════════════════
  
  p1 <- datos_rodal$densidad_especies %>%
    head(10) %>%
    mutate(nombre_cientifico = fct_reorder(nombre_cientifico, densidad_ha)) %>%
    ggplot(aes(x = densidad_ha, y = nombre_cientifico, fill = genero_grupo)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(
      values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9", "Otros" = "#999999")
    ) +
    labs(
      title = sprintf("Rodal %s - Densidad por Especie (Top 10)", rodal_id),
      x = "Densidad (árboles/ha)",
      y = NULL,
      fill = "Género"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "italic")
    )
  
  # ══════════════════════════════════════════════════════════════
  # PLOT 2: VOLUMEN POR ESPECIE - Pinus y Quercus separados
  # ══════════════════════════════════════════════════════════════
  
  if (nrow(datos_rodal$volumen_especies) > 0) {
    p2 <- datos_rodal$volumen_especies %>%
      mutate(nombre_cientifico = fct_reorder(nombre_cientifico, volumen_m3ha)) %>%
      ggplot(aes(x = volumen_m3ha, y = nombre_cientifico, fill = genero_grupo)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(
        values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")
      ) +
      labs(
        title = sprintf("Rodal %s - Volumen por Especie (Pinus y Quercus)", rodal_id),
        x = "Volumen (m³/ha)",
        y = NULL,
        fill = "Género"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "italic")
      )
  } else {
    # Plot vacío si no hay datos
    p2 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Sin datos de volumen\npara Pinus/Quercus", 
               size = 5, color = "gray50") +
      theme_void()
  }
  
  # ══════════════════════════════════════════════════════════════
  # PLOT 3: REGENERACIÓN POR CLASE DE ALTURA
  # ══════════════════════════════════════════════════════════════
  
  if (nrow(datos_rodal$regeneracion) > 0) {
    p3 <- datos_rodal$regeneracion %>%
      mutate(
        clase_altura = factor(clase_altura, 
                              levels = c("0.25-1.50 m", "1.51-2.75 m", "2.76-4.00 m"))
      ) %>%
      ggplot(aes(x = clase_altura, y = densidad_total_ha, fill = genero)) +
      geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      scale_fill_manual(
        values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")
      ) +
      labs(
        title = sprintf("Rodal %s - Regeneración por Clase de Altura", rodal_id),
        subtitle = sprintf("Promedio sobre %d sitios (parcela 9 m²)", datos_rodal$n_sitios),
        x = "Clase de Altura",
        y = "Densidad (individuos/ha)",
        fill = "Género"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  } else {
    p3 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Sin datos de regeneración\npara Pinus/Quercus", 
               size = 5, color = "gray50") +
      theme_void()
  }
  
  return(list(p1 = p1, p2 = p2, p3 = p3))
}

# ==============================================================================
# 4. GENERAR PDF POR RODAL
# ==============================================================================

generar_pdf_rodal <- function(rodal_id, arboles_df, f05, f01, config = CONFIG, 
                              directorio = "reportes_rodales") {
  
  cat(sprintf("\n[Generando PDF para Rodal %s]\n", rodal_id))
  
  # Preparar datos
  datos <- preparar_datos_rodal(rodal_id, arboles_df, f05, f01, config)
  
  # Crear plots
  plots <- crear_plots_rodal(datos)
  
  # Crear directorio
  dir.create(directorio, showWarnings = FALSE, recursive = TRUE)
  
  # Nombre del archivo
  archivo_pdf <- file.path(directorio, sprintf("rodal_%s_resumen.pdf", rodal_id))
  
  # Generar PDF
  pdf(archivo_pdf, width = 11, height = 8.5)  # Tamaño carta horizontal
  
  # Título general
  grid.newpage()
  grid.text(
    sprintf("REPORTE RODAL %s", rodal_id),
    x = 0.5, y = 0.95,
    gp = gpar(fontsize = 20, fontface = "bold")
  )
  grid.text(
    sprintf("Número de sitios: %d | Parcela arbolado: %.2f ha | Parcela regeneración: 9 m²",
            datos$n_sitios, config$area_parcela_ha),
    x = 0.5, y = 0.90,
    gp = gpar(fontsize = 10, col = "gray40")
  )
  
  # Layout de 3 plots
  pushViewport(viewport(layout = grid.layout(3, 1, heights = c(0.32, 0.32, 0.32))))
  
  print(plots$p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(plots$p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(plots$p3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  
  dev.off()
  
  cat(sprintf("✓ PDF guardado: %s\n", archivo_pdf))
  
  return(archivo_pdf)
}

# ==============================================================================
# 5. GENERAR TODOS LOS PDFS
# ==============================================================================

generar_reportes_todos_rodales <- function(arboles_df, f05, f01, config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          GENERANDO REPORTES PDF POR RODAL                 ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  rodales <- sort(unique(arboles_df$rodal))
  
  cat(sprintf("Rodales a procesar: %s\n\n", paste(rodales, collapse = ", ")))
  
  archivos_generados <- map_chr(rodales, function(r) {
    generar_pdf_rodal(r, arboles_df, f05, f01, config)
  })
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              ✓ REPORTES COMPLETADOS                       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat(sprintf("Total de PDFs generados: %d\n", length(archivos_generados)))
  cat(sprintf("Ubicación: reportes_rodales/\n\n"))
  
  return(archivos_generados)
}

# ==============================================================================
# FUNCIÓN MAESTRA
# ==============================================================================

analisis_rodales_completo <- function(inventario, arboles_df, config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║       ANÁLISIS POR RODAL - PINUS Y QUERCUS                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  resultados <- list()
  
  # 1. Plot general de distribución diamétrica
  cat("\n[1/2] Generando plot distribución diamétrica Pinus/Quercus...\n")
  resultados$distribucion <- plot_distribucion_diametrica_pinus_quercus(arboles_df, config)
  
  # 2. PDFs por rodal
  cat("\n[2/2] Generando PDFs individuales por rodal...\n")
  resultados$pdfs <- generar_reportes_todos_rodales(arboles_df, inventario$f05, 
                                                    inventario$f01, config)
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║                ✓ ANÁLISIS COMPLETADO                      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  📊 Plot general: graficos/distribucion_diametrica_pinus_quercus.png\n")
  cat(sprintf("  📄 PDFs por rodal (%d): reportes_rodales/rodal_*.pdf\n", 
              length(resultados$pdfs)))
  cat("\n")
  
  return(resultados)
}

cat("\n✓ Módulo de reportes por rodal cargado\n")
cat("  Funciones principales:\n")
cat("    - plot_distribucion_diametrica_pinus_quercus(arboles, CONFIG)\n")
cat("    - generar_pdf_rodal(rodal_id, arboles, f05, f01, CONFIG)\n")
cat("    - analisis_rodales_completo(inventario, arboles, CONFIG)\n\n")
cat("  Contenido de cada PDF:\n")
cat("    1. Densidad por especie (top 10)\n")
cat("    2. Volumen por especie (solo Pinus/Quercus)\n")
cat("    3. Regeneración por clase de altura (solo Pinus/Quercus)\n\n")
