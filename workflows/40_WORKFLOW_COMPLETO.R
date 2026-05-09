  # ==============================================================================
  # WORKFLOW COMPLETO: ANÁLISIS E INVENTARIO FORESTAL
  # PMF Las Alazanas 2026-2036
  # Actualizado para estructura de configuración modular
  # ==============================================================================
  
  # Limpiar entorno
  rm(list = ls())
  gc()
  
  # Configurar directorio
  setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5/")
  
  PROYECTO_ROOT <- getwd()
  cat(sprintf("\n✓ Directorio base: %s\n", PROYECTO_ROOT))
  
  # Librerías
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(xtable)
  library(patchwork)

  # ══════════════════════════════════════════════════════════════════════════════
  # CONTROL DE FASES — editar aquí qué calcular en esta ejecución
  # TRUE  = ejecutar       FALSE = omitir (usa resultados guardados en disco)
  #
  #   Fase              Qué hace                          Tiempo aprox  Requiere
  # ──────────────────────────────────────────────────────────────────────────────
  FASES <- list(
    importar     = TRUE,  # Leer Excel → arboles_analisis.rds       ~10s   Excel
    descriptivo  = TRUE,  # Análisis dasométrico completo            ~30s   importar|.rds
    incendio     = FALSE, # Riesgo de incendio (combustibles)        ~5s    importar|.rds
    ica          = TRUE,  # Simulación 10a sin cortas → ICA CSV      ~60s   importar|.rds
    simulacion   = TRUE,  # Simulación 10a con cortas → PMF          ~90s   ica|CSV
    tablas       = TRUE,  # Tablas 5-9, ICA, densidad esp. → LaTeX   ~30s   ica+simulacion|CSVs
    graficos     = TRUE,  # Gráficos distrib. diamétrica por UMM     ~15s   simulacion|CSVs
    fichas       = FALSE, # Fichas PDF por sitio (catálogo)          ~120s  importar|.rds
    fichas_umm   = FALSE  # Fichas PDF resumen por UMM               ~60s   importar|.rds
  )
  # ══════════════════════════════════════════════════════════════════════════════

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         WORKFLOW COMPLETO - PMF LAS ALAZANAS              ║\n")
  cat("║              Sistema de Gestión Forestal v2.0             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  # ==============================================================================
  # FASE 0: CARGAR CONFIGURACIÓN
  # ==============================================================================
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║                FASE 0: CARGAR CONFIGURACIÓN               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  # Esto carga automáticamente:
  # - 01_config_especies.R
  # - 02_config_codigos.R
  # - 03_config_simulacion.R
  # - 04_config_programa_cortas.R
  # Y valida todo el sistema
  
  source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
  
  cat("\n✓ Configuración global disponible en: CONFIG\n")
  cat("✓ Parámetros de corta: Q_FACTOR, TOLERANCIA_EQUILIBRIO, DMC\n")
  cat("✓ Especies, códigos y ecuaciones cargadas\n")
  
  # ==============================================================================
  # FASE 1+2: IMPORTACIÓN Y CONSTRUCCIÓN DEL DATASET
  # ==============================================================================

  if (FASES$importar) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║                   FASE 1: IMPORTACIÓN                     ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))

    inventario <- importar_inventario_completo(
      ruta_archivo = "inventario_forestal.xlsx",
      ruta_umm = "UMM_stats.csv"
    )

    cat("\n✓ Inventario importado:\n")
    cat(sprintf("  • F01 (sitios):         %d registros\n", nrow(inventario$f01)))
    cat(sprintf("  • F02 (regeneración):   %d árboles\n", nrow(inventario$f02)))
    cat(sprintf("  • F03 (árboles):        %d árboles\n", nrow(inventario$f03)))
    cat(sprintf("  • F04 (virutas):        %d registros\n", nrow(inventario$f04)))
    cat(sprintf("  • F05 (regeneración):   %d registros\n", nrow(inventario$f05)))
    cat(sprintf("  • F06 (combustibles):   %d sitios\n", nrow(inventario$f06)))

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║              FASE 2: CONSTRUCCIÓN DEL DATASET             ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))

    arboles_analisis <- construir_arboles_analisis(inventario, CONFIG)

    cat(sprintf("\n✓ Dataset construido:\n"))
    cat(sprintf("  • Árboles totales:   %d\n", nrow(arboles_analisis)))
    cat(sprintf("  • Árboles vivos:     %d\n",
                sum(es_arbol_vivo(arboles_analisis$dominancia))))
    cat(sprintf("  • Rodales:           %d\n", n_distinct(arboles_analisis$rodal)))
    cat(sprintf("  • Géneros:           %s\n",
                paste(unique(arboles_analisis$genero_grupo), collapse = ", ")))

    dir.create("datos_intermedios", showWarnings = FALSE)
    dir.create("resultados", showWarnings = FALSE)
    saveRDS(arboles_analisis, "datos_intermedios/arboles_analisis.rds")
    saveRDS(inventario, "datos_intermedios/inventario_completo.rds")
    write.csv(arboles_analisis, "resultados/arboles_analisis.csv", row.names = FALSE)
    cat("\n✓ Datos guardados en datos_intermedios/\n")
    cat(sprintf("  ✓ arboles_analisis.csv: %d árboles (todas las especies)\n", nrow(arboles_analisis)))

  } else {
    cat("\n[FASE 1+2 omitida] Cargando desde datos_intermedios/...\n")
    rds_path <- "datos_intermedios/arboles_analisis.rds"
    if (!file.exists(rds_path)) stop("❌ No existe arboles_analisis.rds. Activa FASES$importar = TRUE")
    arboles_analisis <- readRDS(rds_path)
    inventario       <- readRDS("datos_intermedios/inventario_completo.rds")
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
    cat(sprintf("  ✓ %d árboles cargados desde disco\n", nrow(arboles_analisis)))
  }
  
  # ==============================================================================
  # FASE 3: ANÁLISIS DESCRIPTIVO
  # ==============================================================================

  if (FASES$descriptivo) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║              FASE 3: ANÁLISIS DESCRIPTIVO                 ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    source(file.path(PROYECTO_ROOT, "analisis/20_analisis_descriptivo.R"))

    resultados_descriptivos <- analisis_descriptivo_completo(
      inventario = inventario,
      arboles_df = arboles_analisis,
      config = CONFIG
    )

    dir.create("resultados", showWarnings = FALSE)
    saveRDS(resultados_descriptivos, "resultados/analisis_descriptivo.rds")

    cat("\n✓ Análisis descriptivo guardado en resultados/\n")

  } else {
    cat("\n[FASE 3 omitida] Cargando analisis_descriptivo.rds desde disco si existe...\n")
    rds_desc <- "resultados/analisis_descriptivo.rds"
    if (file.exists(rds_desc)) {
      resultados_descriptivos <- readRDS(rds_desc)
      cat("  ✓ Análisis descriptivo cargado desde disco\n")
    }
  }
  
  # ==============================================================================
  # FASE 4: ANÁLISIS DE RIESGO DE INCENDIO
  # ==============================================================================

  if (FASES$incendio) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║           FASE 4: ANÁLISIS DE RIESGO DE INCENDIO         ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    source(file.path(PROYECTO_ROOT, "opcional/23_Main_incendio.R"))

  } else {
    cat("\n[FASE 4 omitida] Riesgo de incendio no recalculado.\n")
  }
  
  # ==============================================================================
  # FASE 4.5: CÁLCULO DE ICA (SIMULACIÓN SIN CORTAS)
  # Requerido por FASE 5 — el optimizador Liocourt usa resultados/31_ica_por_rodal.csv
  # ==============================================================================

  if (FASES$ica) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║         FASE 4.5: CÁLCULO DE ICA (SIN CORTAS)            ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
    source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
    source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
    source(file.path(PROYECTO_ROOT, "core/16_calcular_ica.R"))

    arboles_ica <- arboles_analisis %>%
      filter(genero_grupo %in% CONFIG$generos)

    cat(sprintf("  • Población para ICA: %d árboles (%d rodales)\n",
                nrow(arboles_ica), n_distinct(arboles_ica$rodal)))

    resultado_ica <- calcular_ica_sin_cortes(
      arboles_inicial = arboles_ica,
      config = CONFIG,
      años = CONFIG$periodo
    )

    exportar_tablas_latex_ica(resultado_ica, directorio = "tablas_latex")
    guardar_resultados_ica(resultado_ica, directorio = "resultados")

    cat("\n✓ ICA calculado y guardado en resultados/ y tablas_latex/\n")

  } else {
    cat("\n[FASE 4.5 omitida] Usando resultados/31_ica_por_rodal.csv desde disco.\n")
    if (!file.exists("resultados/31_ica_por_rodal.csv")) {
      stop("❌ resultados/31_ica_por_rodal.csv no existe. Activa FASES$ica = TRUE")
    }
  }

  # ==============================================================================
  # FASE 5: SIMULACIÓN 10 AÑOS (CON CORTAS)
  # ==============================================================================

  if (FASES$simulacion) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║              FASE 5: SIMULACIÓN 10 AÑOS                   ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    cat("\n[5.1] Cargando módulos de simulación...\n")
    source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
    source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
    source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
    source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))
    source(file.path(PROYECTO_ROOT, "core/14_optimizador_cortas.R"))

    cat("\n[5.2] Ejecutando simulación completa...\n")

    source(file.path(PROYECTO_ROOT, "simulaciones/30_SIMULACION_10AÑOS_COMPLETA.R"))

    cat("\n✓ Simulación 10 años completada\n")

  } else {
    cat("\n[FASE 5 omitida] Simulación con cortas no ejecutada.\n")
  }
  
  # ==============================================================================
  # FASE 6: GENERADORES DE TABLAS PMF
  # ==============================================================================

  if (FASES$tablas) {

    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║           FASE 6: GENERADORES DE TABLAS PMF               ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    # 53_TABLA_DENSIDAD_ESPECIES usa la variable 'arboles', no 'arboles_analisis'
    arboles <- arboles_analisis

    cat("\n[6.1] Tabla densidad por especie (Rodal / Sitio)...\n")
    source(file.path(PROYECTO_ROOT, "generadores/53_TABLA_DENSIDAD_ESPECIES.R"))

    cat("\n[6.2] Tabla ICA y posibilidad por UMM y género...\n")
    source(file.path(PROYECTO_ROOT, "generadores/Tabla_ICA_Posibilidad.R"))

    cat("\n[6.3] Tabla 5 — Existencias por UMM...\n")
    # TABLA_5 espera 'cortas' con rodal_cortado + ano_corta en el environment
    cortas <- read_csv("resultados/cortas_distribucion_diametrica.csv", show_col_types = FALSE)
    source(file.path(PROYECTO_ROOT, "generadores/TABLA_5_COMPLETA_CON_ICA.R"))

    cat("\n[6.4] Tabla 6 — Resumen existencias predio...\n")
    source(file.path(PROYECTO_ROOT, "generadores/TABLA_6.R"))

    cat("\n[6.5] Tabla 7 — Densidad e incrementos (NOM-152)...\n")
    source(file.path(PROYECTO_ROOT, "generadores/TABLA_7_densidad e incrementos.R"))

    cat("\n[6.6] Tablas 8 y 9 — Posibilidad e infraestructura...\n")
    source(file.path(PROYECTO_ROOT, "generadores/TABLA_8 y 9 - Posibilidad Infraestructura.R"))

    cat("\n[6.7] Tabla Anexa — Distribución diamétrica de cortas por UMM...\n")
    source(file.path(PROYECTO_ROOT, "generadores/TABLA ANEXA - DISTRIBUCIÓN DIAMÉTRICA DE CORTAS POR UMM.R"))

    cat("\n✓ Todas las tablas PMF generadas\n")

  } else {
    cat("\n[FASE 6 omitida] Tablas PMF no regeneradas.\n")
  }

  # FASE 6.5: GRÁFICOS DISTRIBUCIÓN DIAMÉTRICA POR UMM
  # Requiere: simulacion|CSVs (historial_completo_10anos.rds, registro_cortas.rds)
  # ──────────────────────────────────────────────────────────────────────────────
  if (FASES$graficos) {
    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║     FASE 6.5: GRÁFICOS DISTRIBUCIÓN DIAMÉTRICA            ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")
    source(file.path(PROYECTO_ROOT, "generadores/60_graficos_distrib_ini_fin_corta_V2.R"))
  } else {
    cat("\n[FASE 6.5 omitida] Gráficos diamétricos no regenerados.\n")
  }

  # FASE 7: FICHAS PDF POR SITIO
  # Requiere: importar|.rds  +  analisis_riesgo_incendio_completo.csv  +  fotos/mapas
  # ──────────────────────────────────────────────────────────────────────────────
  if (FASES$fichas) {
    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║         FASE 7: FICHAS PDF POR SITIO (CATÁLOGO)           ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")
    source(file.path(PROYECTO_ROOT, "analisis/Fichas.R"))
  } else {
    cat("\n[FASE 7 omitida] Fichas PDF no regeneradas.\n")
  }

  # FASE 8: FICHAS PDF RESUMEN POR UMM
  # Requiere: importar|.rds  +  analisis_riesgo_incendio_completo.csv  +  fotos/mapas_umm
  # ──────────────────────────────────────────────────────────────────────────────
  if (FASES$fichas_umm) {
    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║        FASE 8: FICHAS PDF RESUMEN POR UMM                 ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")
    source(file.path(PROYECTO_ROOT, "analisis/FichasUMM.R"))
  } else {
    cat("\n[FASE 8 omitida] Fichas UMM no regeneradas.\n")
  }

  # ==============================================================================
  # RESUMEN FINAL
  # ==============================================================================
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║                  ✓ WORKFLOW COMPLETADO                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("ESTRUCTURA DE ARCHIVOS GENERADA:\n")
  cat("══════════════════════════════════════════════════════════\n\n")
  
  cat("📁 datos_intermedios/\n")
  cat("   ├── arboles_analisis.rds\n")
  cat("   └── inventario_completo.rds\n\n")
  
  cat("📁 resultados/\n")
  cat("   ├── analisis_descriptivo.rds\n")
  cat("   ├── historial_completo_10años.rds\n")
  cat("   ├── metricas_10años.rds\n")
  cat("   └── registro_cortas.rds\n\n")
  
  cat("📁 tablas_latex/\n")
  cat("   ├── desc_01_resumen_rodal.tex\n")
  cat("   ├── desc_02_composicion_genero.tex\n")
  cat("   ├── desc_03_top_especies.tex\n")
  cat("   ├── desc_04_erosion.tex\n")
  cat("   ├── desc_05_sanidad.tex\n")
  cat("   ├── desc_06_regeneracion.tex\n")
  cat("   ├── 01_inventario_inicial.tex\n")
  cat("   ├── 02_comparacion_inicial_final.tex\n")
  cat("   ├── 03_intensidad_corte_rodal.tex\n")
  cat("   └── ...\n\n")
  
  cat("📁 graficos/\n")
  cat("   ├── desc_01_distribucion_diametrica.png\n")
  cat("   ├── desc_02_erosion.png\n")
  cat("   ├── desc_03_sanidad.png\n")
  cat("   ├── desc_04_regeneracion.png\n")
  cat("   ├── evolucion_10años_rodales.png\n")
  cat("   └── ...\n\n")
  
  cat("══════════════════════════════════════════════════════════\n")
  cat("PARÁMETROS UTILIZADOS:\n")
  cat("══════════════════════════════════════════════════════════\n\n")
  
  cat(sprintf("Periodo simulación:      %d años\n", CONFIG$periodo))
  cat(sprintf("Q-factor (Liocourt):     %.2f\n", CONFIG$q_factor))
  cat(sprintf("Tolerancia equilibrio:   ±%d%%\n", CONFIG$tolerancia))
  cat(sprintf("Mortalidad base:         %.2f%%\n", CONFIG$mortalidad_base * 100))
  cat(sprintf("Reclutamiento:           %.1f%%\n", CONFIG$tasa_reclutamiento * 100))
  cat(sprintf("Rodales con corta:       %d\n", nrow(CONFIG$programa_cortas)))
  
  cat("\n══════════════════════════════════════════════════════════\n")
  cat("Para incluir las tablas en tu PMF (LaTeX):\n")
  cat("  \\input{tablas_latex/desc_01_resumen_rodal.tex}\n")
  cat("  \\input{tablas_latex/01_inventario_inicial.tex}\n")
  cat("  ... etc.\n\n")
  
  cat("Para visualizar los gráficos:\n")
  cat("  \\includegraphics{graficos/desc_01_distribucion_diametrica.png}\n\n")
  
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║          Sistema listo para generar PMF completo          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")