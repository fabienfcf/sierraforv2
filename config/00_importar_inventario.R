# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 00_IMPORTAR_INVENTARIO.R
# Importación limpia desde Excel usando funciones modernas
# Actualizado para nueva estructura de configuración modular
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)

cat("\n[IMPORTACIÓN] Cargando módulo de importación...\n")

# ==============================================================================
# FUNCIÓN AUXILIAR: LEER HOJA SIPLAFOR
# ==============================================================================

leer_hoja_siplafor <- function(ruta_archivo, nombre_hoja) {
  read_excel(ruta_archivo, 
             sheet = nombre_hoja,
             col_names = TRUE,
             .name_repair = "unique_quiet") %>%
    janitor::clean_names()
}

# ==============================================================================
# FUNCIÓN AUXILIAR: LIMPIAR NUMÉRICO
# ==============================================================================

limpiar_numerico <- function(x) {
  as.numeric(str_replace_all(as.character(x), "[^0-9.-]", ""))
}

# ==============================================================================
# F01: DATOS DE SITIO
# ==============================================================================

importar_f01 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f01 <- c(
    umafor = "x1_umafor",
    unidad_manejo = "x3_um",
    rodal = "x4_sitio",
    muestreo = "x5_sitio_i",
    muestreo_de = "x6_sitio_de",
    tamano = "x7_tam",
    fecha = "x8_fecha",
    brigada = "x9_brig",
    paraje = "x10_paraje",
    utm_x = "x11_utm_x_oeste",
    utm_y = "x12_utm_y_norte",
    datum = "x13_datum",
    asnm = "x14_asnm",
    pendiente = "x15_pend",
    exposicion = "x16_ex",
    compactacion = "x17_co",
    textura = "x18_te",
    material = "x19_mp",
    materia_organica = "x20_mo",
    ocochal = "x21_oc",
    uso_suelo = "x22_uas",
    uso_agricola = "x23_ua",
    uso_pecuario = "x24_up",
    erosion_laminar = "x25_el",
    erosion_canalillos = "x26_ec",
    erosion_carcavas = "x27_er",
    erosion_antropica = "x28_ea",
    accesibilidad = "x29_acc",
    perturbacion1 = "x30_per1",
    perturbacion2 = "x30_per2",
    perturbacion3 = "x30_per3",
    cobertura_arbustos = "x31_ca",
    cobertura_herbaceas = "x32_ch",
    cobertura_pastos = "x33_cp",
    cobertura_ocochal = "x34_coc",
    tratamiento_silvicola = "x35_ts",
    tratamiento_comp1 = "x36_tc1",
    tratamiento_comp2 = "x36_tc2",
    tratamiento_comp3 = "x36_tc3",
    observaciones = "x37_obs"
  )
  
  # Columnas numéricas
  cols_numericas <- c(
    "utm_x", "utm_y", "asnm", "pendiente", "materia_organica", "ocochal",
    "cobertura_arbustos", "cobertura_herbaceas", "cobertura_pastos", "cobertura_ocochal"
  )
  
  # Columnas enteras
  cols_enteras <- c(
    "unidad_manejo", "rodal", "muestreo", "muestreo_de", "tamano", "exposicion", 
    "compactacion", "textura", "material", "uso_suelo", "uso_agricola", "uso_pecuario",
    "erosion_laminar", "erosion_canalillos", "erosion_carcavas", "erosion_antropica", 
    "accesibilidad", "perturbacion1", "perturbacion2", "perturbacion3", 
    "tratamiento_silvicola", "tratamiento_comp1", "tratamiento_comp2", "tratamiento_comp3"
  )
  
  # Columnas para fill
  cols_fill <- c("umafor", "unidad_manejo", "rodal", "muestreo_de", "tamano", "fecha", "datum", "brigada")
  
  # Importar y procesar
  leer_hoja_siplafor(ruta_archivo, "F01") %>%
    rename(!!!nombres_f01) %>%
    fill(all_of(cols_fill), .direction = "down") %>%
    mutate(
      across(all_of(cols_numericas), as.numeric),
      across(all_of(cols_enteras), as.integer),
      fecha = as.Date(fecha)
    ) %>%
    select(1:37)
}

# ==============================================================================
# F02: REGENERACIÓN
# ==============================================================================

importar_f02 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f02 <- c(
    muestreo = "sitio",
    arbol = "x38_arbol",
    especie = "x39_esp",
    dominancia = "x40_do",
    diametro_normal = "x41_dn",
    altura_total = "x42_at",
    altura_copa = "x43_ac"
  )
  
  # Columnas numéricas
  cols_numericas <- c("diametro_normal", "altura_total", "altura_copa")
  
  # Columnas enteras
  cols_enteras <- c("especie", "dominancia")
  
  # Columnas para fill
  cols_fill <- c("muestreo", "especie")
  
  # Importar y tratar
  leer_hoja_siplafor(ruta_archivo, "F02") %>%
    rename(!!!nombres_f02) %>%
    fill(all_of(cols_fill), .direction = "down") %>%
    group_by(muestreo) %>%
    mutate(arbol = row_number()) %>%
    ungroup() %>%
    mutate(
      across(all_of(cols_numericas), limpiar_numerico),
      across(all_of(cols_enteras), as.integer)
    )
}

# ==============================================================================
# F03: ÁRBOLES MUESTREADOS (DATOS COMPLETOS)
# ==============================================================================

importar_f03 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f03 <- c(
    muestreo = "sitio",
    arbol = "x44_arbol",
    especie = "x45_esp",
    dominancia = "x46_do",
    diametro_normal = "x47_dn",
    altura_total = "x48_at",
    altura_copa = "x49_ac",
    diametro_copa_ns = "x50_dcns",
    diametro_copa_eo = "x51_dceo",
    dano_fisico1 = "x52_df1",
    ubicacion_dano1 = "x53_ub1",
    dano_fisico2 = "x54_df2",
    ubicacion_dano2 = "x55_ub2",
    sanidad = "x56_sa",
    calificacion_sanidad = "x57_cs",
    azimut = "x58_azt",
    distancia = "x59_dist"
  )
  
  # Columnas numéricas
  cols_numericas <- c(
    "diametro_normal", "altura_total", "altura_copa", "diametro_copa_ns",
    "diametro_copa_eo", "azimut", "distancia"
  )
  
  # Columnas enteras
  cols_enteras <- c(
    "arbol", "especie", "dominancia", "dano_fisico1", "ubicacion_dano1",
    "dano_fisico2", "ubicacion_dano2", "sanidad", "calificacion_sanidad"
  )
  
  # Columnas para fill
  cols_fill <- c("muestreo", "especie")
  
  # Importar y tratar
  leer_hoja_siplafor(ruta_archivo, "F03") %>%
    rename(!!!nombres_f03) %>%
    fill(all_of(cols_fill), .direction = "down") %>%
    group_by(muestreo) %>%
    mutate(arbol = row_number()) %>%
    ungroup() %>%
    mutate(
      across(all_of(cols_numericas), limpiar_numerico),
      across(all_of(cols_enteras), as.integer)
    )
}

# ==============================================================================
# F04: VIRUTAS (EDAD)
# ==============================================================================

importar_f04 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f04 <- c(
    muestreo = "sitio",
    viruta = "x60_viruta",
    especie = "x61_esp",
    dominancia = "x62_do",
    diametro_normal = "x63_dn",
    altura_total = "x64_at",
    numero_anillos = "x65_na",
    edad = "x66_edad",
    numero_anillos_25mm = "x67_na_25mm",
    long_10_anillos_mm = "x68_long_10anillos"
  )
  
  # Columnas numéricas (todas excepto las enteras)
  cols_numericas <- c(
    "diametro_normal", "altura_total", "numero_anillos", "edad",
    "numero_anillos_25mm", "long_10_anillos_mm"
  )
  
  # Columnas enteras
  cols_enteras <- c("muestreo", "viruta", "especie", "dominancia")
  
  # Columnas para fill
  cols_fill <- c("muestreo")
  
  # Importar y tratar
  leer_hoja_siplafor(ruta_archivo, "F04") %>%
    rename(!!!nombres_f04) %>%
    fill(all_of(cols_fill), .direction = "down") %>%
    group_by(muestreo) %>%
    mutate(viruta = row_number()) %>%
    ungroup() %>%
    mutate(
      across(all_of(cols_numericas), limpiar_numerico),
      across(all_of(cols_enteras), as.integer)
    )
}

# ==============================================================================
# F05: REGENERACIÓN (CLASES DE TAMAÑO)
# ==============================================================================

importar_f05 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f05 <- c(
    muestreo = "sitio",
    tamano_regeneracion = "x81_tam",
    distribucion = "x82_dist",
    especie = "x83_esp",
    frec_025_150 = "x84_frec_0_25",
    edad_media_025_150 = "x85_em_0_25",
    diametro_medio_025_150 = "x86_dm_0_25",
    frec_151_275 = "x84_frec_1_51",
    edad_media_151_275 = "x85_em_1_51",
    diametro_medio_151_275 = "x86_dm_1_51",
    frec_276_400 = "x84_frec_2_75",
    edad_media_276_400 = "x85_em_2_75",
    diametro_medio_276_400 = "x86_dm_2_75"
  )
  
  # Columnas numéricas
  cols_numericas <- c(
    "frec_025_150", "edad_media_025_150", "diametro_medio_025_150",
    "frec_151_275", "edad_media_151_275", "diametro_medio_151_275",
    "frec_276_400", "edad_media_276_400", "diametro_medio_276_400"
  )
  
  # Columnas enteras
  cols_enteras <- c("tamano_regeneracion", "distribucion", "especie")
  
  # Columnas para fill
  cols_fill <- c("muestreo", "tamano_regeneracion", "distribucion")
  
  # Importar y tratar
  leer_hoja_siplafor(ruta_archivo, "F05") %>%
    rename(!!!nombres_f05) %>%
    fill(all_of(cols_fill), .direction = "down") %>%
    mutate(
      across(all_of(cols_numericas), limpiar_numerico),
      across(all_of(cols_enteras), as.integer)
    )
}

# ==============================================================================
# F06: COMBUSTIBLES
# ==============================================================================

importar_f06 <- function(ruta_archivo) {
  
  # Definir nombres de columnas
  nombres_f06 <- c(
    muestreo = "sitio",
    pendiente_transecto = "x87_pend",
    espesor_hojarasca_3m = "x88_esp_3",
    espesor_hojarasca_6m = "x88_esp_6",
    espesor_hojarasca_9m = "x88_esp_9",
    altura_arbustos_6m = "x89_alt_ar_6",
    altura_arbustos_9m = "x89_alt_ar_9",
    altura_pastos_6m = "x89_alt_pa_6",
    altura_pastos_9m = "x89_alt_pa_9",
    altura_hierbas_6m = "x89_alt_hi_6",
    altura_hierbas_9m = "x89_alt_hi_9",
    cobertura_dosel_3m = "x90_cob_3",
    cobertura_dosel_6m = "x90_cob_6",
    cobertura_dosel_9m = "x90_cob_9",
    combustibles_finos = "x91_comb_f",
    combustibles_regulares = "x91_comb_r",
    combustibles_medianos = "x91_comb_m",
    combustibles_G_diam1 = "x91_comb_g_diam_1",
    combustibles_G_diam2 = "x91_comb_g_diam_2",
    combustibles_G_diam3 = "x91_comb_g_diam_3",
    combustibles_G_diam4 = "x91_comb_g_diam_4",
    combustibles_G_diam5 = "x91_comb_g_diam_5",
    combustibles_G_diam6 = "x91_comb_g_diam_6",
    combustibles_G_diam7 = "x91_comb_g_diam_7",
    combustibles_G_diam8 = "x91_comb_g_diam_8",
    combustibles_G_diam9 = "x91_comb_g_diam_9",
    combustibles_G_diam10 = "x91_comb_g_diam_10",
    combustibles_G_grad1 = "x91_comb_g_grad_1",
    combustibles_G_grad2 = "x91_comb_g_grad_2",
    combustibles_G_grad3 = "x91_comb_g_grad_3",
    combustibles_G_grad4 = "x91_comb_g_grad_4",
    combustibles_G_grad5 = "x91_comb_g_grad_5",
    combustibles_G_grad6 = "x91_comb_g_grad_6",
    combustibles_G_grad7 = "x91_comb_g_grad_7",
    combustibles_G_grad8 = "x91_comb_g_grad_8",
    combustibles_G_grad9 = "x91_comb_g_grad_9",
    combustibles_G_grad10 = "x91_comb_g_grad_10"
  )
  
  # Todas las columnas (excepto sitio) son numéricas para F06
  cols_numericas <- names(nombres_f06)[names(nombres_f06) != "muestreo"]
  
  # Importar y tratar
  leer_hoja_siplafor(ruta_archivo, "F06") %>%
    rename(!!!nombres_f06) %>%
    mutate(
      across(all_of(cols_numericas), as.numeric)
    )
}

# ==============================================================================
# FUNCIÓN PRINCIPAL: IMPORTAR TODO EL INVENTARIO
# ==============================================================================

importar_inventario_completo <- function(ruta_archivo, ruta_umm = NULL) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          IMPORTACIÓN DE INVENTARIO FORESTAL               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Importar todas las hojas
  cat("[1/7] Importando F01 (sitios)...\n")
  f01 <- importar_f01(ruta_archivo)
  
  cat("[2/7] Importando F02 (regeneración pequeña)...\n")
  f02 <- importar_f02(ruta_archivo)
  
  cat("[3/7] Importando F03 (árboles completos)...\n")
  f03 <- importar_f03(ruta_archivo)
  
  cat("[4/7] Importando F04 (virutas/edad)...\n")
  f04 <- importar_f04(ruta_archivo)
  
  cat("[5/7] Importando F05 (regeneración por clases)...\n")
  f05 <- importar_f05(ruta_archivo)
  
  cat("[6/7] Importando F06 (combustibles)...\n")
  f06 <- importar_f06(ruta_archivo)
  
  # Cargar info de rodales si existe
  if (!is.null(ruta_umm) && file.exists(ruta_umm)) {
    cat("[7/7] Cargando información de rodales (UMM)...\n")
    umm <- read_csv(ruta_umm, locale = locale(encoding = "latin1"), show_col_types = FALSE)
  } else {
    cat("[7/7] Sin información de rodales\n")
    umm <- NULL
  }
  
  cat("\n✓ Importación completada\n")
  cat(sprintf("  Sitios:            %d\n", nrow(f01)))
  cat(sprintf("  Árboles F02:       %d\n", nrow(f02)))
  cat(sprintf("  Árboles F03:       %d\n", nrow(f03)))
  cat(sprintf("  Virutas:           %d\n", nrow(f04)))
  cat(sprintf("  Regeneración:      %d registros\n", nrow(f05)))
  cat(sprintf("  Combustibles:      %d sitios\n", nrow(f06)))
  
  return(list(
    f01 = f01,
    f02 = f02,
    f03 = f03,
    f04 = f04,
    f05 = f05,
    f06 = f06,
    umm = umm
  ))
}

# ==============================================================================
# CONSTRUIR DATAFRAME DE ÁRBOLES PARA SIMULACIÓN - CON SUPERFICIE_CORTA
# ==============================================================================

construir_arboles_analisis <- function(inventario, config = CONFIG) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         CONSTRUCCIÓN DEL DATASET DE ANÁLISIS              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Verificar que CONFIG esté cargado
  if (!exists("CONFIG") && missing(config)) {
    stop("❌ CONFIG no está disponible. Ejecuta: source(file.path(PROYECTO_ROOT, 'config/01_parametros_configuracion.R'))")
  }
  
  # Verificar que tenemos las tablas necesarias de CONFIG
  if (!exists("ESPECIES")) {
    stop("❌ ESPECIES no está cargado. CONFIG debe estar inicializado correctamente.")
  }
  
  if (!exists("ECUACIONES_VOLUMEN")) {
    stop("❌ ECUACIONES_VOLUMEN no está cargado.")
  }
  
  cat("[1/5] Uniendo tablas F02 y F03...\n")
  
  # Unir F02 y F03
  arboles <- inventario$f02 %>%
    left_join(
      inventario$f03 %>% 
        select(-c(especie, dominancia, diametro_normal, altura_total, altura_copa)),
      by = c("muestreo", "arbol")
    )
  
  cat("[2/5] Agregando información de sitio...\n")
  
  # Agregar info de sitio
  arboles <- arboles %>%
    left_join(
      inventario$f01 %>% 
        select(rodal, muestreo, unidad_manejo, utm_x, utm_y, asnm,
               pendiente, exposicion, fecha, tamano),
      by = "muestreo"
    )
  
  cat("[3/5] Agregando información de especies...\n")
  
  # Agregar info de especies usando ESPECIES de CONFIG
  arboles <- arboles %>%
    left_join(
      ESPECIES,
      by = c("especie" = "codigo")
    ) %>%
    mutate(genero_grupo = genero) %>%
    mutate(
      arbol_id = paste0(
        "R", sprintf("%02d", rodal),
        "_M", sprintf("%03d", muestreo),
        "_A", sprintf("%03d", arbol)
      )
    )
  
  cat("[4/5] Calculando métricas dasométricas...\n")
  
  # Cargar core_calculos si no está
  if (!exists("calcular_area_basal")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  # Calcular área basal
  arboles <- arboles %>%
    mutate(area_basal = calcular_area_basal(diametro_normal))
  
  # Agregar parámetros de ecuaciones alométricas
  arboles <- arboles %>%
    left_join(ECUACIONES_VOLUMEN, by = "nombre_cientifico")
  
  # Calcular volúmenes
  cat("  → Calculando volúmenes con ecuaciones alométricas...\n")
  arboles <- calcular_volumenes_vectorizado(arboles)
  
  # Poner volumen = 0 para árboles muertos
  arboles <- arboles %>%
    mutate(volumen_m3 = if_else(!es_arbol_vivo(dominancia), 0, volumen_m3))
  
  cat("[5/5] Agregando información de rodales...\n")
  
  # ============================================================================
  # ✅ CAMBIO CRÍTICO: Agregar SUPERFICIE_CORTA además de SUPERFICIE total
  # ============================================================================
  
  if (!is.null(inventario$umm)) {

    arboles <- arboles %>%
      left_join(
        inventario$umm %>%
          select(
            id,
            num_puntos,
            `SUPERFICIE UMM`,
            `Superficie en Producción (ha)`,
            `Longitud corriente de agua (m)`,
            `Franja protectora de vegetación ribereña (ha)`,
            PEND_mean
          ) %>%
          rename(
            rodal                  = id,
            num_muestreos_realizados = num_puntos,
            superficie_total_ha    = `SUPERFICIE UMM`,
            superficie_corta_ha    = `Superficie en Producción (ha)`,
            longitud_corriente_m   = `Longitud corriente de agua (m)`,
            franja_ribereña_ha     = `Franja protectora de vegetación ribereña (ha)`,
            pendiente_media        = PEND_mean
          ),
        by = "rodal"
      )
    
    # Crear superficie_ha (mantener compatibilidad con código existente)
    # Por defecto = superficie_total_ha
    arboles <- arboles %>%
      mutate(
        superficie_ha = superficie_total_ha  # Para análisis descriptivos
      )
    
    cat(sprintf("  ✓ Superficie total agregada: %.2f ha\n", 
                sum(unique(arboles$superficie_total_ha), na.rm = TRUE)))
    cat(sprintf("  ✅ Superficie aprovechable agregada: %.2f ha\n", 
                sum(unique(arboles$superficie_corta_ha), na.rm = TRUE)))
    
  } else {
    warning("⚠️ No hay datos de UMM (superficie)")
  }
  
  # Estadísticas finales
  n_total <- nrow(arboles)
  n_vivos <- sum(es_arbol_vivo(arboles$dominancia))
  n_con_volumen <- sum(!is.na(arboles$volumen_m3) & arboles$volumen_m3 > 0)
  vol_total <- sum(arboles$volumen_m3, na.rm = TRUE)
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              ✓ DATASET CONSTRUIDO                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("RESUMEN:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat(sprintf("  • Árboles totales:        %d\n", n_total))
  cat(sprintf("  • Árboles vivos:          %d (%.1f%%)\n", 
              n_vivos, n_vivos/n_total*100))
  cat(sprintf("  • Con volumen calculado:  %d\n", n_con_volumen))
  cat(sprintf("  • Volumen total:          %.2f m³\n", vol_total))
  cat(sprintf("  • Rodales:                %d\n", n_distinct(arboles$rodal)))
  cat(sprintf("  • Especies:               %d\n", n_distinct(arboles$nombre_cientifico)))
  
  # Resumen por género
  resumen_genero <- filtrar_arboles_vivos(arboles) %>%
    count(genero_grupo, name = "n") %>%
    mutate(pct = n / sum(n) * 100)
  
  cat("\n  Composición por género:\n")
  for (i in 1:nrow(resumen_genero)) {
    cat(sprintf("    - %s: %d árboles (%.1f%%)\n",
                resumen_genero$genero_grupo[i],
                resumen_genero$n[i],
                resumen_genero$pct[i]))
  }
  
  cat("\n")
  
  return(arboles)
}

cat("\n✓ Módulo de importación cargado\n")
cat("  Funciones disponibles:\n")
cat("    - importar_inventario_completo(ruta, ruta_umm)\n")
cat("    - construir_arboles_analisis(inventario, CONFIG)\n\n")