# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 53_TABLA_DENSIDAD_ESPECIES.R
# Genera dos pestañas en inventario_forestal.xlsx:
#
#   Resumen_Especies  — por Rodal (UMM): densidad con n_sitios FIJO
#                       (mismo método que calcular_metricas_por_especie)
#
#   Resumen_Sitios    — por Sitio de Muestreo (0-57): factor = 1/0.1 ha = 10
#
# Subtotales por género (Pinus verde, Quercus amarillo, otros azul)
# ==============================================================================

library(tidyverse)
library(openxlsx)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║     53 — TABLA DENSIDAD POR ESPECIE (RODAL / SITIO)       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 1. DEPENDENCIAS
# ==============================================================================

if (!exists("CONFIG")) {
  source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
}
if (!exists("importar_inventario_completo")) {
  source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))
}
if (!exists("filtrar_arboles_vivos")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}
if (!exists("calcular_metricas")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_metricas.R"))
}

# ==============================================================================
# 2. DATOS
# ==============================================================================

RUTA_XLSX <- file.path(PROYECTO_ROOT, "inventario_forestal.xlsx")
RUTA_UMM  <- file.path(PROYECTO_ROOT, "UMM_stats.csv")

if (!exists("arboles")) {
  cat("[datos] Construyendo dataset de árboles...\n")
  if (!exists("inventario")) {
    inventario <- importar_inventario_completo(RUTA_XLSX, RUTA_UMM)
  }
  arboles <- construir_arboles_analisis(inventario, CONFIG)
}

vivos <- filtrar_arboles_vivos(arboles)
FACTOR <- 1 / CONFIG$area_parcela_ha   # = 10

cat(sprintf("  Árboles vivos: %d  |  Sitios: %d  |  Rodales: %d\n\n",
            nrow(vivos), n_distinct(vivos$muestreo), n_distinct(vivos$rodal)))

# ==============================================================================
# 3. ORDEN DE GÉNEROS
# ==============================================================================

generos_presentes <- sort(unique(vivos$genero_grupo))
generos_ord <- c(
  intersect(c("Pinus", "Quercus"), generos_presentes),
  sort(setdiff(generos_presentes, c("Pinus", "Quercus")))
)

# ==============================================================================
# 4. HELPER: construir bloques de filas para una tabla
#    Recibe: data frame de árboles vivos ya filtrado + factor de expansión
#    Devuelve: lista(filas, tipo_fila, genero_fila)
# ==============================================================================

bloques_especie <- function(df_grupo, fe, cols_extra = list()) {
  filas <- list()
  tipos <- character(0)
  generos <- character(0)

  generos_g <- generos_ord[generos_ord %in% unique(df_grupo$genero_grupo)]
  n_gen <- length(generos_g)

  for (g in generos_g) {
    df_gen <- df_grupo %>% filter(genero_grupo == g)
    especies_g <- sort(unique(df_gen$nombre_cientifico))

    for (sp in especies_g) {
      df_sp <- df_gen %>% filter(nombre_cientifico == sp)
      fila <- c(cols_extra, list(
        Género  = g,
        Especie = sp,
        `N árboles`           = nrow(df_sp),
        `Densidad (árb/ha)`   = round(nrow(df_sp) * fe, 1),
        `Diámetro medio (cm)` = round(mean(df_sp$diametro_normal, na.rm = TRUE), 1),
        `Altura media (m)`    = round(mean(df_sp$altura_total,    na.rm = TRUE), 1),
        `Volumen (m³/ha)`     = round(sum(df_sp$volumen_m3, na.rm = TRUE) * fe, 3)
      ))
      filas  <- c(filas,  list(as_tibble(fila)))
      tipos  <- c(tipos,  "especie")
      generos <- c(generos, g)
    }

    # Subtotal del género si hay >1 especie o >1 género
    if (length(especies_g) > 1 || n_gen > 1) {
      fila <- c(cols_extra, list(
        Género  = g,
        Especie = paste0("Subtotal ", g),
        `N árboles`           = nrow(df_gen),
        `Densidad (árb/ha)`   = round(nrow(df_gen) * fe, 1),
        `Diámetro medio (cm)` = round(mean(df_gen$diametro_normal, na.rm = TRUE), 1),
        `Altura media (m)`    = round(mean(df_gen$altura_total,    na.rm = TRUE), 1),
        `Volumen (m³/ha)`     = round(sum(df_gen$volumen_m3, na.rm = TRUE) * fe, 3)
      ))
      filas  <- c(filas,  list(as_tibble(fila)))
      tipos  <- c(tipos,  "subtotal_genero")
      generos <- c(generos, g)
    }
  }

  list(filas = filas, tipos = tipos, generos = generos)
}

# ==============================================================================
# 5. TABLA A: RODAL POR RODAL
#    Densidad = n_arboles / (num_muestreos_realizados × area_parcela_ha)
#    Igual que calcular_metricas_por_especie() — n_sitios FIJO
# ==============================================================================

cat("[1/2] Construyendo tabla por Rodal...\n")

rodales_ord <- sort(unique(vivos$rodal))
umm_info    <- inventario$umm   # tiene SUPERFICIE, SUPERFICIE_CORTA, num_puntos

filas_r  <- list()
tipos_r  <- character(0)
gen_r    <- character(0)

for (r in rodales_ord) {
  df_rod  <- vivos %>% filter(rodal == r)
  n_sit   <- first(df_rod$num_muestreos_realizados)        # FIJO, como en utils_metricas
  fe_r    <- 1 / (n_sit * CONFIG$area_parcela_ha)
  sup_tot <- first(df_rod$superficie_total_ha)
  sup_cor <- first(df_rod$superficie_corta_ha)

  # Encabezado de rodal
  filas_r <- c(filas_r, list(tibble(
    Rodal   = paste0("RODAL ", r),
    Especie = paste0("Sup. total: ", round(sup_tot, 2),
                     " ha  |  Sup. aprovechable: ", round(sup_cor, 2),
                     " ha  |  Sitios: ", n_sit),
    `N árboles`           = NA_integer_,
    `Densidad (árb/ha)`   = NA_real_,
    `Diámetro medio (cm)` = NA_real_,
    `Altura media (m)`    = NA_real_,
    `Volumen (m³/ha)`     = NA_real_
  )))
  tipos_r <- c(tipos_r, "header_rodal")
  gen_r   <- c(gen_r,   as.character(r))

  b <- bloques_especie(df_rod, fe_r,
         cols_extra = list(Rodal = as.character(r)))
  filas_r <- c(filas_r, b$filas)
  tipos_r <- c(tipos_r, b$tipos)
  gen_r   <- c(gen_r,   b$generos)

  # Total rodal
  filas_r <- c(filas_r, list(tibble(
    Rodal   = as.character(r),
    Especie = paste0("TOTAL RODAL ", r),
    `N árboles`           = nrow(df_rod),
    `Densidad (árb/ha)`   = round(nrow(df_rod) * fe_r, 1),
    `Diámetro medio (cm)` = round(mean(df_rod$diametro_normal, na.rm = TRUE), 1),
    `Altura media (m)`    = round(mean(df_rod$altura_total,    na.rm = TRUE), 1),
    `Volumen (m³/ha)`     = round(sum(df_rod$volumen_m3, na.rm = TRUE) * fe_r, 3)
  )))
  tipos_r <- c(tipos_r, "total_rodal")
  gen_r   <- c(gen_r,   as.character(r))
}

# Total general (media entre rodales de su densidad/ha)
tot_r <- vivos %>%
  group_by(rodal) %>%
  summarise(
    n     = n(),
    n_sit = first(num_muestreos_realizados),
    vol   = sum(volumen_m3, na.rm = TRUE),
    sum_d = sum(diametro_normal),
    sum_h = sum(altura_total),
    .groups = "drop"
  ) %>%
  mutate(fe = 1 / (n_sit * CONFIG$area_parcela_ha))

filas_r <- c(filas_r, list(tibble(
  Rodal   = "TOTAL",
  Especie = "TOTAL GENERAL",
  `N árboles`           = sum(tot_r$n),
  `Densidad (árb/ha)`   = round(mean(tot_r$n * tot_r$fe), 1),
  `Diámetro medio (cm)` = round(sum(tot_r$sum_d) / sum(tot_r$n), 1),
  `Altura media (m)`    = round(sum(tot_r$sum_h) / sum(tot_r$n), 1),
  `Volumen (m³/ha)`     = round(mean(tot_r$vol * tot_r$fe), 3)
)))
tipos_r <- c(tipos_r, "total_general")
gen_r   <- c(gen_r,   "TOTAL")

tabla_rodal <- bind_rows(filas_r)
cat(sprintf("  → %d filas\n", nrow(tabla_rodal)))

# ==============================================================================
# 6. TABLA B: SITIO POR SITIO
#    Densidad = n_arboles_sitio × (1 / 0.1 ha) = n × 10  (factor FIJO)
# ==============================================================================

cat("[2/2] Construyendo tabla por Sitio de Muestreo...\n")

# Mapa rodal por muestreo (incluye sitios sin árboles vivos)
mapa_muestreos <- arboles %>%
  select(muestreo, rodal) %>%
  distinct() %>%
  arrange(rodal, muestreo)

filas_s  <- list()
tipos_s  <- character(0)
gen_s    <- character(0)
rodal_s  <- character(0)

for (i in seq_len(nrow(mapa_muestreos))) {
  m   <- mapa_muestreos$muestreo[i]
  r   <- mapa_muestreos$rodal[i]
  df_sit <- vivos %>% filter(muestreo == m)

  # Encabezado del sitio
  filas_s <- c(filas_s, list(tibble(
    Rodal   = as.character(r),
    Sitio   = as.character(m),
    Género  = NA_character_,
    Especie = NA_character_,
    `N árboles`           = NA_integer_,
    `Densidad (árb/ha)`   = NA_real_,
    `Diámetro medio (cm)` = NA_real_,
    `Altura media (m)`    = NA_real_,
    `Volumen (m³/ha)`     = NA_real_
  )))
  tipos_s <- c(tipos_s, "header_sitio")
  gen_s   <- c(gen_s,   "")
  rodal_s <- c(rodal_s, as.character(r))

  if (nrow(df_sit) == 0) {
    filas_s <- c(filas_s, list(tibble(
      Rodal = as.character(r), Sitio = as.character(m),
      Género = NA_character_, Especie = "(sin árboles vivos)",
      `N árboles` = 0L, `Densidad (árb/ha)` = 0,
      `Diámetro medio (cm)` = NA_real_, `Altura media (m)` = NA_real_,
      `Volumen (m³/ha)` = 0
    )))
    tipos_s <- c(tipos_s, "vacio")
    gen_s   <- c(gen_s,   "")
    rodal_s <- c(rodal_s, as.character(r))
    next
  }

  b <- bloques_especie(df_sit, FACTOR,
         cols_extra = list(Rodal = as.character(r), Sitio = as.character(m)))
  filas_s <- c(filas_s, b$filas)
  tipos_s <- c(tipos_s, b$tipos)
  gen_s   <- c(gen_s,   b$generos)
  rodal_s <- c(rodal_s, rep(as.character(r), length(b$tipos)))

  # Total del sitio
  filas_s <- c(filas_s, list(tibble(
    Rodal   = as.character(r),
    Sitio   = as.character(m),
    Género  = NA_character_,
    Especie = paste0("TOTAL SITIO ", m),
    `N árboles`           = nrow(df_sit),
    `Densidad (árb/ha)`   = round(nrow(df_sit) * FACTOR, 1),
    `Diámetro medio (cm)` = round(mean(df_sit$diametro_normal, na.rm = TRUE), 1),
    `Altura media (m)`    = round(mean(df_sit$altura_total,    na.rm = TRUE), 1),
    `Volumen (m³/ha)`     = round(sum(df_sit$volumen_m3, na.rm = TRUE) * FACTOR, 3)
  )))
  tipos_s <- c(tipos_s, "total_sitio")
  gen_s   <- c(gen_s,   "")
  rodal_s <- c(rodal_s, as.character(r))
}

# Total general
tot_s <- vivos %>%
  group_by(muestreo) %>%
  summarise(n = n(), vol = sum(volumen_m3, na.rm = TRUE),
            sum_d = sum(diametro_normal), sum_h = sum(altura_total),
            .groups = "drop")

filas_s <- c(filas_s, list(tibble(
  Rodal = "TOTAL", Sitio = "",
  Género = NA_character_, Especie = "TOTAL GENERAL",
  `N árboles`           = sum(tot_s$n),
  `Densidad (árb/ha)`   = round(mean(tot_s$n * FACTOR), 1),
  `Diámetro medio (cm)` = round(sum(tot_s$sum_d) / sum(tot_s$n), 1),
  `Altura media (m)`    = round(sum(tot_s$sum_h) / sum(tot_s$n), 1),
  `Volumen (m³/ha)`     = round(mean(tot_s$vol * FACTOR), 3)
)))
tipos_s <- c(tipos_s, "total_general")
gen_s   <- c(gen_s,   "")
rodal_s <- c(rodal_s, "TOTAL")

tabla_sitios <- bind_rows(filas_s)
cat(sprintf("  → %d filas\n\n", nrow(tabla_sitios)))

# ==============================================================================
# 7. ESCRITURA EN EXCEL
# ==============================================================================

cat("[Excel] Escribiendo hojas...\n")
wb  <- loadWorkbook(RUTA_XLSX)
brd <- "TopBottomLeftRight"

# Paleta por rodal
col_fondo <- c("1"="#E8F4FD","2"="#FEF9E7","3"="#EAFAF1",
               "4"="#FDEDEC","5"="#F4ECF7","6"="#FDF2E9")
col_hdr   <- c("1"="#1F618D","2"="#B7950B","3"="#1E8449",
               "4"="#922B21","5"="#7D3C98","6"="#CA6F1E")

# Estilos fijos
st_titulo  <- createStyle(fontSize=13, textDecoration="bold",
               halign="CENTER", fgFill="#1A3A1A", fontColour="#FFFFFF")
st_header  <- createStyle(fontColour="#FFFFFF", fgFill="#2E5E2E",
               halign="CENTER", valign="CENTER", textDecoration="bold",
               border=brd, wrapText=TRUE)
st_tot_gen <- createStyle(fgFill="#1A3A1A", fontColour="#FFFFFF",
               fontSize=11, textDecoration="bold", border=brd)
st_vacio   <- createStyle(fontColour="#999999", border=brd,
               textDecoration="italic")
st_sub     <- list(
  Pinus   = createStyle(fgFill="#C6EFCE", textDecoration="bold", border=brd),
  Quercus = createStyle(fgFill="#FFEB9C", textDecoration="bold", border=brd),
  otro    = createStyle(fgFill="#DDEBF7", textDecoration="bold", border=brd)
)
st_tot_bloque <- createStyle(fgFill="#2E5E2E", fontColour="#FFFFFF",
                  textDecoration="bold", border=brd)
st_num <- createStyle(numFmt="#,##0.0",   border=brd)
st_vol <- createStyle(numFmt="#,##0.000", border=brd)
st_int <- createStyle(numFmt="#,##0",     border=brd)

# ── Función interna: aplicar estilos a una hoja ──────────────────────────────
escribir_hoja <- function(wb, nombre, tabla, tipos, gen_vec, rodal_vec,
                          titulo_txt, anchos, col_num, col_vol) {
  if (nombre %in% names(wb)) removeWorksheet(wb, nombre)
  addWorksheet(wb, nombre)

  COLS <- ncol(tabla)
  writeData(wb, nombre, titulo_txt, startRow=1, startCol=1)
  mergeCells(wb, nombre, cols=1:COLS, rows=1)
  addStyle(wb, nombre, st_titulo, rows=1, cols=1:COLS, gridExpand=TRUE)

  writeData(wb, nombre, tabla, startRow=2, startCol=1, headerStyle=st_header)
  setRowHeights(wb, nombre, rows=2, heights=36)

  for (i in seq_along(tipos)) {
    fila_wb <- i + 2
    tf  <- tipos[i]
    gf  <- gen_vec[i]
    rf  <- rodal_vec[i]
    cf  <- col_fondo[rf]; if (is.na(cf)) cf <- "#FFFFFF"

    st <- if (tf == "header_rodal" || tf == "header_sitio") {
      ch <- col_hdr[rf]; if (is.na(ch)) ch <- "#2E5E2E"
      createStyle(fgFill=ch, fontColour="#FFFFFF",
                  textDecoration="bold", fontSize=11, border=brd)
    } else if (tf == "especie") {
      createStyle(fgFill=cf, border=brd)
    } else if (tf == "subtotal_genero") {
      switch(gf, Pinus=st_sub$Pinus, Quercus=st_sub$Quercus, st_sub$otro)
    } else if (tf %in% c("total_rodal", "total_sitio")) {
      st_tot_bloque
    } else if (tf == "total_general") {
      st_tot_gen
    } else {
      st_vacio
    }

    addStyle(wb, nombre, st, rows=fila_wb, cols=1:COLS, gridExpand=TRUE)
    if (!tf %in% c("header_rodal", "header_sitio")) {
      addStyle(wb, nombre, st_int, rows=fila_wb, cols=col_num[1])
      addStyle(wb, nombre, st_num, rows=fila_wb, cols=col_num[-1])
      addStyle(wb, nombre, st_vol, rows=fila_wb, cols=col_vol)
    }
  }

  setColWidths(wb, nombre, cols=1:COLS, widths=anchos)
  freezePane(wb, nombre, firstActiveRow=3)
  invisible(wb)
}

# Resumen_Especies (rodal por rodal)
# Columnas: Rodal | Especie | N árboles | Densidad | Diámetro | Altura | Volumen
wb <- escribir_hoja(wb, "Resumen_Especies", tabla_rodal,
  tipos_r, gen_r, gen_r,   # gen_r también funciona como rodal_vec para los headers
  "Densidad, Dimensiones y Volumen por Especie — Rodal por Rodal (F02, n_sitios fijo)",
  anchos  = c(14, 34, 12, 18, 21, 17, 16),
  col_num = c(3, 4, 5, 6),  # N árboles, densidad, diam, altura
  col_vol = 7
)

# Resumen_Sitios (sitio por sitio)
# Columnas: Rodal | Sitio | Género | Especie | N árboles | Densidad | Diámetro | Altura | Volumen
wb <- escribir_hoja(wb, "Resumen_Sitios", tabla_sitios,
  tipos_s, gen_s, rodal_s,
  "Densidad, Dimensiones y Volumen por Especie — Sitio de Muestreo por Sitio (F02, factor=10)",
  anchos  = c(8, 8, 14, 30, 12, 18, 21, 17, 16),
  col_num = c(5, 6, 7, 8),
  col_vol = 9
)

saveWorkbook(wb, RUTA_XLSX, overwrite = TRUE)

cat(sprintf("✓ Resumen_Especies: %d filas\n", nrow(tabla_rodal)))
cat(sprintf("✓ Resumen_Sitios:   %d filas\n", nrow(tabla_sitios)))
cat(sprintf("✓ Guardado en: %s\n\n", RUTA_XLSX))
