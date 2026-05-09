# ==============================================================================
# FICHAS_UMM.R — Resumen por Unidad de Manejo (UMM)
# PMF Las Alazanas 2026-2036
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(scales)
})

if (!exists("PROYECTO_ROOT"))
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"

if (!exists("es_arbol_vivo"))
  source(file.path(PROYECTO_ROOT, "core", "15_core_calculos.R"))

# ==============================================================================
# CONSTANTES
# ==============================================================================

AREA_SITIO_HA <- 0.1      # cada sitio de muestreo = 0.1 ha (1 000 m²)
AREA_REGEN_HA <- 0.0009   # cuadrante regeneración = 9 m²
FACTOR_REGEN  <- 1 / AREA_REGEN_HA   # 1 111.11 pl/ha

DANOS <- c("0"="Sin daño","1"="Sin daño","2"="Viejo/resinado",
           "3"="Fuste nudoso","4"="Ladeado/torcido","5"="Descortezado",
           "6"="Puntiseco","7"="Anillado","8"="Fuste ovoide",
           "9"="Daño por cable","10"="Bifurcado")
SANIDAD <- c("1"="Sano","2"="Muérdago","3"="Barrenadores yemas",
             "4"="Descortezadores","5"="Defoliadores","6"="Paxtle")

cod_lbl <- function(cod, tabla) {
  k <- as.character(cod)
  ifelse(is.na(cod) | !k %in% names(tabla), NA_character_, tabla[k])
}

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("Cargando datos...\n")

if (!exists("arboles"))
  arboles <- read.csv(file.path(PROYECTO_ROOT, "resultados", "arboles_analisis.csv"),
                      stringsAsFactors = FALSE)

arboles <- arboles %>%
  mutate(
    estado      = ifelse(!es_arbol_vivo(dominancia), "muerto", "vivo"),
    dano1_lbl   = cod_lbl(dano_fisico1, DANOS),
    sanidad_lbl = cod_lbl(sanidad, SANIDAD)
  )

riesgo_raw <- read.csv(
  file.path(PROYECTO_ROOT, "analisis_riesgo_incendio_completo.csv"),
  stringsAsFactors = FALSE)

regen_raw <- read.csv(
  file.path(PROYECTO_ROOT, "resultados/desc_09_regeneracion_observada.csv"),
  stringsAsFactors = FALSE)

# Metadatos por UMM (una fila por UMM)
umm_meta <- arboles %>%
  distinct(rodal, num_muestreos_realizados,
           superficie_total_ha, superficie_corta_ha,
           longitud_corriente_m, franja_ribereña_ha) %>%
  mutate(
    area_ribera_ha = round(superficie_total_ha - superficie_corta_ha, 2),
    factor_exp     = superficie_corta_ha / (num_muestreos_realizados * AREA_SITIO_HA)
  ) %>%
  arrange(rodal)

# Tabla sitio → rodal
sitios_rodal <- arboles %>% distinct(muestreo, rodal)

# Añadir rodal al riesgo
riesgo_raw <- riesgo_raw %>% left_join(sitios_rodal, by = "muestreo")

# ==============================================================================
# DIRECTORIOS TEMPORALES
# ==============================================================================

tmp_dir <- "/tmp/fichas_umm"
for (d in c("mapas", "fotos", "graficos"))
  dir.create(file.path(tmp_dir, d), showWarnings = FALSE, recursive = TRUE)

MAPAS_UMM_DIR <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/Fichas por sitio de muestreo/mapas_umm"

cat("Copiando imágenes...\n")
for (f in list.files(MAPAS_UMM_DIR, pattern = "\\.jpg$", full.names = TRUE))
  file.copy(f, file.path(tmp_dir, "mapas", basename(f)), overwrite = TRUE)
for (f in list.files(file.path(PROYECTO_ROOT, "Fotos_sitios"), full.names = TRUE))
  file.copy(f, file.path(tmp_dir, "fotos", basename(f)), overwrite = TRUE)

# ==============================================================================
# HELPERS LATEX
# ==============================================================================

esc <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&",  "\\\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\\\%",  x, fixed = TRUE)
  x <- gsub("$",  "\\\\$",  x, fixed = TRUE)
  x <- gsub("#",  "\\\\#",  x, fixed = TRUE)
  x <- gsub("_",  "\\\\_",  x, fixed = TRUE)
  x
}

fnum <- function(x, d = 1)
  ifelse(is.na(x) | is.nan(x), "---",
         formatC(round(x, d), format = "f", digits = d))

fint <- function(x)
  ifelse(is.na(x), "---", formatC(as.integer(round(x)), format = "d", big.mark = ","))

riesgo_color <- function(cat) {
  switch(as.character(cat),
    "BAJO"     = "riesgobajo",
    "MODERADO" = "riesgomoderado",
    "ALTO"     = "riesgoalto",
    "EXTREMO"  = "riesgoextremo",
    "gris")
}

genus_rank <- function(g)
  case_when(g == "Pinus" ~ 1L, g == "Quercus" ~ 2L, TRUE ~ 3L)

# ==============================================================================
# GENERAR GRÁFICOS POR UMM
# ==============================================================================

generar_graficos_umm <- function(umm, vivos, area_muestreada_ha) {

  grp <- function(g) case_when(
    g == "Pinus"   ~ "Pinus",
    g == "Quercus" ~ "Quercus",
    TRUE           ~ "Otras esp."
  )

  colores <- c("Pinus" = "#2E7D32", "Quercus" = "#E65100", "Otras esp." = "#1565C0")

  # ---- Pie: volumen por grupo --------------------------------------------------
  vol_grp <- vivos %>%
    mutate(grp = grp(genero)) %>%
    group_by(grp) %>%
    summarise(vol = sum(volumen_m3, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct   = vol / sum(vol) * 100,
           label = sprintf("%s\n%.1f%%", grp, pct))

  p_vol <- ggplot(vol_grp, aes("", vol, fill = grp)) +
    geom_col(width = 1, color = "white", linewidth = 0.4) +
    coord_polar("y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              size = 3, fontface = "bold", color = "white") +
    scale_fill_manual(values = colores) +
    labs(title = "Volumen por género") +
    theme_void(base_size = 9) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 9))

  ggsave(file.path(tmp_dir, "graficos", sprintf("umm%d_pie_vol.png", umm)),
         p_vol, width = 7, height = 7, units = "cm", dpi = 150)

  # ---- Pie: individuos por grupo -----------------------------------------------
  n_grp <- vivos %>%
    mutate(grp = grp(genero)) %>%
    count(grp) %>%
    mutate(pct   = n / sum(n) * 100,
           label = sprintf("%s\n%.1f%%", grp, pct))

  p_n <- ggplot(n_grp, aes("", n, fill = grp)) +
    geom_col(width = 1, color = "white", linewidth = 0.4) +
    coord_polar("y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              size = 3, fontface = "bold", color = "white") +
    scale_fill_manual(values = colores) +
    labs(title = "Individuos por género") +
    theme_void(base_size = 9) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 9))

  ggsave(file.path(tmp_dir, "graficos", sprintf("umm%d_pie_n.png", umm)),
         p_n, width = 7, height = 7, units = "cm", dpi = 150)

  # ---- Distribución diamétrica Pinus y Quercus — densidad/ha ------------------
  breaks_diam  <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, Inf)
  labels_diam  <- c("5-10","10-15","15-20","20-25","25-30","30-35",
                    "35-40","40-45","45-50","50-60",">60")

  # Contar sobre TODOS los sitios del UMM (ausencia = 0, no exclusión del FE)
  dist_data <- vivos %>%
    filter(genero %in% c("Pinus", "Quercus")) %>%
    mutate(clase = cut(diametro_normal, breaks = breaks_diam,
                       labels = labels_diam, right = FALSE, include.lowest = TRUE)) %>%
    filter(!is.na(clase)) %>%
    count(genero, clase) %>%
    mutate(nha = expandir_a_hectarea(n, area_muestreada_ha))

  dist_path <- NULL
  if (nrow(dist_data) > 0) {
    p_dist <- ggplot(dist_data, aes(x = clase, y = nha, fill = genero)) +
      geom_col(position = "dodge", color = "white", linewidth = 0.3, width = 0.8) +
      scale_fill_manual(values = c("Pinus" = "#2E7D32", "Quercus" = "#E65100")) +
      labs(x = "Clase diamétrica (cm)", y = "Densidad (ind/ha)",
           title = "Distribución diamétrica — Pinus y Quercus",
           fill = NULL) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      theme_classic(base_size = 8) +
      theme(plot.title    = element_text(size = 9, face = "bold"),
            axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
            legend.position = "top",
            legend.text   = element_text(size = 8))

    dist_path <- file.path(tmp_dir, "graficos", sprintf("umm%d_dist_diam.png", umm))
    ggsave(dist_path, p_dist, width = 14, height = 7, units = "cm", dpi = 150)
  }

  list(
    pie_vol  = sprintf("graficos/umm%d_pie_vol.png", umm),
    pie_n    = sprintf("graficos/umm%d_pie_n.png", umm),
    dist     = if (!is.null(dist_path)) sprintf("graficos/umm%d_dist_diam.png", umm) else NULL
  )
}

# ==============================================================================
# GENERAR FICHA LATEX POR UMM
# ==============================================================================

generar_ficha_umm <- function(umm) {

  cat(sprintf("  UMM %d...\n", umm))

  meta      <- umm_meta %>% filter(rodal == umm)
  n_sitios  <- meta$num_muestreos_realizados
  sup_total <- meta$superficie_total_ha
  sup_corta <- meta$superficie_corta_ha
  sup_rib   <- meta$area_ribera_ha
  long_corr <- meta$longitud_corriente_m
  fexp      <- meta$factor_exp   # sup_corta / (n_sitios * 0.1)

  arboles_u <- arboles %>% filter(rodal == umm)
  vivos_u   <- arboles_u %>% filter(estado == "vivo")
  muertos_u <- arboles_u %>% filter(estado == "muerto")
  sitios_u  <- unique(arboles_u$muestreo)

  riesgo_u  <- riesgo_raw %>% filter(rodal == umm)
  regen_u   <- regen_raw  %>% filter(UMM  == umm)

  # ---- Gráficos --------------------------------------------------------------
  graficos <- generar_graficos_umm(umm, vivos_u, n_sitios * AREA_SITIO_HA)

  # ---- Mapa de la UMM -------------------------------------------------------
  mapa_fname <- sprintf("UMM%d.jpg", umm)
  mapa_rel   <- if (file.exists(file.path(tmp_dir, "mapas", mapa_fname)))
                  file.path("mapas", mapa_fname) else NULL

  # ---- Fotos: muestra aleatoria representativa (max 6, 2 por sitio max) -----
  fotos_por_sitio <- lapply(sitios_u, function(s) {
    fs <- list.files(file.path(tmp_dir, "fotos"),
                     pattern = sprintf("^Sitio_%02d[_.]", s),
                     full.names = FALSE)
    if (length(fs) > 0) file.path("fotos", fs) else character(0)
  })
  fotos_todas <- unlist(fotos_por_sitio)
  set.seed(umm * 42)
  fotos_sel <- if (length(fotos_todas) > 6) sample(fotos_todas, 6) else fotos_todas

  # ---- Tabla de composición arbórea ------------------------------------------
  # Factor expansion por ha = 1 / (n_sitios * AREA_SITIO_HA)
  fe_ha <- 1 / (n_sitios * AREA_SITIO_HA)

  por_especie <- vivos_u %>%
    group_by(nombre_cientifico, genero) %>%
    summarise(
      n      = n(),
      dn_m   = mean(diametro_normal, na.rm = TRUE),
      h_m    = mean(altura_total,    na.rm = TRUE),
      vol_p  = sum(volumen_m3,       na.rm = TRUE),   # vol en parcelas
      .groups = "drop"
    ) %>%
    mutate(
      n_ha    = n * fe_ha,           # individuos/ha
      n_umm   = n_ha * sup_corta,    # total en UMM (sin ribereñas)
      vol_ha  = vol_p * fe_ha,       # m³/ha
      vol_umm = vol_ha * sup_corta   # m³ totales
    ) %>%
    arrange(genus_rank(genero), nombre_cientifico)

  # Sub-función subtotal
  subtotal_grupo <- function(df) {
    if (nrow(df) == 0) return(NULL)
    tibble(
      n       = sum(df$n),
      dn_m    = weighted.mean(df$dn_m, df$n),
      h_m     = weighted.mean(df$h_m,  df$n),
      n_ha    = sum(df$n_ha),
      n_umm   = sum(df$n_umm),
      vol_ha  = sum(df$vol_ha),
      vol_umm = sum(df$vol_umm)
    )
  }

  sub_p <- subtotal_grupo(por_especie %>% filter(genero == "Pinus"))
  sub_q <- subtotal_grupo(por_especie %>% filter(genero == "Quercus"))
  sub_o <- subtotal_grupo(por_especie %>% filter(!genero %in% c("Pinus","Quercus")))
  sub_t <- subtotal_grupo(por_especie)

  # ---- Daños -----------------------------------------------------------------
  n_v <- nrow(vivos_u)
  arb_dano <- vivos_u %>% filter(!is.na(dano_fisico1) & dano_fisico1 > 1)
  arb_san  <- vivos_u %>% filter(!is.na(sanidad)      & sanidad > 1)
  pct_d    <- if (n_v > 0) nrow(arb_dano) / n_v * 100 else 0
  pct_s    <- if (n_v > 0) nrow(arb_san)  / n_v * 100 else 0

  top_dano <- arb_dano %>% count(dano1_lbl,   sort = TRUE) %>% slice_head(n = 3)
  top_san  <- arb_san  %>% count(sanidad_lbl, sort = TRUE) %>% slice_head(n = 3)

  # ---- Regeneración: TODOS los sitios, 0 si no hay datos -------------------
  # Total por sitio (sumando géneros)
  regen_sitio_total <- regen_u %>%
    group_by(muestreo) %>%
    summarise(n_ind = sum(num_individuos, na.rm = TRUE), .groups = "drop")

  regen_completa <- tibble(muestreo = sitios_u) %>%
    left_join(regen_sitio_total, by = "muestreo") %>%
    mutate(n_ind = replace_na(n_ind, 0))

  regen_dens_ha <- mean(regen_completa$n_ind) * FACTOR_REGEN

  # Por género (completando con 0 para sitios sin ese género)
  generos_u <- unique(regen_u$genero)
  if (length(generos_u) > 0) {
    regen_gen_por_sitio <- regen_u %>%
      group_by(muestreo, genero) %>%
      summarise(n = sum(num_individuos, na.rm = TRUE), .groups = "drop")

    regen_por_genero <- expand.grid(
      muestreo = sitios_u, genero = generos_u,
      stringsAsFactors = FALSE) %>%
      left_join(regen_gen_por_sitio, by = c("muestreo","genero")) %>%
      mutate(n = replace_na(n, 0)) %>%
      group_by(genero) %>%
      summarise(dens_ha = mean(n) * FACTOR_REGEN, .groups = "drop") %>%
      arrange(desc(dens_ha))
  } else {
    regen_por_genero <- tibble(genero = character(), dens_ha = numeric())
  }

  # ---- Riesgo incendio -------------------------------------------------------
  cats_n    <- riesgo_u %>% count(categoria_riesgo)
  idx_med   <- mean(riesgo_u$indice_riesgo, na.rm = TRUE)
  cat_modal <- cats_n %>% slice_max(n, n = 1) %>% pull(categoria_riesgo)

  carga_med <- riesgo_u %>%
    filter(!is.na(carga_total) & carga_total > 0) %>%
    summarise(c1   = mean(carga_1h,    na.rm = TRUE),
              c10  = mean(carga_10h,   na.rm = TRUE),
              c100 = mean(carga_100h,  na.rm = TRUE),
              c1k  = mean(carga_1000h, na.rm = TRUE),
              ctot = mean(carga_total, na.rm = TRUE),
              n_sitios_carga = n())

  # ==========================================================================
  # LATEX
  # ==========================================================================

  tex <- c(
    "\\newpage",
    "\\phantomsection",
    sprintf("\\addcontentsline{toc}{section}{UMM~%d}", umm),
    "\\begin{center}",
    sprintf("{\\LARGE\\bfseries\\color{verde_bosque} Unidad M\\'inima de Manejo~%d}", umm),
    "\\end{center}",
    "\\vspace{2pt}\\hrule\\vspace{5pt}"
  )

  # ---- Metadatos UMM --------------------------------------------------------
  tex <- c(tex,
    "\\begin{small}",
    "\\begin{tabular}{@{}ll@{\\qquad}ll@{\\qquad}ll@{}}",
    sprintf("\\textbf{Sup. total:} & %.2f~ha & \\textbf{Sup. producci\\'on:} & %.2f~ha & \\textbf{Franja ribere\\~na:} & %.2f~ha \\\\",
            sup_total, sup_corta, sup_rib),
    sprintf("\\textbf{Sitios muestreados:} & %d & \\textbf{Sup. muestreada:} & %.2f~ha & \\textbf{Long. corriente:} & %s~m \\\\",
            n_sitios, n_sitios * AREA_SITIO_HA,
            ifelse(is.na(long_corr) | long_corr == 0, "---", formatC(long_corr, format = "d", big.mark = ","))),
    "\\end{tabular}",
    "\\end{small}",
    "\\vspace{4pt}"
  )

  # ---- Mapa -----------------------------------------------------------------
  if (!is.null(mapa_rel)) {
    tex <- c(tex,
      "\\begin{center}",
      sprintf("\\includegraphics[width=10cm,keepaspectratio]{%s}", mapa_rel),
      "\\end{center}",
      "\\vspace{4pt}"
    )
  }

  # ---- Tabla composición arbórea --------------------------------------------
  tex <- c(tex,
    "\\subsection*{\\normalsize Composici\\'on arbórea (arbolado vivo)}",
    "\\vspace{-4pt}",
    "\\begin{center}\\begin{footnotesize}",
    "\\begin{longtable}{@{}>{}l r r rr rr@{}}",
    "\\toprule",
    paste0("Especie & N/ha & N UMM",
           " & $\\bar{D}_{1.3}$ (cm) & $\\bar{H}$ (m)",
           " & Vol/ha (m\\textsuperscript{3}) & Vol UMM (m\\textsuperscript{3}) \\\\"),
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    paste0("Especie & N/ha & N UMM",
           " & $\\bar{D}_{1.3}$ (cm) & $\\bar{H}$ (m)",
           " & Vol/ha (m\\textsuperscript{3}) & Vol UMM (m\\textsuperscript{3}) \\\\"),
    "\\midrule",
    "\\endhead",
    "\\bottomrule",
    "\\endlastfoot"
  )

  fila_esp <- function(r, indent = TRUE) {
    sp <- esc(r$nombre_cientifico)
    sp <- sprintf("\\textit{%s}", sp)
    if (indent) sp <- paste0("\\quad ", sp)
    sprintf("%s & %.0f & %.0f & %s & %s & %s & %s \\\\",
            sp, r$n_ha, r$n_umm,
            fnum(r$dn_m, 1), fnum(r$h_m, 1),
            fnum(r$vol_ha, 2), fnum(r$vol_umm, 1))
  }

  fila_sub <- function(s, label) {
    sprintf(
      "\\midrule\\quad\\textbf{%s} & \\textbf{%.0f} & \\textbf{%.0f} & %s & %s & \\textbf{%s} & \\textbf{%s} \\\\",
      esc(label), s$n_ha, s$n_umm,
      fnum(s$dn_m, 1), fnum(s$h_m, 1),
      fnum(s$vol_ha, 2), fnum(s$vol_umm, 1))
  }

  # Pinus
  sp_p <- por_especie %>% filter(genero == "Pinus")
  if (nrow(sp_p) > 0) {
    tex <- c(tex, "\\multicolumn{7}{@{}l}{\\textbf{Pinus}} \\\\")
    for (i in seq_len(nrow(sp_p))) tex <- c(tex, fila_esp(sp_p[i,]))
    if (!is.null(sub_p)) tex <- c(tex, fila_sub(sub_p, "Subtotal Pinus"), "\\midrule")
  }

  # Quercus
  sp_q <- por_especie %>% filter(genero == "Quercus")
  if (nrow(sp_q) > 0) {
    tex <- c(tex, "\\multicolumn{7}{@{}l}{\\textbf{Quercus}} \\\\")
    for (i in seq_len(nrow(sp_q))) tex <- c(tex, fila_esp(sp_q[i,]))
    if (!is.null(sub_q)) tex <- c(tex, fila_sub(sub_q, "Subtotal Quercus"), "\\midrule")
  }

  # Otras
  sp_o <- por_especie %>% filter(!genero %in% c("Pinus","Quercus"))
  if (nrow(sp_o) > 0) {
    tex <- c(tex, "\\multicolumn{7}{@{}l}{\\textbf{Otras especies}} \\\\")
    for (i in seq_len(nrow(sp_o))) tex <- c(tex, fila_esp(sp_o[i,]))
    if (!is.null(sub_o))
      tex <- c(tex, fila_sub(sub_o, "Subtotal Otras especies"), "\\midrule")
  }

  # Total
  if (!is.null(sub_t)) {
    tex <- c(tex,
      sprintf("\\midrule\\textbf{TOTAL} & \\textbf{%.0f} & \\textbf{%.0f} & %s & %s & \\textbf{%s} & \\textbf{%s} \\\\",
              sub_t$n_ha, sub_t$n_umm,
              fnum(sub_t$dn_m, 1), fnum(sub_t$h_m, 1),
              fnum(sub_t$vol_ha, 2), fnum(sub_t$vol_umm, 1))
    )
  }

  tex <- c(tex,
    "\\end{longtable}",
    "\\end{footnotesize}\\end{center}",
    "\\vspace{2pt}"
  )

  # Muertos: resumen corto
  if (nrow(muertos_u) > 0) {
    n_m_ha  <- nrow(muertos_u) * fe_ha
    n_m_umm <- n_m_ha * sup_corta
    tex <- c(tex,
      "\\begin{small}",
      sprintf("\\noindent\\textbf{Arbolado muerto:} %d en parcelas — %.0f ind/ha — ~%.0f ind. estimados en UMM\\\\[2pt]",
              nrow(muertos_u), n_m_ha, n_m_umm),
      "\\end{small}",
      "\\vspace{2pt}"
    )
  }

  # ---- Gráficos (pie vol + pie n + distribución) ----------------------------
  tex <- c(tex,
    "\\begin{center}",
    sprintf("\\includegraphics[width=7cm,keepaspectratio]{%s}\\hspace{0.5cm}",
            graficos$pie_vol),
    sprintf("\\includegraphics[width=7cm,keepaspectratio]{%s}", graficos$pie_n),
    "\\end{center}",
    "\\vspace{2pt}"
  )

  if (!is.null(graficos$dist)) {
    tex <- c(tex,
      "\\begin{center}",
      sprintf("\\includegraphics[width=14cm,keepaspectratio]{%s}", graficos$dist),
      "\\end{center}",
      "\\vspace{4pt}"
    )
  }

  # ---- Daños y sanidad -------------------------------------------------------
  tex <- c(tex, "\\subsection*{\\normalsize Da\\~nos y sanidad}")

  if (n_v == 0) {
    tex <- c(tex,
      "\\begin{small}\\noindent\\textit{Sin arbolado vivo registrado.}\\end{small}",
      "\\vspace{4pt}")
  } else {
    dano_str <- if (nrow(top_dano) > 0)
      paste(sprintf("%s (%d árb.)", esc(top_dano$dano1_lbl), top_dano$n), collapse = "; ")
    else
      "Sin tipos registrados"
    san_str <- if (nrow(top_san) > 0)
      paste(sprintf("%s (%d árb.)", esc(top_san$sanidad_lbl), top_san$n), collapse = "; ")
    else
      "Sin problemas registrados"

    tex <- c(tex,
      "\\begin{small}",
      sprintf("\\noindent\\textbf{Da\\~nos f\\'isicos:} %d de %d \\'arb. vivos (%.0f\\%%) --- %s \\\\[2pt]",
              nrow(arb_dano), n_v, pct_d, dano_str),
      sprintf("\\textbf{Sanidad:} %d de %d \\'arb. vivos (%.0f\\%%) --- %s",
              nrow(arb_san), n_v, pct_s, san_str),
      "\\end{small}",
      "\\vspace{4pt}"
    )
  }

  # ---- Regeneración natural --------------------------------------------------
  tex <- c(tex, "\\subsection*{\\normalsize Regeneraci\\'on natural}")

  tex <- c(tex,
    "\\begin{small}",
    sprintf("\\noindent Densidad media: \\textbf{%.0f pl/ha} (promedio sobre los %d sitios del UMM)",
            regen_dens_ha, n_sitios),
    "\\\\[4pt]"
  )

  if (nrow(regen_por_genero) > 0) {
    tex <- c(tex,
      "\\begin{tabular}{@{}lr@{}}",
      "\\toprule",
      "G\\'enero & Densidad (pl/ha) \\\\",
      "\\midrule"
    )
    for (i in seq_len(nrow(regen_por_genero))) {
      r <- regen_por_genero[i, ]
      tex <- c(tex,
        sprintf("\\textit{%s} & %.0f \\\\", esc(r$genero), r$dens_ha))
    }
    tex <- c(tex,
      "\\midrule",
      sprintf("\\textbf{Total} & \\textbf{%.0f} \\\\", regen_dens_ha),
      "\\bottomrule",
      "\\end{tabular}"
    )
  } else {
    tex <- c(tex,
      "\\textit{Sin regeneraci\\'on registrada en ning\\'un sitio de este UMM.}")
  }

  tex <- c(tex, "\\end{small}", "\\vspace{4pt}")

  # ---- Riesgo de incendio ----------------------------------------------------
  tex <- c(tex, "\\subsection*{\\normalsize Riesgo de incendio}")

  tex <- c(tex,
    "\\begin{small}",
    sprintf("\\noindent \\'{I}ndice medio: \\textbf{%.1f/100} — Categor\\'ia predominante: \\colorbox{%s}{\\textbf{\\color{white}~%s~}}\\\\[4pt]",
            idx_med, riesgo_color(cat_modal[1]), cat_modal[1])
  )

  # Tabla de categorías
  for (i in seq_len(nrow(cats_n))) {
    r <- cats_n[i,]
    pct_s <- r$n / n_sitios * 100
    col   <- riesgo_color(r$categoria_riesgo)
    tex   <- c(tex,
      sprintf("\\colorbox{%s}{\\color{white}\\textbf{~%s~}}~%d sitios (%.0f\\%%)\\hspace{1em}",
              col, r$categoria_riesgo, r$n, pct_s))
  }

  tex <- c(tex,
    "\\\\[4pt]",
    "\\textbf{Carga media de combustible (t/ha):} \\\\[2pt]",
    "\\begin{tabular}{@{}lrrrrr@{}}",
    "\\toprule",
    "& 1H & 10H & 100H & 1000H & Total \\\\",
    "\\midrule",
    sprintf("Carga (t/ha) & %.3f & %.3f & %.3f & %.2f & %.2f \\\\",
            carga_med$c1, carga_med$c10, carga_med$c100, carga_med$c1k, carga_med$ctot),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{small}",
    "\\vspace{4pt}"
  )

  # ---- Serie fotográfica -----------------------------------------------------
  foto_caption <- function(fname) {
    # Sitio_01_Norte_a_Sur.jpg → "Sitio 1 — Norte a Sur"; Sitio_34.jpg → "Sitio 34"
    base  <- tools::file_path_sans_ext(basename(fname))
    parts <- strsplit(base, "_")[[1]]
    num   <- as.integer(parts[2])
    rest  <- parts[-c(1, 2)]
    if (length(rest) == 0) return(sprintf("Sitio %d", num))
    sprintf("Sitio %d --- %s", num, paste(rest, collapse = " "))
  }

  if (length(fotos_sel) > 0) {
    tex <- c(tex, "\\subsection*{\\normalsize Serie fotogr\\'afica representativa}")

    for (f in fotos_sel) {
      cap <- foto_caption(f)
      tex <- c(tex,
        "\\begin{center}",
        sprintf("\\includegraphics[width=\\linewidth,keepaspectratio]{%s}", f),
        sprintf("\\par\\vspace{1pt}\\small\\textit{%s}", esc(cap)),
        "\\end{center}",
        "\\vspace{3pt}"
      )
    }
  }

  tex
}

# ==============================================================================
# DOCUMENTO COMPLETO
# ==============================================================================

cat("Generando fichas por UMM...\n")

preamble <- c(
  "\\documentclass[a4paper,10pt]{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[spanish,es-noshorthands]{babel}",
  "\\usepackage[top=2cm,bottom=2.2cm,left=2cm,right=2cm]{geometry}",
  "\\usepackage{graphicx}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{array}",
  "\\usepackage{fancyhdr}",
  "\\usepackage{xcolor}",
  "\\usepackage{colortbl}",
  "\\usepackage{amsmath}",
  "\\usepackage{microtype}",
  "\\usepackage{tocloft}",
  "\\usepackage[hidelinks]{hyperref}",
  "\\definecolor{verde_bosque}{RGB}{34,100,34}",
  "\\definecolor{riesgobajo}{RGB}{39,150,60}",
  "\\definecolor{riesgomoderado}{RGB}{220,130,0}",
  "\\definecolor{riesgoalto}{RGB}{200,30,30}",
  "\\definecolor{riesgoextremo}{RGB}{100,0,0}",
  "\\definecolor{gris}{RGB}{120,120,120}",
  "\\pagestyle{fancy}\\fancyhf{}",
  "\\renewcommand{\\headrulewidth}{0.4pt}",
  "\\fancyhead[L]{\\small\\color{verde_bosque}\\textbf{PMF Las Alazanas 2026--2036}}",
  "\\fancyhead[C]{\\small\\color{gray} Inventario Forestal --- Resumen por UMM}",
  "\\fancyhead[R]{\\small\\color{gray}\\today}",
  "\\fancyfoot[C]{\\small\\thepage}",
  "\\setlength{\\parindent}{0pt}",
  "\\setlength{\\LTpre}{2pt}",
  "\\setlength{\\LTpost}{2pt}",
  "\\begin{document}",
  # Portada
  "\\begin{titlepage}\\vspace*{4cm}\\begin{center}",
  "{\\Huge\\bfseries\\color{verde_bosque}Inventario Forestal\\\\[0.5cm]}",
  "{\\LARGE\\bfseries PMF Las Alazanas 2026--2036\\\\[0.8cm]}",
  "{\\large Resumen por Unidad M\\'inima de Manejo (UMM)\\\\[0.5cm]}",
  "\\hrule\\vspace{0.5cm}{\\large\\today}",
  "\\end{center}\\vfill\\end{titlepage}",
  "\\tableofcontents",
  "\\newpage"
)

umm_ids <- sort(unique(arboles$rodal))
body    <- c()
for (umm in umm_ids)
  body <- c(body, generar_ficha_umm(umm))

doc      <- c(preamble, body, "\\end{document}")
tex_path <- file.path(tmp_dir, "fichas_umm.tex")
writeLines(doc, tex_path, useBytes = FALSE)
cat(sprintf("Archivo .tex: %d líneas\n", length(doc)))

# ==============================================================================
# COMPILAR
# ==============================================================================

old_wd <- getwd()
setwd(tmp_dir)

for (pasada in 1:2) {
  cat(sprintf("Compilando (pasada %d/2)...\n", pasada))
  system2("pdflatex",
    args   = c("-interaction=nonstopmode", "fichas_umm.tex"),
    stdout = sprintf("log%d.txt", pasada),
    stderr = sprintf("log%d.txt", pasada))
}

setwd(old_wd)

pdf_src  <- file.path(tmp_dir, "fichas_umm.pdf")
pdf_dest <- file.path(PROYECTO_ROOT, "Fichas", "Resumen_UMM.pdf")
dir.create(dirname(pdf_dest), showWarnings = FALSE)

if (file.exists(pdf_src)) {
  file.copy(pdf_src, pdf_dest, overwrite = TRUE)
  cat(sprintf("\n✓ PDF: %s\n", pdf_dest))
} else {
  cat(sprintf("\n✗ PDF no generado. Ver: %s\n", file.path(tmp_dir, "log1.txt")))
}
