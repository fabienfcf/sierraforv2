# ==============================================================================
# FICHAS DE INVENTARIO POR SITIO DE MUESTREO — PMF Las Alazanas 2026-2036
# Genera un catálogo PDF con una ficha por sitio (árboles, riesgo, regeneración)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

if (!exists("PROYECTO_ROOT"))
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"

if (!exists("es_arbol_vivo"))
  source(file.path(PROYECTO_ROOT, "core", "15_core_calculos.R"))

# ==============================================================================
# TABLAS DE CÓDIGOS
# ==============================================================================

EXPOSICION   <- c("1"="Cénit","2"="Norte","3"="Noreste","4"="Este",
                  "5"="Sureste","6"="Sur","7"="Suroeste","8"="Oeste","9"="Noroeste")
DOMINANCIA   <- c("1"="Dominante","2"="Intermedio","3"="Suprimido",
                  "4"="Libre s/sup.","5"="Libre c/sup.","6"="Aislado",
                  "7"="Muerto en pie","8"="Muerto caído","9"="Tocón")
DANOS        <- c("0"="Sin daño","1"="Sin daño","2"="Viejo/resinado",
                  "3"="Fuste nudoso","4"="Ladeado/torcido","5"="Descortezado",
                  "6"="Puntiseco","7"="Anillado","8"="Fuste ovoide",
                  "9"="Daño por cable","10"="Bifurcado")
UBIC_DANO    <- c("0"="Sin daño","1"="Sin daño","2"="Punta","3"="Parte media",
                  "4"="Base","5"="Punta+media","6"="Punta+base","7"="Media+base","8"="Completo")
SANIDAD      <- c("1"="Sano","2"="Muérdago","3"="Barrenadores yemas",
                  "4"="Descortezadores","5"="Defoliadores","6"="Paxtle")
INTENSIDAD   <- c("1"="Sin afectación","2"="Baja (<33%)","3"="Moderada (33-66%)","4"="Alta (>66%)")
PERTURBACION <- c("1"="Sin perturbación","2"="Hongos/enfermedades","3"="Plagas",
                  "4"="Aprovechamiento ilegal","5"="Anillado","6"="Resinado",
                  "7"="Incendios","8"="Pastoreo","9"="Extracción de leña",
                  "10"="Plantas parásitas","11"="Lianas","12"="Roedores",
                  "13"="Rayos","14"="Viento","15"="Otras")
TRATAMIENTO  <- c("1"="No corta","2"="Corta selección","3"="Corta regeneración",
                  "4"="Corta liberación/pre-clareo","5"="Primer clareo",
                  "6"="Segundo clareo","7"="Tercer clareo","8"="Cuarto clareo",
                  "9"="Corta rasa con plantación","10"="Corta protección")
TRAT_COMP    <- c("1"="Quema controlada","2"="Desbroce","3"="Limpieza suelo",
                  "4"="Reducción densidad regen.","5"="Reforestación",
                  "6"="Plantación directa","7"="Limpieza regeneración",
                  "8"="Restauración suelos","9"="Cortas sanitarias",
                  "10"="Control azolves","11"="Brecha cortafuego",
                  "12"="Cercado","13"="Podas")

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

f01_raw <- read_excel(file.path(PROYECTO_ROOT, "inventario_forestal.xlsx"), sheet = "F01")
names(f01_raw) <- make.names(names(f01_raw))

riesgo <- read.csv(file.path(PROYECTO_ROOT, "analisis_riesgo_incendio_completo.csv"),
                   stringsAsFactors = FALSE)

regen_obs <- read.csv(file.path(PROYECTO_ROOT, "resultados/desc_09_regeneracion_observada.csv"),
                      stringsAsFactors = FALSE)

FACTOR_EXP <- 10000 / 9  # cuadrante 9 m² → plantas/ha

# ==============================================================================
# PREPARACIÓN DE DATOS
# ==============================================================================

# Volumen para árboles muertos en pie con altura medida (no tocones)
calc_vol <- function(d, h, tipo, a, b, c) {
  ifelse(
    is.na(d) | is.na(h) | d <= 0 | h <= 0,
    0,
    ifelse(tipo == "potencia",
           a * d^b * h^c,
           exp(a + b * log(d) + c * log(h)))
  )
}

arboles <- arboles %>%
  mutate(
    copa_m2 = ifelse(
      !is.na(diametro_copa_ns) & !is.na(diametro_copa_eo) &
        diametro_copa_ns > 0 & diametro_copa_eo > 0,
      pi * (diametro_copa_ns / 2) * (diametro_copa_eo / 2), NA_real_),
    estado = ifelse(!es_arbol_vivo(dominancia), "muerto", "vivo"),
    # Recalcular volumen para muertos en pie/caído con altura medida
    volumen_m3 = ifelse(
      estado == "muerto" & dominancia != 9 & altura_total > 0 & volumen_m3 == 0,
      calc_vol(diametro_normal, altura_total, tipo, a, b, c),
      volumen_m3),
    dano1_lbl  = cod_lbl(dano_fisico1, DANOS),
    ubic1_lbl  = cod_lbl(ubicacion_dano1, UBIC_DANO),
    sanidad_lbl = cod_lbl(sanidad, SANIDAD),
    intens_lbl  = cod_lbl(calificacion_sanidad, INTENSIDAD)
  )

# F01: datos a nivel de sitio
f01 <- f01_raw %>%
  select(sitio_id = X5.SITIO_I,
         per1 = X30.PER1, per2 = X30.PER2, per3 = X30.PER3,
         ts   = X35.TS,
         tc1  = X36.TC1, tc2  = X36.TC2, tc3  = X36.TC3,
         obs  = X37.OBS) %>%
  filter(!is.na(sitio_id)) %>%
  mutate(across(c(per1, per2, per3), ~ cod_lbl(., PERTURBACION)),
         ts_lbl  = cod_lbl(ts,  TRATAMIENTO),
         tc1_lbl = cod_lbl(tc1, TRAT_COMP),
         tc2_lbl = cod_lbl(tc2, TRAT_COMP),
         tc3_lbl = cod_lbl(tc3, TRAT_COMP))

# Regeneración: sumar por sitio y género
regen_sitio <- regen_obs %>%
  group_by(muestreo) %>%
  summarize(
    n_individuos  = sum(num_individuos, na.rm = TRUE),
    dens_ha       = sum(num_individuos, na.rm = TRUE) * FACTOR_EXP,
    generos       = paste(sort(unique(genero)), collapse = ", "),
    .groups = "drop"
  )

regen_detalle <- regen_obs %>%
  group_by(muestreo, genero) %>%
  summarize(
    n   = sum(num_individuos, na.rm = TRUE),
    dens = sum(num_individuos, na.rm = TRUE) * FACTOR_EXP,
    .groups = "drop"
  )

# Orden de procesamiento: por UMM (rodal) luego por sitio
sitio_orden <- arboles %>%
  distinct(muestreo, rodal) %>%
  arrange(rodal, muestreo)

# ==============================================================================
# DIRECTORIO TEMPORAL SIN ESPACIOS
# ==============================================================================

tmp_dir <- "/tmp/fichas_inventario"
dir.create(file.path(tmp_dir, "mapas"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(tmp_dir, "fotos"), showWarnings = FALSE, recursive = TRUE)

cat("Copiando imágenes...\n")
for (f in list.files(file.path(PROYECTO_ROOT, "Mapas_sitios"), full.names = TRUE))
  file.copy(f, file.path(tmp_dir, "mapas", basename(f)), overwrite = TRUE)
for (f in list.files(file.path(PROYECTO_ROOT, "Fotos_sitios"), full.names = TRUE))
  file.copy(f, file.path(tmp_dir, "fotos", basename(f)), overwrite = TRUE)

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

esc <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&",  "\\\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\\\%",  x, fixed = TRUE)
  x <- gsub("$",  "\\\\$",  x, fixed = TRUE)
  x <- gsub("#",  "\\\\#",  x, fixed = TRUE)
  x <- gsub("_",  "\\\\_",  x, fixed = TRUE)
  x <- gsub("{",  "\\\\{",  x, fixed = TRUE)
  x <- gsub("}",  "\\\\}",  x, fixed = TRUE)
  x
}

fnum <- function(x, d = 1)
  ifelse(is.na(x) | is.nan(x), "---", formatC(round(x, d), format = "f", digits = d))

fvol <- function(x)
  ifelse(is.na(x) | is.nan(x) | x == 0, "---",
         formatC(round(x, 3), format = "f", digits = 3))

cat_notna <- function(..., sep = ", ") {
  vals <- c(...); vals <- vals[!is.na(vals) & vals != "NA" & nchar(trimws(vals)) > 0]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = sep)
}

foto_orient <- function(fname) {
  # "Sitio_35_Norte_a_Sur.jpg" → "Norte → Sur"; "Sitio_34.jpg" → ""
  s <- tools::file_path_sans_ext(basename(fname))
  resto <- sub("^Sitio_[0-9]+_", "", s)
  if (resto == s) return("")   # sin sufijo direccional
  resto <- gsub("_a_", " → ", resto, fixed = TRUE)
  resto <- gsub("_", " ", resto, fixed = TRUE)
  resto
}

riesgo_color <- function(cat) {
  switch(cat,
    "BAJO"     = "riesgobajo",
    "MODERADO" = "riesgomoderado",
    "ALTO"     = "riesgoalto",
    "EXTREMO"  = "riesgoextremo",
    "gris")
}

# ==============================================================================
# BLOQUE LATEX PARA CADA FICHA
# ==============================================================================

generar_ficha <- function(sitio_df, muestreo_num) {
  umm_num  <- unique(sitio_df$rodal)[1]
  utm_x    <- unique(sitio_df$utm_x)[1]
  utm_y    <- unique(sitio_df$utm_y)[1]
  asnm     <- as.integer(unique(sitio_df$asnm)[1])
  pend     <- as.integer(unique(sitio_df$pendiente)[1])
  exp_lbl  <- cod_lbl(unique(sitio_df$exposicion)[1], EXPOSICION)
  if (is.na(exp_lbl)) exp_lbl <- as.character(unique(sitio_df$exposicion)[1])
  fecha    <- unique(sitio_df$fecha)[1]

  genus_rank <- function(g) case_when(g == "Pinus" ~ 1L, g == "Quercus" ~ 2L, TRUE ~ 3L)

  vivos   <- sitio_df %>% filter(estado == "vivo") %>%
    arrange(genus_rank(genero), nombre_cientifico, arbol)
  muertos <- sitio_df %>% filter(estado == "muerto") %>%
    arrange(genus_rank(genero), nombre_cientifico, arbol)

  # Imágenes
  mapa_rel <- sprintf("mapas/UMM%d_sitio%02d.jpg", umm_num, muestreo_num)
  mapa_ok  <- file.exists(file.path(tmp_dir, mapa_rel))

  foto_files <- list.files(file.path(tmp_dir, "fotos"),
                           pattern = sprintf("^Sitio_%02d[_.]", muestreo_num))
  fotos_rel  <- file.path("fotos", foto_files)

  # F01 para este sitio
  f01s <- f01 %>% filter(sitio_id == muestreo_num)

  # Riesgo incendio
  ries <- riesgo %>% filter(muestreo == muestreo_num)

  # Regeneración
  regen_s  <- regen_sitio  %>% filter(muestreo == muestreo_num)
  regen_d  <- regen_detalle %>% filter(muestreo == muestreo_num)

  # -----------------------------------------------------------------------
  toc_entry <- sprintf("UMM~%d \\textemdash{} Sitio de Muestreo~%02d", umm_num, muestreo_num)
  tex <- c(
    "\\newpage",
    "\\phantomsection",
    sprintf("\\addcontentsline{toc}{section}{%s}", toc_entry)
  )

  # ---- Título -----------------------------------------------------------
  tex <- c(tex,
    "\\begin{center}",
    sprintf("{\\LARGE\\bfseries\\color{verde_bosque} %s}", toc_entry),
    "\\end{center}",
    "\\vspace{1pt}\\hrule\\vspace{4pt}"
  )

  # ---- Metadatos (tabla 3 columnas) + Mapa centrado ----------------------
  tex <- c(tex,
    "\\begin{small}",
    "\\begin{tabular}{@{}ll@{\\qquad}ll@{\\qquad}ll@{}}",
    sprintf("\\textbf{UTM~X:} & %.4f & \\textbf{Altitud:} & %d~m.s.n.m. & \\textbf{Exposici\\'{o}n:} & %s \\\\",
            utm_x, asnm, esc(exp_lbl)),
    sprintf("\\textbf{UTM~Y:} & %.5f & \\textbf{Pendiente:} & %d\\%% & \\textbf{Fecha:} & %s \\\\",
            utm_y, pend, fecha),
    sprintf("\\textbf{\\'{A}rb.~vivos:} & %d~(%.0f/ha) & \\textbf{\\'{A}rb.~muertos:} & %d~(%.0f/ha) & \\textbf{Superficie:} & 0.1~ha \\\\",
            nrow(vivos), nrow(vivos)*10, nrow(muertos), nrow(muertos)*10),
    "\\end{tabular}",
    "\\end{small}",
    "\\vspace{3pt}"
  )

  if (mapa_ok) {
    tex <- c(tex,
      "\\begin{center}",
      sprintf("\\includegraphics[width=9cm,keepaspectratio]{%s}", mapa_rel),
      "\\end{center}",
      "\\vspace{3pt}"
    )
  }

  # ---- Fotos apiladas con leyenda (ancho de página) --------------------
  if (length(fotos_rel) > 0) {
    tex <- c(tex, "\\begin{center}")
    for (foto in fotos_rel) {
      orient <- foto_orient(foto)
      if (nchar(orient) > 0) {
        tex <- c(tex,
          sprintf("\\includegraphics[width=\\textwidth,keepaspectratio]{%s}\\\\[2pt]", foto),
          sprintf("{\\scriptsize\\textit{%s}}\\\\[8pt]", esc(orient))
        )
      } else {
        tex <- c(tex,
          sprintf("\\includegraphics[width=\\textwidth,keepaspectratio]{%s}\\\\[8pt]", foto)
        )
      }
    }
    tex <- c(tex, "\\end{center}", "\\vspace{4pt}")
  }

  # ---- Perturbaciones y tratamientos ------------------------------------
  if (nrow(f01s) > 0) {
    r <- f01s[1, ]

    pers <- cat_notna(r$per1, r$per2, r$per3)
    pers_str <- ifelse(is.na(pers), "Sin registro", esc(pers))

    trats <- cat_notna(r$ts_lbl, r$tc1_lbl, r$tc2_lbl, r$tc3_lbl)
    trats_str <- ifelse(is.na(trats), "Sin registro", esc(trats))

    obs_str <- ifelse(is.na(r$obs) | trimws(r$obs) == "",
                      "", sprintf("\\textbf{Obs.:} %s", esc(trimws(r$obs))))

    tex <- c(tex,
      "\\begin{small}",
      "\\noindent\\textbf{Perturbaciones:} ", pers_str, "\\quad",
      "\\textbf{Tratamientos previos:} ", trats_str, "\\\\[1pt]",
      ifelse(obs_str != "", paste0(obs_str, "\\\\[1pt]"), ""),
      "\\end{small}",
      "\\vspace{2pt}"
    )
  }

  # ---- Sanidad a nivel sitio (árboles vivos con daño) ------------------
  arboles_con_dano <- vivos %>%
    filter(!is.na(dano_fisico1) & dano_fisico1 != 0 & dano_fisico1 != 1)
  arboles_con_sanidad <- vivos %>%
    filter(!is.na(sanidad) & sanidad != 1)

  if (nrow(arboles_con_dano) > 0 || nrow(arboles_con_sanidad) > 0) {
    n_dano    <- nrow(arboles_con_dano)
    pct_dano  <- round(n_dano / nrow(vivos) * 100, 0)
    n_san     <- nrow(arboles_con_sanidad)
    pct_san   <- round(n_san / nrow(vivos) * 100, 0)

    dano_resumen <- arboles_con_dano %>%
      count(dano1_lbl, ubic1_lbl) %>%
      arrange(desc(n)) %>%
      mutate(str = sprintf("%s en %s (%d árb.)",
                           esc(dano1_lbl), esc(ubic1_lbl), n))

    san_resumen <- arboles_con_sanidad %>%
      count(sanidad_lbl, intens_lbl) %>%
      arrange(desc(n)) %>%
      mutate(str = sprintf("%s — %s (%d árb.)",
                           esc(sanidad_lbl), esc(intens_lbl), n))

    tex <- c(tex,
      "\\begin{small}",
      sprintf("\\noindent\\textbf{Daños físicos:} %d árb. (%d\\%%) --- %s\\\\[1pt]",
              n_dano, pct_dano,
              paste(dano_resumen$str, collapse = "; ")),
      ifelse(nrow(san_resumen) > 0,
             sprintf("\\textbf{Sanidad:} %d árb. (%d\\%%) --- %s\\\\[1pt]",
                     n_san, pct_san,
                     paste(san_resumen$str, collapse = "; ")),
             ""),
      "\\end{small}",
      "\\vspace{2pt}"
    )
  }

  # ---- Riesgo de incendio -----------------------------------------------
  if (nrow(ries) > 0) {
    r <- ries[1, ]
    col <- riesgo_color(r$categoria_riesgo)
    tex <- c(tex,
      "\\noindent",
      sprintf("\\colorbox{%s}{\\textbf{\\color{white} RIESGO DE INCENDIO: %s}}~~",
              col, r$categoria_riesgo),
      "\\begin{small}",
      sprintf("Índice de riesgo:~%.1f/100", r$indice_riesgo),
      "\\end{small}\\\\[3pt]",
      "\\begin{small}",
      "\\begin{tabular}{@{}lrrrrr@{}}",
      "\\toprule",
      "& 1H & 10H & 100H & 1000H & Total \\\\",
      "\\midrule",
      sprintf("Carga combustible (t/ha) & %.3f & %.3f & %.3f & %.2f & %.2f \\\\",
              r$carga_1h, r$carga_10h, r$carga_100h, r$carga_1000h, r$carga_total),
      "\\bottomrule",
      "\\end{tabular}",
      "\\end{small}",
      "\\vspace{4pt}"
    )
  }

  # ---- Tabla árboles (vivos y muertos) ----------------------------------
  make_tree_table <- function(df, titulo) {
    if (nrow(df) == 0) return(character(0))
    has_copa <- any(!is.na(df$copa_m2))

    if (has_copa) {
      cfmt <- "@{}>{\\footnotesize}r @{\\;} >{\\footnotesize\\itshape}l >{\\footnotesize}r >{\\footnotesize}r >{\\footnotesize}r >{\\footnotesize}r >{\\footnotesize}r@{}"
      h1   <- "N° & Especie & Alt.Tot. & Alt.Com. & $D_{1.3}$ & Copa & Vol. \\\\"
      h2   <- " & & (m) & (m) & (cm) & (m\\textsuperscript{2}) & (m\\textsuperscript{3}) \\\\"
      ncol <- 7
    } else {
      cfmt <- "@{}>{\\footnotesize}r @{\\;} >{\\footnotesize\\itshape}l >{\\footnotesize}r >{\\footnotesize}r >{\\footnotesize}r >{\\footnotesize}r@{}"
      h1   <- "N° & Especie & Alt.Tot. & Alt.Com. & $D_{1.3}$ & Vol. \\\\"
      h2   <- " & & (m) & (m) & (cm) & (m\\textsuperscript{3}) \\\\"
      ncol <- 6
    }

    rows <- character(nrow(df))
    for (i in seq_len(nrow(df))) {
      r  <- df[i, ]
      sp <- esc(r$nombre_cientifico)

      # Para muertos: altura y volumen solo si > 0
      ht_str <- if (r$estado == "muerto" && (is.na(r$altura_total) || r$altura_total == 0))
        "---" else fnum(r$altura_total, 1)
      hc_str <- if (r$estado == "muerto" && (is.na(r$altura_copa) || r$altura_copa == 0))
        "---" else fnum(r$altura_copa, 1)
      # Volumen: mostrar si calculado (no 0 ni NA)
      vol_str <- fvol(r$volumen_m3)

      copa_col <- if (has_copa) paste0(" & ", fnum(r$copa_m2, 1)) else ""
      dom_str  <- if (r$estado == "muerto")
        sprintf(" {\\tiny(%s)}", esc(DOMINANCIA[as.character(r$dominancia)])) else ""

      rows[i] <- sprintf("%d & %s%s & %s & %s & %s%s & %s \\\\",
        i, sp, dom_str, ht_str, hc_str,
        fnum(r$diametro_normal, 1), copa_col, vol_str)
    }

    cont_cols <- sprintf("\\multicolumn{%d}{r}{\\footnotesize (continúa...)}", ncol)
    c(
      sprintf("\\subsection*{\\normalsize %s (%d árb.)}", titulo, nrow(df)),
      "\\vspace{-4pt}",
      sprintf("\\begin{longtable}{%s}", cfmt),
      "\\toprule", h1, h2, "\\midrule",
      "\\endfirsthead",
      "\\toprule", h1, h2, "\\midrule",
      "\\endhead",
      sprintf("\\midrule %s \\\\", cont_cols),
      "\\endfoot",
      "\\bottomrule",
      "\\endlastfoot",
      rows,
      "\\end{longtable}",
      "\\vspace{2pt}"
    )
  }

  make_dead_table <- function(df) {
    if (nrow(df) == 0) return(c(
      "\\subsection*{\\normalsize Árboles Muertos}",
      "\\vspace{-4pt}",
      "\\begin{small}\\noindent\\textit{Sin árboles muertos registrados en este sitio.}\\end{small}",
      "\\vspace{4pt}"
    ))
    cfmt <- "@{}>{\\footnotesize}r @{\\;} >{\\footnotesize\\itshape}l >{\\footnotesize}r@{}"
    rows <- character(nrow(df))
    for (i in seq_len(nrow(df))) {
      r <- df[i, ]
      dom_str <- sprintf(" {\\tiny(%s)}", esc(DOMINANCIA[as.character(r$dominancia)]))
      rows[i] <- sprintf("%d & %s%s & %s \\\\",
        i, esc(r$nombre_cientifico), dom_str, fnum(r$diametro_normal, 1))
    }

    # Resumen por especie con N/ha
    by_sp_m <- df %>%
      group_by(nombre_cientifico) %>%
      summarize(n = n(), n_ha = n() * 10L,
                dn_m = mean(diametro_normal, na.rm = TRUE), .groups = "drop") %>%
      arrange(nombre_cientifico)
    tot_m <- df %>% summarize(n = n(), n_ha = n() * 10L,
                               dn_m = mean(diametro_normal, na.rm = TRUE))
    sum_rows <- by_sp_m %>% pmap_chr(function(nombre_cientifico, n, n_ha, dn_m)
      sprintf("\\textit{%s} & %d & %d & %s \\\\",
              esc(nombre_cientifico), n, n_ha, fnum(dn_m, 1)))

    c(
      sprintf("\\subsection*{\\normalsize Árboles Muertos (%d árb.)}", nrow(df)),
      "\\vspace{-4pt}",
      sprintf("\\begin{longtable}{%s}", cfmt),
      "\\toprule",
      "N° & Especie & $D_{1.3}$ (cm) \\\\",
      "\\midrule",
      "\\endfirsthead",
      "\\toprule",
      "N° & Especie & $D_{1.3}$ (cm) \\\\",
      "\\midrule",
      "\\endhead",
      "\\midrule \\multicolumn{3}{r}{\\footnotesize (continúa...)} \\\\",
      "\\endfoot",
      "\\bottomrule",
      "\\endlastfoot",
      rows,
      "\\end{longtable}",
      "\\vspace{2pt}",
      # Resumen muertos
      "\\begin{center}\\begin{small}",
      "\\begin{tabular}{@{}>{\\itshape}l rr r@{}}",
      "\\toprule",
      "\\normalfont Especie & N & N/ha & $\\bar{D}_{1.3}$ (cm) \\\\",
      "\\midrule",
      sum_rows,
      "\\midrule",
      sprintf("\\normalfont\\textbf{Total} & \\textbf{%d} & \\textbf{%d} & %s \\\\",
              tot_m$n, tot_m$n_ha, fnum(tot_m$dn_m, 1)),
      "\\bottomrule",
      "\\end{tabular}",
      "\\end{small}\\end{center}",
      "\\vspace{2pt}"
    )
  }

  tex <- c(tex, make_tree_table(vivos,   "Árboles Vivos"))
  tex <- c(tex, make_dead_table(muertos))

  # ---- Resumen árboles vivos --------------------------------------------
  if (nrow(vivos) > 0) {
    by_sp <- vivos %>%
      group_by(nombre_cientifico) %>%
      summarize(
        n      = n(),
        n_ha   = n() * 10L,
        dn_m   = mean(diametro_normal, na.rm = TRUE),
        dn_sd  = sd(diametro_normal,   na.rm = TRUE),
        ht_m   = mean(altura_total,    na.rm = TRUE),
        ht_sd  = sd(altura_total,      na.rm = TRUE),
        vol_ha = sum(volumen_m3,       na.rm = TRUE) * 10,
        .groups = "drop") %>%
      arrange(genus_rank(sub(" .*", "", nombre_cientifico)), nombre_cientifico)

    tot <- vivos %>%
      summarize(n = n(), n_ha = n() * 10L,
                dn_m = mean(diametro_normal, na.rm = TRUE),
                dn_sd = sd(diametro_normal,  na.rm = TRUE),
                ht_m  = mean(altura_total,   na.rm = TRUE),
                ht_sd = sd(altura_total,     na.rm = TRUE),
                vol_ha = sum(volumen_m3,     na.rm = TRUE) * 10)

    sp_rows <- by_sp %>% pmap_chr(function(nombre_cientifico, n, n_ha,
                                            dn_m, dn_sd, ht_m, ht_sd, vol_ha) {
      sprintf("\\textit{%s} & %d & %d & %s $\\pm$ %s & %s $\\pm$ %s & %.2f \\\\",
              esc(nombre_cientifico), n, n_ha,
              fnum(dn_m,1), fnum(dn_sd,1),
              fnum(ht_m,1), fnum(ht_sd,1), vol_ha)
    })

    tex <- c(tex,
      "\\subsection*{\\normalsize Resumen — Árboles Vivos}",
      "\\vspace{-4pt}",
      "\\begin{center}\\begin{small}",
      "\\begin{tabular}{@{}>{\\itshape}l rr cc r@{}}",
      "\\toprule",
      "\\normalfont Especie & N & N/ha & $\\bar{D}_{1.3}\\pm\\sigma$ (cm) & Alt.Tot. $\\pm\\sigma$ (m) & Vol/ha (m\\textsuperscript{3}) \\\\",
      "\\midrule",
      sp_rows,
      "\\midrule",
      sprintf("\\normalfont\\textbf{Total} & \\textbf{%d} & \\textbf{%d} & %s $\\pm$ %s & %s $\\pm$ %s & \\textbf{%.2f} \\\\",
              tot$n, tot$n_ha,
              fnum(tot$dn_m,1), fnum(tot$dn_sd,1),
              fnum(tot$ht_m,1), fnum(tot$ht_sd,1), tot$vol_ha),
      "\\bottomrule",
      "\\end{tabular}",
      "\\end{small}\\end{center}",
      "\\vspace{4pt}"
    )
  }

  # ---- Regeneración natural ---------------------------------------------
  if (nrow(regen_d) > 0) {
    regen_rows <- regen_d %>% pmap_chr(function(muestreo, genero, n, dens) {
      sprintf("\\textit{%s} & %d & %.0f \\\\", esc(genero), n, dens)
    })
    tot_regen <- regen_s[1, ]

    tex <- c(tex,
      "\\subsection*{\\normalsize Regeneración Natural}",
      "\\vspace{-4pt}",
      "\\begin{center}\\begin{small}",
      "\\begin{tabular}{@{}>{\\itshape}l rr@{}}",
      "\\toprule",
      "\\normalfont Género & N individuos & Densidad (pl/ha) \\\\",
      "\\midrule",
      regen_rows,
      "\\midrule",
      sprintf("\\normalfont\\textbf{Total} & \\textbf{%d} & \\textbf{%.0f} \\\\",
              tot_regen$n_individuos, tot_regen$dens_ha),
      "\\bottomrule",
      "\\end{tabular}",
      "\\end{small}\\end{center}"
    )
  } else {
    tex <- c(tex,
      "\\begin{small}\\noindent\\textit{Sin regeneración registrada en este sitio.}\\end{small}")
  }

  tex
}

# ==============================================================================
# TABLA DE REFERENCIA
# ==============================================================================

generar_tabla_referencia <- function() {
  resumen <- sitio_orden %>%
    left_join(
      arboles %>%
        group_by(muestreo) %>%
        summarize(n_vivos   = sum(estado == "vivo"),
                  n_muertos = sum(estado == "muerto"),
                  dn_m      = mean(diametro_normal[estado=="vivo"], na.rm=TRUE),
                  vol_ha    = sum(volumen_m3[estado=="vivo"], na.rm=TRUE)*10,
                  .groups = "drop"),
      by = "muestreo") %>%
    left_join(riesgo %>% select(muestreo, categoria_riesgo, indice_riesgo),
              by = "muestreo") %>%
    left_join(regen_sitio %>% select(muestreo, dens_ha),
              by = "muestreo") %>%
    left_join(arboles %>% distinct(muestreo, utm_x, utm_y, asnm, fecha),
              by = "muestreo")

  rows <- resumen %>% pmap_chr(function(rodal, muestreo, n_vivos, n_muertos,
                                         dn_m, vol_ha, categoria_riesgo,
                                         indice_riesgo, dens_ha,
                                         utm_x, utm_y, asnm, fecha) {
    cat_str <- if (!is.na(categoria_riesgo)) categoria_riesgo else "---"
    regen_str <- if (!is.na(dens_ha)) sprintf("%.0f", dens_ha) else "---"
    sprintf("%d & %02d & %s & %.4f & %.5f & %d & %d & %d & %s & %s \\\\",
            rodal, muestreo, fecha,
            ifelse(is.na(utm_x), 0, utm_x),
            ifelse(is.na(utm_y), 0, utm_y),
            ifelse(is.na(asnm), 0, as.integer(asnm)),
            ifelse(is.na(n_vivos), 0L, n_vivos),
            ifelse(is.na(n_muertos), 0L, n_muertos),
            cat_str, regen_str)
  })

  c(
    "\\section*{Tabla de Referencia de Sitios de Muestreo}",
    "\\begin{center}\\begin{small}",
    "\\begin{longtable}{@{}rr l rr r rr l r@{}}",
    "\\toprule",
    "UMM & Sitio & Fecha & UTM X & UTM Y & Alt.(m) & Vivos & Muertos & Riesgo & Regen.(pl/ha) \\\\",
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    "UMM & Sitio & Fecha & UTM X & UTM Y & Alt.(m) & Vivos & Muertos & Riesgo & Regen.(pl/ha) \\\\",
    "\\midrule",
    "\\endhead",
    "\\midrule \\multicolumn{10}{r}{\\footnotesize (continúa...)} \\\\",
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot",
    rows,
    "\\end{longtable}",
    "\\end{small}\\end{center}"
  )
}

# ==============================================================================
# DOCUMENTO LATEX COMPLETO
# ==============================================================================

cat("Generando documento LaTeX...\n")

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
  "\\usepackage{etoolbox}",
  "\\usepackage{tocloft}",
  "\\usepackage[hidelinks]{hyperref}",
  # Colores riesgo
  "\\definecolor{verde_bosque}{RGB}{34,100,34}",
  "\\definecolor{riesgobajo}{RGB}{39,150,60}",
  "\\definecolor{riesgomoderado}{RGB}{220,130,0}",
  "\\definecolor{riesgoalto}{RGB}{200,30,30}",
  "\\definecolor{riesgoextremo}{RGB}{100,0,0}",
  "\\definecolor{gris}{RGB}{120,120,120}",
  # Cabecera / pie
  "\\pagestyle{fancy}",
  "\\fancyhf{}",
  "\\renewcommand{\\headrulewidth}{0.4pt}",
  "\\fancyhead[L]{\\small\\color{verde_bosque}\\textbf{PMF Las Alazanas 2026--2036}}",
  "\\fancyhead[C]{\\small\\color{gray} Inventario Forestal --- Fichas de Sitio}",
  "\\fancyhead[R]{\\small\\color{gray}\\today}",
  "\\fancyfoot[C]{\\small\\thepage}",
  "\\setlength{\\parindent}{0pt}",
  "\\setlength{\\LTpre}{3pt}",
  "\\setlength{\\LTpost}{3pt}",
  "\\begin{document}"
)

# Índice (tabla de contenido) — se popula en segunda compilación
toc_block <- c(
  "\\tableofcontents",
  "\\newpage"
)

# Fichas en orden UMM > sitio
body <- c()
for (i in seq_len(nrow(sitio_orden))) {
  s  <- sitio_orden$muestreo[i]
  cat(sprintf("  Ficha UMM %d sitio %02d...\n", sitio_orden$rodal[i], s))
  df <- arboles %>% filter(muestreo == s)
  body <- c(body, generar_ficha(df, s))
}

doc <- c(preamble, toc_block, body, "\\end{document}")

tex_path <- file.path(tmp_dir, "fichas_inventario.tex")
writeLines(doc, tex_path, useBytes = FALSE)
cat(sprintf("Archivo .tex escrito (%d líneas)\n", length(doc)))

# ==============================================================================
# COMPILACIÓN
# ==============================================================================

old_wd <- getwd()
setwd(tmp_dir)

for (pasada in 1:2) {
  cat(sprintf("Compilando PDF (pasada %d/2)...\n", pasada))
  system2("pdflatex",
    args   = c("-interaction=nonstopmode", "-halt-on-error", "fichas_inventario.tex"),
    stdout = sprintf("pdflatex_%d.log", pasada),
    stderr = sprintf("pdflatex_%d.log", pasada))
}

setwd(old_wd)

pdf_src  <- file.path(tmp_dir, "fichas_inventario.pdf")
dir_fichas <- file.path(PROYECTO_ROOT, "Fichas")
dir.create(dir_fichas, showWarnings = FALSE)
pdf_dest <- file.path(dir_fichas, "Fichas_Inventario_Sitios.pdf")

if (file.exists(pdf_src)) {
  file.copy(pdf_src, pdf_dest, overwrite = TRUE)
  cat(sprintf("\n✓ PDF generado: %s\n", pdf_dest))
} else {
  cat("\n✗ ERROR: fallo en compilación LaTeX.\n")
  log_lines <- readLines(file.path(tmp_dir, "pdflatex_2.log"))
  cat(paste(grep("^!", log_lines, value = TRUE), collapse = "\n"), "\n")
}
