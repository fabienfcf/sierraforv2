# ==============================================================================
# TABLA 7 - DENSIDAD E INCREMENTOS (NOM-152)
# Genera LaTeX y CSV con densidad, AB, ICA e IMA por UMM
# ==============================================================================

library(tidyverse)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA 7 - DENSIDAD E INCREMENTOS (NOM-152)            ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# PARÁMETROS
# ==============================================================================

# Tiempos de paso (años)
TIEMPO_PASO_PINUS <- 12.5
TIEMPO_PASO_QUERCUS <- 10  # Corregido a 10 años

cat(sprintf("Tiempo de paso Pinus: %.1f años\n", TIEMPO_PASO_PINUS))
cat(sprintf("Tiempo de paso Quercus: %.1f años\n", TIEMPO_PASO_QUERCUS))

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("\n[1/4] Cargando datos...\n")

# Evolución año 0
evolucion <- read_csv("resultados/evolucion_rodal_10anos.csv", show_col_types = FALSE) %>%
  filter(ano_simulacion == 0)

# Superficie por UMM
umm_stats <- read_csv("UMM_stats.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  select(rodal = id,
         superficie_ha = `SUPERFICIE UMM`,
         superficie_corta_ha = `Superficie en Producción (ha)`)

# ICA por género (ya calculado)
ica_data <- read_csv("resultados/31_ica_por_genero_rodal.csv", show_col_types = FALSE)

# Renombrar si es necesario
if ("genero" %in% names(ica_data) && !"genero_grupo" %in% names(ica_data)) {
  ica_data <- ica_data %>% rename(genero_grupo = genero)
}

cat(sprintf("  ✓ Datos cargados para %d UMM\n", n_distinct(evolucion$rodal)))

# ==============================================================================
# FUNCIÓN ÁREA BASAL
# ==============================================================================

# AB = π/4 × dg² × densidad / 10000
calcular_ab_ha <- function(dg_cm, densidad_ha) {
  ifelse(
    is.na(dg_cm) | is.na(densidad_ha) | densidad_ha == 0,
    0,
    (pi / 4) * (dg_cm^2) * densidad_ha / 10000
  )
}

# ==============================================================================
# PREPARAR DATOS POR UMM Y GÉNERO
# ==============================================================================

cat("\n[2/4] Calculando métricas por UMM...\n")

# Datos por UMM y género
datos_umm <- evolucion %>%
  left_join(umm_stats, by = "rodal") %>%
  transmute(
    rodal,
    superficie_ha,
    # PINUS
    pinus_densidad_ha,
    pinus_dg_cm,
    pinus_ab_ha = calcular_ab_ha(pinus_dg_cm, pinus_densidad_ha),
    pinus_vol_ha = pinus_vol_ha_m3,
    # QUERCUS
    quercus_densidad_ha,
    quercus_dg_cm,
    quercus_ab_ha = calcular_ab_ha(quercus_dg_cm, quercus_densidad_ha),
    quercus_vol_ha = quercus_vol_ha_m3,
    # TOTALES
    densidad_total = pinus_densidad_ha + quercus_densidad_ha,
    ab_total = pinus_ab_ha + quercus_ab_ha
  )

# Agregar ICA desde el CSV (columna ICA_m3_ha)
datos_umm <- datos_umm %>%
  left_join(
    ica_data %>% 
      select(rodal, genero_grupo, ICA_m3_ha) %>%
      pivot_wider(names_from = genero_grupo, values_from = ICA_m3_ha, names_prefix = "ica_"),
    by = "rodal"
  ) %>%
  rename(
    pinus_ica = ica_Pinus,
    quercus_ica = ica_Quercus
  ) %>%
  replace_na(list(pinus_ica = 0, quercus_ica = 0))

# Calcular IMA = ER / Tiempo de paso
datos_umm <- datos_umm %>%
  mutate(
    pinus_ima = pinus_vol_ha / 60,
    quercus_ima = quercus_vol_ha / 60,
    # Subtotales
    ica_subtotal = pinus_ica + quercus_ica,
    ima_subtotal = pinus_ima + quercus_ima
  )

cat(sprintf("  ✓ Métricas calculadas para %d UMM\n", nrow(datos_umm)))

# ==============================================================================
# CALCULAR TOTALES PREDIO (PONDERADOS CORRECTAMENTE)
# ==============================================================================

cat("\n[3/4] Calculando totales predio...\n")

superficie_total <- sum(datos_umm$superficie_ha)

# TOTALES POR GÉNERO (Pinus, Quercus) - método mutate → summarise
totales_genero_predio <- datos_umm %>%
  mutate(
    # PASO 1: mutate - ponderar por superficie (crear totales por UMM)
    pinus_densidad_total = pinus_densidad_ha * superficie_ha,
    pinus_ab_total = pinus_ab_ha * superficie_ha,
    pinus_ica_total = pinus_ica * superficie_ha,
    pinus_ima_total = pinus_ima * superficie_ha,
    
    quercus_densidad_total = quercus_densidad_ha * superficie_ha,
    quercus_ab_total = quercus_ab_ha * superficie_ha,
    quercus_ica_total = quercus_ica * superficie_ha,
    quercus_ima_total = quercus_ima * superficie_ha
  ) %>%
  summarise(
    # PASO 2: summarise - sumar todos los totales
    superficie_ha = superficie_total,
    
    pinus_densidad_sum = sum(pinus_densidad_total, na.rm = TRUE),
    pinus_ab_sum = sum(pinus_ab_total, na.rm = TRUE),
    pinus_ica_sum = sum(pinus_ica_total, na.rm = TRUE),
    pinus_ima_sum = sum(pinus_ima_total, na.rm = TRUE),
    
    quercus_densidad_sum = sum(quercus_densidad_total, na.rm = TRUE),
    quercus_ab_sum = sum(quercus_ab_total, na.rm = TRUE),
    quercus_ica_sum = sum(quercus_ica_total, na.rm = TRUE),
    quercus_ima_sum = sum(quercus_ima_total, na.rm = TRUE)
  ) %>%
  mutate(
    # PASO 3: mutate - dividir por superficie total para obtener /ha
    pinus_densidad_ha = pinus_densidad_sum / superficie_ha,
    pinus_ab_ha = pinus_ab_sum / superficie_ha,
    pinus_ica = pinus_ica_sum / superficie_ha,
    pinus_ima = pinus_ima_sum / superficie_ha,
    
    quercus_densidad_ha = quercus_densidad_sum / superficie_ha,
    quercus_ab_ha = quercus_ab_sum / superficie_ha,
    quercus_ica = quercus_ica_sum / superficie_ha,
    quercus_ima = quercus_ima_sum / superficie_ha
  )

# TOTAL PREDIO (suma Pinus + Quercus)
totales_predio <- totales_genero_predio %>%
  mutate(
    # Sumar totales de cada género
    densidad_total_sum = pinus_densidad_sum + quercus_densidad_sum,
    ab_total_sum = pinus_ab_sum + quercus_ab_sum,
    ica_subtotal_sum = pinus_ica_sum + quercus_ica_sum,
    ima_subtotal_sum = pinus_ima_sum + quercus_ima_sum,
    
    # Dividir por superficie
    densidad_total = densidad_total_sum / superficie_ha,
    ab_total = ab_total_sum / superficie_ha,
    ica_subtotal = ica_subtotal_sum / superficie_ha,
    ima_subtotal = ima_subtotal_sum / superficie_ha
  ) %>%
  select(superficie_ha, densidad_total, ab_total, ica_subtotal, ima_subtotal)

cat(sprintf("  ✓ Superficie total: %.2f ha\n", superficie_total))
cat(sprintf("  ✓ Pinus densidad: %.1f árbol/ha\n", totales_genero_predio$pinus_densidad_ha))
cat(sprintf("  ✓ Quercus densidad: %.1f árbol/ha\n", totales_genero_predio$quercus_densidad_ha))
cat(sprintf("  ✓ Densidad predio: %.1f árboles/ha\n", totales_predio$densidad_total))
cat(sprintf("  ✓ AB predio: %.2f m²/ha\n", totales_predio$ab_total))
cat(sprintf("  ✓ ICA predio: %.2f m³/ha/año\n", totales_predio$ica_subtotal))
cat(sprintf("  ✓ IMA predio: %.2f m³/ha/año\n", totales_predio$ima_subtotal))

# ==============================================================================
# GENERAR TABLA LATEX
# ==============================================================================

cat("\n[4/4] Generando LaTeX...\n")

# Función formatear (3 cifras significativas)
fmt <- function(x) {
  if (is.na(x)) return("0")
  if (x == 0) return("0")
  
  x_sig <- signif(x, 3)
  resultado <- format(x_sig, scientific = FALSE, trim = TRUE)
  
  # Si es entero grande, formatear sin decimales
  if (x_sig >= 100 && x_sig == floor(x_sig)) {
    resultado <- format(floor(x_sig), big.mark = ",", trim = TRUE)
  }
  
  return(resultado)
}

# Iniciar tabla
latex <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Densidad e incrementos (Tabla 7 - Sección 4.10.3 NOM-152)}",
  "\t\\label{tab:densidad_incrementos}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{c@{\\hspace{7pt}}c@{\\hspace{8pt}}l@{\\hspace{7pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c}",
  "\t\t\\toprule",
  "\t\t\\textbf{Unidad mínima} & \\textbf{Sup.} & \\multirow{2}{*}{\\textbf{Especie}} & \\textbf{N.º de} & \\textbf{Área Basal} & \\textbf{Tiempo de Paso} & \\textbf{I.C.A.} & \\textbf{I.M.A.} \\\\",
  "\t\t\\textbf{de manejo} & \\textbf{(ha)} & & \\textbf{árboles/ha} & \\textbf{(m$^2$/ha)} & \\textbf{(años)} & \\textbf{(m$^3$/ha/año)} & \\textbf{(m$^3$/ha/año)} \\\\",
  "\t\t\\midrule"
)

# Filas por UMM
for (i in 1:nrow(datos_umm)) {
  d <- datos_umm[i, ]
  
  # Pinus
  latex <- c(latex,
             sprintf("\t\t\\multirow{3}{*}{%d} & \\multirow{3}{*}{%.1f} & \\textit{Pinus} & %s & %s & %.1f & %s & %s \\\\",
                     d$rodal, d$superficie_ha,
                     fmt(d$pinus_densidad_ha),
                     fmt(d$pinus_ab_ha),
                     TIEMPO_PASO_PINUS,
                     fmt(d$pinus_ica),
                     fmt(d$pinus_ima))
  )
  
  # Quercus
  latex <- c(latex,
             sprintf("\t\t& & \\textit{Quercus} & %s & %s & %.0f & %s & %s \\\\",
                     fmt(d$quercus_densidad_ha),
                     fmt(d$quercus_ab_ha),
                     TIEMPO_PASO_QUERCUS,
                     fmt(d$quercus_ica),
                     fmt(d$quercus_ima))
  )
  
  # Subtotal
  latex <- c(latex,
             sprintf("\t\t\\rowcolor{gray!15} & & \\textbf{Subtotal} & \\textbf{%s} & \\textbf{%s} &  & \\textbf{%s} & \\textbf{%s} \\\\",
                     fmt(d$densidad_total),
                     fmt(d$ab_total),
                     fmt(d$ica_subtotal),
                     fmt(d$ima_subtotal))
  )
  
  # Separador
  if (i < nrow(datos_umm)) {
    latex <- c(latex, "\t\t\\midrule")
  }
}

# Total predio - AGREGAR: TOTAL (superficie), Pinus, Quercus, PREDIO
tg <- totales_genero_predio
tp <- totales_predio

latex <- c(latex,
           "\t\t\\midrule",
           # Fila TOTAL - Pinus
           sprintf("\t\t\\textbf{TOTAL} & \\textbf{%.0f} & \\textit{\\textbf{Pinus}} & \\textbf{%s} & \\textbf{%s} & %.1f & \\textbf{%s} & \\textbf{%s} \\\\",
                   tg$superficie_ha,
                   fmt(tg$pinus_densidad_ha),
                   fmt(tg$pinus_ab_ha),
                   TIEMPO_PASO_PINUS,
                   fmt(tg$pinus_ica),
                   fmt(tg$pinus_ima)),
           # Quercus
           sprintf("\t\t & & \\textit{\\textbf{Quercus}} & \\textbf{%s} & \\textbf{%s} & %.0f & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(tg$quercus_densidad_ha),
                   fmt(tg$quercus_ab_ha),
                   TIEMPO_PASO_QUERCUS,
                   fmt(tg$quercus_ica),
                   fmt(tg$quercus_ima)),
           # PREDIO (subtotal)
           sprintf("\t\t\\rowcolor{gray!35} & & \\textbf{PREDIO} & \\textbf{%s} & \\textbf{%s} &  & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(tp$densidad_total),
                   fmt(tp$ab_total),
                   fmt(tp$ica_subtotal),
                   fmt(tp$ima_subtotal))
)

# Cerrar tabla
latex <- c(latex,
           "\t\t\\bottomrule",
           "\t\\end{tabular}",
           "\t\\\\[0.3cm]",
           "\t{\\scriptsize I.C.A.: Incremento Corriente Anual; I.M.A.: Incremento Medio Anual}",
           "\\end{table}"
)

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex_file <- "resultados/tablas/tabla_7_densidad_incrementos.tex"
writeLines(latex, latex_file)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("LATEX GENERADO:\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat(paste(latex, collapse = "\n"))
cat("\n\n═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Tabla LaTeX guardada: %s\n", latex_file))

# ==============================================================================
# GENERAR CSV
# ==============================================================================

# CSV por UMM y género
csv_umm_genero <- bind_rows(
  # Pinus
  datos_umm %>%
    transmute(
      tipo = "umm",
      rodal,
      superficie_ha,
      genero = "Pinus",
      densidad_ha = pinus_densidad_ha,
      ab_ha = pinus_ab_ha,
      tiempo_paso = TIEMPO_PASO_PINUS,
      ica = pinus_ica,
      ima = pinus_ima
    ),
  # Quercus
  datos_umm %>%
    transmute(
      tipo = "umm",
      rodal,
      superficie_ha,
      genero = "Quercus",
      densidad_ha = quercus_densidad_ha,
      ab_ha = quercus_ab_ha,
      tiempo_paso = TIEMPO_PASO_QUERCUS,
      ica = quercus_ica,
      ima = quercus_ima
    )
)

# CSV subtotales por UMM
csv_umm_subtotal <- datos_umm %>%
  transmute(
    tipo = "umm_subtotal",
    rodal,
    superficie_ha,
    genero = "Subtotal",
    densidad_ha = densidad_total,
    ab_ha = ab_total,
    tiempo_paso = NA,
    ica = ica_subtotal,
    ima = ima_subtotal
  )

# CSV totales por género a nivel predio
tg <- totales_genero_predio

csv_total_genero <- bind_rows(
  tibble(
    tipo = "predio_genero",
    rodal = 999,
    superficie_ha = tg$superficie_ha,
    genero = "Pinus",
    densidad_ha = tg$pinus_densidad_ha,
    ab_ha = tg$pinus_ab_ha,
    tiempo_paso = TIEMPO_PASO_PINUS,
    ica = tg$pinus_ica,
    ima = tg$pinus_ima
  ),
  tibble(
    tipo = "predio_genero",
    rodal = 999,
    superficie_ha = tg$superficie_ha,
    genero = "Quercus",
    densidad_ha = tg$quercus_densidad_ha,
    ab_ha = tg$quercus_ab_ha,
    tiempo_paso = TIEMPO_PASO_QUERCUS,
    ica = tg$quercus_ica,
    ima = tg$quercus_ima
  )
)

# CSV total predio
csv_total <- tibble(
  tipo = "predio_total",
  rodal = 999,
  superficie_ha = totales_predio$superficie_ha,
  genero = "PREDIO",
  densidad_ha = totales_predio$densidad_total,
  ab_ha = totales_predio$ab_total,
  tiempo_paso = NA,
  ica = totales_predio$ica_subtotal,
  ima = totales_predio$ima_subtotal
)

# Consolidar CSV
csv_data <- bind_rows(
  csv_umm_genero,
  csv_umm_subtotal,
  csv_total_genero,
  csv_total
) %>%
  arrange(rodal, genero)

csv_file <- "resultados/tablas/tabla_7_densidad_incrementos.csv"
write_csv(csv_data, csv_file)

cat(sprintf("✓ CSV guardado: %s\n\n", csv_file))

# ==============================================================================
# RESUMEN
# ==============================================================================
writeLines(latex, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/tablas/tabla_7_densidad_incrementos.tex")

cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║              ✓ TABLA 7 GENERADA                          ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")