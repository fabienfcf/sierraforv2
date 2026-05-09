# ==============================================================================
# TABLA 6 - RESUMEN DE EXISTENCIAS A NIVEL PREDIO (NOM-152)
# Genera LaTeX y CSV con totales por género
# ==============================================================================

library(tidyverse)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA 6 - RESUMEN EXISTENCIAS PREDIO (NOM-152)        ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("[1/4] Cargando datos...\n")

# Evolución
evolucion <- read_csv("resultados/evolucion_rodal_10anos.csv", show_col_types = FALSE)

# Superficie por UMM
umm_stats <- read_csv("UMM_stats.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  select(rodal = id,
         superficie_ha = `SUPERFICIE UMM`,
         superficie_corta_ha = `Superficie en Producción (ha)`)

# ICA para cálculo de posibilidad
ica_data <- read_csv("resultados/31_ica_por_genero_rodal.csv", show_col_types = FALSE)

# Renombrar si es necesario
if ("genero" %in% names(ica_data) && !"genero_grupo" %in% names(ica_data)) {
  ica_data <- ica_data %>% rename(genero_grupo = genero)
}

# Programa de cortas
source("config/05_config_programa_cortas.R")

cat(sprintf("  ✓ Datos cargados\n"))

# ==============================================================================
# CALCULAR EXISTENCIAS REALES INICIALES (t=0)
# ==============================================================================

cat("\n[2/4] Calculando existencias iniciales (t=0)...\n")

er_inicial <- evolucion %>%
  filter(ano_simulacion == 0) %>%
  left_join(umm_stats, by = "rodal") %>%
  group_by(rodal) %>%
  summarise(
    superficie_ha = first(superficie_ha),
    pinus_vol_m3 = pinus_vol_ha_m3 * superficie_ha,
    quercus_vol_m3 = quercus_vol_ha_m3 * superficie_ha,
    .groups = "drop"
  )

# Totales por género
totales_er_inicial <- er_inicial %>%
  summarise(
    pinus_vol_m3 = sum(pinus_vol_m3, na.rm = TRUE),
    quercus_vol_m3 = sum(quercus_vol_m3, na.rm = TRUE),
    total_vol_m3 = pinus_vol_m3 + quercus_vol_m3
  )

cat(sprintf("  ✓ ER inicial Pinus: %.0f m³\n", totales_er_inicial$pinus_vol_m3))
cat(sprintf("  ✓ ER inicial Quercus: %.0f m³\n", totales_er_inicial$quercus_vol_m3))
cat(sprintf("  ✓ ER inicial TOTAL: %.0f m³\n", totales_er_inicial$total_vol_m3))

# ==============================================================================
# CALCULAR EXISTENCIAS RESIDUALES (t=10)
# ==============================================================================

cat("\n[3/4] Calculando residuales (t=10)...\n")

er_final <- evolucion %>%
  filter(ano_simulacion == 10) %>%
  left_join(umm_stats, by = "rodal") %>%
  group_by(rodal) %>%
  summarise(
    superficie_ha = first(superficie_ha),
    pinus_vol_m3 = pinus_vol_ha_m3 * superficie_ha,
    quercus_vol_m3 = quercus_vol_ha_m3 * superficie_ha,
    .groups = "drop"
  )

# Totales por género
totales_er_final <- er_final %>%
  summarise(
    pinus_vol_m3 = sum(pinus_vol_m3, na.rm = TRUE),
    quercus_vol_m3 = sum(quercus_vol_m3, na.rm = TRUE),
    total_vol_m3 = pinus_vol_m3 + quercus_vol_m3
  )

cat(sprintf("  ✓ Residuales Pinus: %.0f m³\n", totales_er_final$pinus_vol_m3))
cat(sprintf("  ✓ Residuales Quercus: %.0f m³\n", totales_er_final$quercus_vol_m3))
cat(sprintf("  ✓ Residuales TOTAL: %.0f m³\n", totales_er_final$total_vol_m3))

# ==============================================================================
# CALCULAR POSIBILIDAD (MISMA LÓGICA QUE TABLA 5)
# ==============================================================================

cat("\n[4/4] Calculando posibilidad...\n")

# Agregar configuración de cortas
ica_data <- ica_data %>%
  left_join(
    PROGRAMA_CORTAS %>% select(rodal, intensidad_pct, proporcion_quercus),
    by = "rodal"
  )

# Calcular posibilidad por UMM
posibilidad_umm <- ica_data %>%
  mutate(ICA_10anos = ICA_m3_ha * 10) %>%
  group_by(rodal) %>%
  mutate(
    # PASO 1: Posibilidad TOTAL de la UMM
    posibilidad_total_umm = sum(ICA_10anos) * (first(intensidad_pct) / 100),
    
    # PASO 2: Factor por género
    factor_genero = case_when(
      genero_grupo == "Quercus" ~ first(proporcion_quercus),
      genero_grupo == "Pinus" ~ (1 - first(proporcion_quercus)),
      TRUE ~ 0
    ),
    
    # PASO 3: Posibilidad por género (m³/ha)
    posibilidad_m3_ha = posibilidad_total_umm * factor_genero
  ) %>%
  ungroup() %>%
  left_join(umm_stats, by = "rodal") %>%
  mutate(
    # Convertir a m³ totales
    posibilidad_m3 = posibilidad_m3_ha * superficie_corta_ha
  )

# Totales por género
totales_posibilidad <- posibilidad_umm %>%
  group_by(genero_grupo) %>%
  summarise(
    posibilidad_m3 = sum(posibilidad_m3, na.rm = TRUE),
    .groups = "drop"
  )

pinus_pos <- totales_posibilidad %>% filter(genero_grupo == "Pinus") %>% pull(posibilidad_m3)
quercus_pos <- totales_posibilidad %>% filter(genero_grupo == "Quercus") %>% pull(posibilidad_m3)
total_pos <- pinus_pos + quercus_pos

cat(sprintf("  ✓ Posibilidad Pinus: %.0f m³\n", pinus_pos))
cat(sprintf("  ✓ Posibilidad Quercus: %.0f m³\n", quercus_pos))
cat(sprintf("  ✓ Posibilidad TOTAL: %.0f m³\n", total_pos))

# ==============================================================================
# GENERAR TABLA LATEX
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("GENERANDO TABLA LATEX...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Función formatear (3 cifras significativas con separador de miles)
fmt <- function(x) {
  if (is.na(x)) return("0")
  if (x == 0) return("0")
  
  x_sig <- signif(x, 3)
  
  # Formatear con separador de miles
  resultado <- format(x_sig, scientific = FALSE, big.mark = ",", trim = TRUE)
  
  # Si es entero, quitar decimales
  if (x_sig >= 100 && x_sig == floor(x_sig)) {
    resultado <- format(floor(x_sig), big.mark = ",", trim = TRUE)
  }
  
  return(resultado)
}

# Crear tabla
latex <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Resumen de existencias a nivel de predio o conjunto predial (Tabla 6 - Sección 4.10.2 NOM-152)}",
  "\t\\label{tab:resumen_existencias_predio}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{lccc}",
  "\t\t\\toprule",
  "\t\t\\textbf{Especie} & \\textbf{Existencias reales en t=0 (m$^3$ VTA)} & \\textbf{Posibilidad (m$^3$ VTA)} & \\textbf{Residuales en t=10 (m$^3$ VTA)} \\\\",
  "\t\t\\midrule",
  sprintf("\t\t\\textit{Pinus} & %s & %s & %s \\\\",
          fmt(totales_er_inicial$pinus_vol_m3),
          fmt(pinus_pos),
          fmt(totales_er_final$pinus_vol_m3)),
  sprintf("\t\t\\textit{Quercus} & %s & %s & %s \\\\",
          fmt(totales_er_inicial$quercus_vol_m3),
          fmt(quercus_pos),
          fmt(totales_er_final$quercus_vol_m3)),
  "\t\t\\midrule",
  sprintf("\t\t\\textbf{Total} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
          fmt(totales_er_inicial$total_vol_m3),
          fmt(total_pos),
          fmt(totales_er_final$total_vol_m3)),
  "\t\t\\bottomrule",
  "\t\\end{tabular}",
  "\t\\\\[0.3cm]",
  "%\t{\\scriptsize Esta tabla debe incluirse en el cuerpo del Programa de Manejo Forestal.}",
  "\\end{table}"
)

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex_file <- "resultados/tablas/tabla_6_resumen_existencias_predio.tex"
writeLines(latex, latex_file)
writeLines(latex, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/tablas/Tabla 6_resumen_existencias_predio.tex")

cat(paste(latex, collapse = "\n"))
cat("\n\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Tabla LaTeX guardada: %s\n", latex_file))

# ==============================================================================
# GENERAR CSV
# ==============================================================================

csv_data <- tibble(
  genero = c("Pinus", "Quercus", "Total"),
  er_inicial_m3 = c(
    totales_er_inicial$pinus_vol_m3,
    totales_er_inicial$quercus_vol_m3,
    totales_er_inicial$total_vol_m3
  ),
  posibilidad_m3 = c(
    pinus_pos,
    quercus_pos,
    total_pos
  ),
  residuales_m3 = c(
    totales_er_final$pinus_vol_m3,
    totales_er_final$quercus_vol_m3,
    totales_er_final$total_vol_m3
  )
)

csv_file <- "resultados/tablas/tabla_6_resumen_existencias_predio.csv"
write_csv(csv_data, csv_file)

cat(sprintf("✓ CSV guardado: %s\n\n", csv_file))

# ==============================================================================
# RESUMEN
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║              ✓ TABLA 6 GENERADA                          ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("RESUMEN:\n")
cat(sprintf("  ER inicial:  %s m³ VTA\n", fmt(totales_er_inicial$total_vol_m3)))
cat(sprintf("  Posibilidad: %s m³ VTA\n", fmt(total_pos)))
cat(sprintf("  Residuales:  %s m³ VTA\n\n", fmt(totales_er_final$total_vol_m3)))