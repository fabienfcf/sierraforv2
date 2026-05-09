# ==============================================================================
# TABLA DE EXISTENCIAS E ICA POR UMM Y GÉNERO
# Genera tabla LaTeX y CSV con datos ponderados correctamente
# ==============================================================================

library(tidyverse)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA EXISTENCIAS E ICA POR UMM - LATEX + CSV         ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("[1/5] Cargando datos...\n")

# Cargar configuración y programa de cortas
source("config/05_config_programa_cortas.R")

# Cargar CSV con ICA por género y rodal
datos <- read_csv("resultados/31_ica_por_genero_rodal.csv", show_col_types = FALSE)

cat(sprintf("  ✓ Datos cargados: %d filas\n", nrow(datos)))

# Renombrar si es necesario
if ("genero" %in% names(datos) && !"genero_grupo" %in% names(datos)) {
  datos <- datos %>% rename(genero_grupo = genero)
}

# Agregar intensidad y proporción desde PROGRAMA_CORTAS
datos <- datos %>%
  left_join(
    PROGRAMA_CORTAS %>% select(rodal, intensidad_pct, proporcion_quercus),
    by = "rodal"
  )

cat(sprintf("  ✓ Intensidades agregadas\n"))

# ==============================================================================
# CALCULAR POSIBILIDAD
# ==============================================================================

cat("\n[2/5] Calculando posibilidad...\n")

# Posibilidad: PRIMERO calcular total UMM, LUEGO distribuir
datos <- datos %>%
  mutate(
    ICA_10anos = ICA_m3_ha * 10
  ) %>%
  group_by(rodal) %>%
  mutate(
    # Posibilidad TOTAL de la UMM
    posibilidad_total_umm = sum(ICA_10anos) * (first(intensidad_pct) / 100),
    # Distribuir según proporción
    factor_genero = case_when(
      genero_grupo == "Quercus" ~ first(proporcion_quercus),
      genero_grupo == "Pinus" ~ (1 - first(proporcion_quercus)),
      TRUE ~ 0
    ),
    # Posibilidad por género
    posibilidad_m3_ha = posibilidad_total_umm * factor_genero
  ) %>%
  ungroup()

# Totales por UMM
totales_umm <- datos %>%
  group_by(rodal, superficie_total_ha, intensidad_pct) %>%
  summarise(
    ER_m3_ha = sum(ER_m3_ha, na.rm = TRUE),
    ICA_m3_ha = sum(ICA_m3_ha, na.rm = TRUE),
    ICA_10anos = sum(ICA_10anos, na.rm = TRUE),
    posibilidad_m3_ha = sum(posibilidad_m3_ha, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  ✓ Posibilidad calculada\n"))

# ==============================================================================
# TOTALES GENERALES PONDERADOS
# ==============================================================================

cat("\n[3/5] Totales generales...\n")

superficie_total <- sum(unique(datos$superficie_total_ha))

totales_genero <- datos %>%
  mutate(
    ER_pond = ER_m3_ha * superficie_total_ha,
    ICA_pond = ICA_m3_ha * superficie_total_ha,
    ICA_10anos_pond = ICA_10anos * superficie_total_ha,
    pos_pond = posibilidad_m3_ha * superficie_total_ha
  ) %>%
  group_by(genero_grupo) %>%
  summarise(
    ER_m3_ha = sum(ER_pond) / superficie_total,
    ICA_m3_ha = sum(ICA_pond) / superficie_total,
    ICA_10anos = sum(ICA_10anos_pond) / superficie_total,
    posibilidad_m3_ha = sum(pos_pond) / superficie_total,
    .groups = "drop"
  )

total_predio <- totales_genero %>%
  summarise(
    superficie_total_ha = superficie_total,
    ER_m3_ha = sum(ER_m3_ha),
    ICA_m3_ha = sum(ICA_m3_ha),
    ICA_10anos = sum(ICA_10anos),
    posibilidad_m3_ha = sum(posibilidad_m3_ha)
  )

intensidad_promedio <- datos %>%
  distinct(rodal, superficie_total_ha, intensidad_pct) %>%
  summarise(int = sum(intensidad_pct * superficie_total_ha) / sum(superficie_total_ha)) %>%
  pull(int)

cat(sprintf("  ✓ Superficie: %.2f ha\n", superficie_total))
cat(sprintf("  ✓ Intensidad promedio: %.1f%%\n", intensidad_promedio))

# ==============================================================================
# GENERAR LATEX
# ==============================================================================

cat("\n[4/5] Generando LaTeX...\n")

if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)

fmt <- function(x) format(signif(x, 3), scientific = FALSE, trim = TRUE)

latex <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Existencias reales, Incremento Corriente Anual y Posibilidad de corta por UMM}",
  "\t\\label{tab:existencias_ica_umm}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{cccccccc}",
  "\t\t\\toprule",
  "\t\t\\textbf{UMM} & \\textbf{Superficie} & \\textbf{Género} & \\textbf{ER} & \\textbf{ICA} & \\textbf{ICA$\\times$10} & \\textbf{p} & \\textbf{Posibilidad} \\\\",
  "\t\t\\textbf{N.\\textsuperscript{o}} & \\textbf{(ha)} & & \\textbf{(m\\textsuperscript{3}/ha)} & \\textbf{(m\\textsuperscript{3}/ha/a)} & \\textbf{(m\\textsuperscript{3}/ha/año)} & \\textbf{(\\%)} & \\textbf{(m\\textsuperscript{3}/ha)} \\\\",
  "\t\t\\midrule"
)

rodales <- sort(unique(datos$rodal))

for (i in seq_along(rodales)) {
  rid <- rodales[i]
  umm <- datos %>% filter(rodal == rid) %>% arrange(genero_grupo)
  sup <- first(umm$superficie_total_ha)
  int <- first(umm$intensidad_pct)
  
  pinus <- umm %>% filter(genero_grupo == "Pinus")
  if (nrow(pinus) > 0) {
    # p% para Pinus
    p_pinus <- int * (1 - first(pinus$proporcion_quercus))
    latex <- c(latex,
               sprintf("\t\t%d & %.2f & \\textit{Pinus} & %s & %s & %s & %s & %s \\\\",
                       rid, sup, 
                       fmt(pinus$ER_m3_ha), 
                       fmt(pinus$ICA_m3_ha),
                       fmt(pinus$ICA_10anos),
                       fmt(p_pinus),
                       fmt(pinus$posibilidad_m3_ha))
    )
  }
  
  quercus <- umm %>% filter(genero_grupo == "Quercus")
  if (nrow(quercus) > 0) {
    # p% para Quercus
    p_quercus <- int * first(quercus$proporcion_quercus)
    latex <- c(latex,
               sprintf("\t\t & & \\textit{Quercus} & %s & %s & %s & %s & %s \\\\",
                       fmt(quercus$ER_m3_ha), 
                       fmt(quercus$ICA_m3_ha),
                       fmt(quercus$ICA_10anos),
                       fmt(p_quercus),
                       fmt(quercus$posibilidad_m3_ha))
    )
  }
  
  tot <- totales_umm %>% filter(rodal == rid)
  latex <- c(latex,
             sprintf("\t\t\\rowcolor{gray!15} & & \\textbf{UMM %d} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                     rid, 
                     fmt(tot$ER_m3_ha), 
                     fmt(tot$ICA_m3_ha),
                     fmt(tot$ICA_10anos),
                     fmt(int),
                     fmt(tot$posibilidad_m3_ha))
  )
  
  if (i < length(rodales)) latex <- c(latex, "\t\t\\midrule")
}

latex <- c(latex,
           "\t\t\\midrule"
)

pin_gen <- totales_genero %>% filter(genero_grupo == "Pinus")
# p% ponderado para Pinus
p_pinus_gen <- datos %>%
  distinct(rodal, superficie_total_ha, intensidad_pct, proporcion_quercus) %>%
  summarise(p = sum(intensidad_pct * (1 - proporcion_quercus) * superficie_total_ha) / sum(superficie_total_ha)) %>%
  pull(p)

latex <- c(latex,
           sprintf("\t\t\\textbf{TOTAL} & \\textbf{%.2f} & \\textit{\\textbf{Pinus}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   superficie_total, 
                   fmt(pin_gen$ER_m3_ha), 
                   fmt(pin_gen$ICA_m3_ha),
                   fmt(pin_gen$ICA_10anos),
                   fmt(p_pinus_gen),
                   fmt(pin_gen$posibilidad_m3_ha))
)

que_gen <- totales_genero %>% filter(genero_grupo == "Quercus")
# p% ponderado para Quercus
p_quercus_gen <- datos %>%
  distinct(rodal, superficie_total_ha, intensidad_pct, proporcion_quercus) %>%
  summarise(p = sum(intensidad_pct * proporcion_quercus * superficie_total_ha) / sum(superficie_total_ha)) %>%
  pull(p)

latex <- c(latex,
           sprintf("\t\t & & \\textit{\\textbf{Quercus}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(que_gen$ER_m3_ha), 
                   fmt(que_gen$ICA_m3_ha),
                   fmt(que_gen$ICA_10anos),
                   fmt(p_quercus_gen),
                   fmt(que_gen$posibilidad_m3_ha))
)

latex <- c(latex,
           sprintf("\t\t\\rowcolor{gray!30} & & \\textbf{PREDIO} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(total_predio$ER_m3_ha), 
                   fmt(total_predio$ICA_m3_ha),
                   fmt(total_predio$ICA_10anos),
                   fmt(intensidad_promedio),
                   fmt(total_predio$posibilidad_m3_ha))
)

latex <- c(latex,
           "\t\t\\bottomrule",
           "\t\\end{tabular}",
           "\t\\\\[0.3cm]",
           "\t{\\scriptsize ER: Existencias Reales; ICA: Incremento Corriente Anual; p: porcentaje de aprovechamiento del ICA$\\times$10.}",
           "\\end{table}"
)

latex_file <- "resultados/tablas/tabla_existencias_ica.tex"
writeLines(latex, latex_file)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("LATEX GENERADO:\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat(paste(latex, collapse = "\n"))
cat("\n\n═══════════════════════════════════════════════════════════\n\n")

# ==============================================================================
# CSV
# ==============================================================================

cat("[5/5] CSV...\n")

csv_data <- bind_rows(
  datos %>% mutate(tipo = "umm_genero") %>%
    select(tipo, rodal, genero_grupo, superficie_total_ha, ER_m3_ha, ICA_m3_ha, intensidad_pct, posibilidad_m3_ha),
  totales_umm %>% mutate(tipo = "total_umm", genero_grupo = "TOTAL") %>%
    select(tipo, rodal, genero_grupo, superficie_total_ha, ER_m3_ha, ICA_m3_ha, intensidad_pct, posibilidad_m3_ha)
)

write_csv(csv_data, "resultados/tablas/tabla_existencias_ica.csv")
cat("  ✓ CSV generado\n\n")