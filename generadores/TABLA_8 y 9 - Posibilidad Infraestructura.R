# ==============================================================================
# TABLA 8 Y 9 - POSIBILIDAD ANUAL Y PLAN DE CORTAS (NOM-152)
# Genera LaTeX y CSV según formato oficial
# CORRECCIÓN: Volumen por infraestructura calculado POR GÉNERO
# ==============================================================================

library(tidyverse)
source("config/04_config_simulacion.R")
source("config/05_config_programa_cortas.R")

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA 8 Y 9 - POSIBILIDAD ANUAL (NOM-152)             ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 1. ÁREA POR INFRAESTRUCTURA (TIBBLE MANUAL)
# ==============================================================================

cat("[1/6] Definiendo área por infraestructura...\n")

# Tibble de área de infraestructura por UMM (hectáreas)
area_infraestructura <- tribble(
  ~rodal, ~area_infraestructura_ha,
  1,      0.12,
  2,      0.13,
  3,      0.15,
  4,      0.29,
  5,      0.15,
  6,      0.28
)

cat(sprintf("  ✓ Área de infraestructura definida para %d UMM\n", nrow(area_infraestructura)))

# ==============================================================================
# 2. TRATAMIENTO SILVÍCOLA (TIBBLE MANUAL - POR UMM × GÉNERO)
# ==============================================================================

cat("[2/6] Definiendo tratamiento silvícola...\n")

# Tratamiento por UMM y género
# TODO: Selección EXCEPTO UMM 3 y 4 × Pinus = Aclareo
tratamiento <- tribble(
  ~rodal, ~genero_grupo, ~tratamiento_silvicola,
  1,      "Pinus",       "No Corta",
  1,      "Quercus",     "Selección",
  2,      "Pinus",       "No Corta",
  2,      "Quercus",     "Selección",
  3,      "Pinus",       "Aclareo",
  3,      "Quercus",     "Selección",
  4,      "Pinus",       "Aclareo",
  4,      "Quercus",     "Selección",
  5,      "Pinus",       "Selección",
  5,      "Quercus",     "Selección",
  6,      "Pinus",       "Selección",
  6,      "Quercus",     "Selección"
)

cat(sprintf("  ✓ Tratamiento definido para %d combinaciones UMM×Género\n", nrow(tratamiento)))

# ==============================================================================
# 3. CARGAR DATOS
# ==============================================================================

cat("[3/6] Cargando datos...\n")

# ICA por rodal y género (contiene ER_m3_ha por género)
ica_genero_rodal <- read_csv("resultados/31_ica_por_genero_rodal.csv", show_col_types = FALSE) %>%
  filter(rodal != 0) %>%  # ← Excluir UMM 0
  select(rodal, genero, ER_m3_ha)

# Posibilidad por género y UMM
posibilidad <- read_csv("resultados/cortas_resumen_rodal_genero.csv", show_col_types = FALSE) %>%
  filter(rodal_cortado != 0) %>%  # ← Excluir UMM 0
  select(rodal_cortado, ano_corta, genero_grupo, vol_total_umm_m3) %>%
  rename(rodal = rodal_cortado)

# Superficie por UMM
umm_stats <- read_csv("UMM_stats.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  filter(id != 0) %>%  # ← Excluir UMM 0
  select(rodal = id,
         superficie_ha = `SUPERFICIE UMM`,
         superficie_corta_ha = `Superficie en Producción (ha)`)

cat(sprintf("  ✓ Datos cargados (sin UMM 0)\n"))

# ==============================================================================
# 4. CALCULAR VOLUMEN POR INFRAESTRUCTURA (m³ VTA) - POR GÉNERO
# ==============================================================================

cat("[4/6] Calculando volumen por infraestructura POR GÉNERO...\n")

# Vol infraestructura = área_infraestructura × ER_m3_ha (por género)
# Necesitamos expandir para cada género
vol_infraestructura <- area_infraestructura %>%
  # Expandir para ambos géneros
  crossing(genero_grupo = c("Pinus", "Quercus")) %>%
  # Join con ER por género
  left_join(
    ica_genero_rodal %>% rename(genero_grupo = genero),
    by = c("rodal", "genero_grupo")
  ) %>%
  mutate(
    vol_infraestructura_m3 = area_infraestructura_ha * ER_m3_ha
  )

cat(sprintf("  ✓ Volumen calculado para %d combinaciones UMM×Género\n", 
            nrow(vol_infraestructura)))

# DIAGNÓSTICO
cat("\n  DIAGNÓSTICO - Volumen infraestructura por género:\n")
vol_infra_summary <- vol_infraestructura %>%
  group_by(genero_grupo) %>%
  summarise(
    vol_total = sum(vol_infraestructura_m3, na.rm = TRUE),
    .groups = "drop"
  )
print(vol_infra_summary)

# ==============================================================================
# 5. PREPARAR DATOS TABLA 8 (POR UMM)
# ==============================================================================

cat("[5/6] Preparando Tabla 8 (por UMM)...\n")

# Expandir posibilidad para tener TODAS las combinaciones UMM × Género
# (incluir géneros sin corta con valor 0)
posibilidad_completa <- expand_grid(
  rodal = unique(umm_stats$rodal),
  genero_grupo = c("Pinus", "Quercus")
) %>%
  left_join(
    posibilidad %>% select(rodal, ano_corta, genero_grupo, vol_total_umm_m3),
    by = c("rodal", "genero_grupo")
  ) %>%
  mutate(
    vol_total_umm_m3 = replace_na(vol_total_umm_m3, 0)
  )

# DIAGNÓSTICO: Verificar años de corta
cat("\n  DIAGNÓSTICO - Años de corta por UMM:\n")
anos_por_umm <- posibilidad_completa %>%
  distinct(rodal, ano_corta) %>%
  arrange(rodal)
print(anos_por_umm)

# Si ano_corta es NA, asignar año por defecto (obtener del programa de cortas)
posibilidad_completa <- posibilidad_completa %>%
  left_join(
    PROGRAMA_CORTAS %>% select(rodal, ano_corta_prog = ano_corta),
    by = "rodal"
  ) %>%
  mutate(
    ano_corta = coalesce(ano_corta, ano_corta_prog)
  ) %>%
  select(-ano_corta_prog)

cat("\n  ✓ Años de corta corregidos\n")

# Consolidar datos por UMM × Género
tabla8_data <- posibilidad_completa %>%
  left_join(umm_stats, by = "rodal") %>%
  left_join(tratamiento, by = c("rodal", "genero_grupo")) %>%
  left_join(vol_infraestructura, by = c("rodal", "genero_grupo")) %>%
  rename(pos_genero = vol_total_umm_m3) %>%
  arrange(ano_corta, rodal, genero_grupo)

# DIAGNÓSTICO: Verificar valores NULL
cat("\n  DIAGNÓSTICO - Valores NULL:\n")
cat(sprintf("    pos_genero NULL: %d\n", sum(is.na(tabla8_data$pos_genero))))
cat(sprintf("    ano_corta NULL: %d\n", sum(is.na(tabla8_data$ano_corta))))
cat(sprintf("    superficie_ha NULL: %d\n", sum(is.na(tabla8_data$superficie_ha))))
cat(sprintf("    vol_infraestructura_m3 NULL: %d\n", 
            sum(is.na(tabla8_data$vol_infraestructura_m3))))

# Calcular subtotales por UMM (sumando ambos géneros)
subtotales_umm <- tabla8_data %>%
  group_by(rodal, ano_corta) %>%
  summarise(
    superficie_ha = first(superficie_ha),
    area_infraestructura_ha = first(area_infraestructura_ha),
    pos_subtotal = sum(pos_genero, na.rm = TRUE),
    vol_infra_subtotal = sum(vol_infraestructura_m3, na.rm = TRUE),
    pos_infra_total = pos_subtotal + vol_infra_subtotal,
    .groups = "drop"
  )

cat(sprintf("\n  ✓ Tabla 8 preparada para %d UMM × Género\n", nrow(tabla8_data)))

# ==============================================================================
# 6. CALCULAR TOTALES
# ==============================================================================

cat("[6/6] Calculando totales...\n")

# Totales por género (posibilidad + infraestructura)
totales_genero <- tabla8_data %>%
  group_by(genero_grupo) %>%
  summarise(
    pos_total = sum(pos_genero, na.rm = TRUE),
    vol_infra_total = sum(vol_infraestructura_m3, na.rm = TRUE),
    .groups = "drop"
  )

# Total infraestructura (suma de ambos géneros)
total_infraestructura <- sum(totales_genero$vol_infra_total, na.rm = TRUE)

# Total posibilidad
total_posibilidad <- sum(totales_genero$pos_total)

# Gran total
gran_total <- total_posibilidad + total_infraestructura

cat(sprintf("  ✓ Totales calculados\n"))
cat(sprintf("    Posibilidad Pinus:        %.2f m³\n", 
            totales_genero$pos_total[totales_genero$genero_grupo == "Pinus"]))
cat(sprintf("    Posibilidad Quercus:      %.2f m³\n", 
            totales_genero$pos_total[totales_genero$genero_grupo == "Quercus"]))
cat(sprintf("    Posibilidad TOTAL:        %.2f m³\n", total_posibilidad))
cat(sprintf("    Infraestructura Pinus:    %.2f m³\n", 
            totales_genero$vol_infra_total[totales_genero$genero_grupo == "Pinus"]))
cat(sprintf("    Infraestructura Quercus:  %.2f m³\n", 
            totales_genero$vol_infra_total[totales_genero$genero_grupo == "Quercus"]))
cat(sprintf("    Infraestructura TOTAL:    %.2f m³\n", total_infraestructura))
cat(sprintf("    GRAN TOTAL:               %.2f m³\n", gran_total))

# ==============================================================================
# GENERAR TABLA 8 - LATEX (NUEVA ESTRUCTURA)
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("GENERANDO TABLA 8 (por UMM)...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Función formatear (3 cifras para < 1000, 4 cifras para >= 1000)
fmt <- function(x) {
  # Gérer cas NULL ou vecteur vide
  if (is.null(x) || length(x) == 0) return("0.00")
  
  # Prendre premier élément si vecteur
  if (length(x) > 1) {
    warning("fmt() reçu vecteur de longueur ", length(x), ", prenant premier élément")
    x <- x[1]
  }
  
  if (is.na(x)) return("---")
  if (x == 0) return("0.00")
  
  # 4 cifras significativas para >= 1000, 3 cifras para < 1000
  n_sig <- ifelse(abs(x) >= 1000, 4, 3)
  x_sig <- signif(x, n_sig)
  
  # Formatear con separador de miles
  resultado <- format(x_sig, scientific = FALSE, big.mark = ",", trim = TRUE)
  
  # Si es entero grande (>= 100), quitar decimales innecesarios
  if (x_sig >= 100 && x_sig == floor(x_sig)) {
    resultado <- format(floor(x_sig), big.mark = ",", trim = TRUE)
  }
  
  return(resultado)
}

# Iniciar tabla
latex8 <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Posibilidad anual y plan de cortas, por unidad mínima de manejo (Tabla 8 - Sección 4.12.1.1 NOM-152)}",
  "\t\\label{tab:posibilidad_umm}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{c@{\\hspace{5pt}}c@{\\hspace{5pt}}c@{\\hspace{5pt}}l@{\\hspace{5pt}}c@{\\hspace{5pt}}c@{\\hspace{5pt}}c@{\\hspace{5pt}}c}",
  "\t\t\\toprule",
  "\t\t\\textbf{Área de} & \\textbf{UMM} & \\textbf{Superficie} & \\textbf{Tratamiento} & \\multicolumn{2}{c}{\\textbf{Posibilidad}} & \\textbf{Vol. por} & \\textbf{Posibilidad +} \\\\",
  "\t\t\\textbf{Corta} & \\textbf{No.} & \\textbf{(ha)} & \\textbf{Silvícola} & \\textbf{Género} & \\textbf{m$^3$ VTA} & \\textbf{infraest. (m$^3$ VTA)} & \\textbf{vol. infraest. (m$^3$ VTA)} \\\\",
  "\t\t\\midrule"
)

# Obtener UMMs únicos ordenados
rodales_unicos <- tabla8_data %>%
  distinct(rodal, ano_corta) %>%
  arrange(ano_corta, rodal)

# Iterar por UMM
for (i in 1:nrow(rodales_unicos)) {
  rodal_actual <- rodales_unicos$rodal[i]
  ano_actual <- rodales_unicos$ano_corta[i]
  
  # Datos de este UMM
  datos_umm <- tabla8_data %>%
    filter(rodal == rodal_actual, ano_corta == ano_actual) %>%
    arrange(genero_grupo)  # Pinus primero
  
  # Subtotal de este UMM (tomar primera fila si hay duplicados)
  subtotal_umm <- subtotales_umm %>%
    filter(rodal == rodal_actual) %>%
    slice(1)  # Asegurar una sola fila
  
  # Fila por género
  for (j in 1:nrow(datos_umm)) {
    d <- datos_umm[j, ]
    
    # Vol infraestructura se muestra en cada línea de género
    vol_infra_genero <- fmt(d$vol_infraestructura_m3)
    
    # Posibilidad + infraestructura (suma por género)
    pos_infra_genero <- as.numeric(d$pos_genero) + as.numeric(d$vol_infraestructura_m3)
    
    latex8 <- c(latex8,
                sprintf("\t\t%s & %s & %s & %s & \\textit{%s} & %s & %s & %s \\\\",
                        ifelse(j == 1, sprintf("%d", INICIO_SIMULACION + ano_actual - 1), ""),
                        ifelse(j == 1, as.character(rodal_actual), ""),
                        ifelse(j == 1, fmt(d$superficie_ha), ""),
                        d$tratamiento_silvicola,
                        d$genero_grupo,
                        fmt(d$pos_genero),
                        vol_infra_genero,
                        fmt(pos_infra_genero))
    )
  }
  
  # Fila SUBTOTAL (grisée) sans répéter UMM ni superficie
  latex8 <- c(latex8,
              sprintf("\t\t\\rowcolor{gray!15} \\textbf{Subtotal} & & & & \\textbf{Subtotal} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                      fmt(as.numeric(subtotal_umm$pos_subtotal)),
                      fmt(as.numeric(subtotal_umm$vol_infra_subtotal)),
                      fmt(as.numeric(subtotal_umm$pos_infra_total)))
  )
  
  # Separador
  if (i < nrow(rodales_unicos)) {
    latex8 <- c(latex8, "\t\t\\midrule")
  }
}

# FILA TOTAL
latex8 <- c(latex8, "\t\t\\midrule")

# Total superficie
superficie_total <- sum(umm_stats$superficie_ha)

# Total par género
for (genero in c("Pinus", "Quercus")) {
  datos_genero <- totales_genero %>% filter(genero_grupo == genero)
  
  pos_total_genero <- datos_genero$pos_total
  vol_infra_genero <- datos_genero$vol_infra_total
  total_genero <- pos_total_genero + vol_infra_genero
  
  latex8 <- c(latex8,
              sprintf("\t\t%s & & %s & & \\textit{\\textbf{%s}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                      ifelse(genero == "Pinus", "\\textbf{Total}", ""),
                      ifelse(genero == "Pinus", sprintf("\\textbf{%.0f}", superficie_total), ""),
                      genero,
                      fmt(pos_total_genero),
                      fmt(vol_infra_genero),
                      fmt(total_genero))
  )
}

# Fila PREDIO (grisée)
latex8 <- c(latex8,
            sprintf("\t\t\\rowcolor{gray!35} \\textbf{PREDIO} & & \\textbf{%.0f} & & \\textbf{PREDIO} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                    superficie_total,
                    fmt(total_posibilidad),
                    fmt(total_infraestructura),
                    fmt(gran_total))
)

# Cerrar tabla
latex8 <- c(latex8,
            "\t\t\\bottomrule",
            "\t\\end{tabular}",
            "\t\\\\[0.3cm]",
            "\t{\\scriptsize m$^3$ VTA: metros cúbicos de volumen total árbol}",
            "\\end{table}"
)

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex8_file <- "resultados/tablas/tabla_8_posibilidad_umm.tex"
writeLines(latex8, latex8_file)
writeLines(latex8, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/tablas/tabla_8_posibilidad_umm.tex")


cat(paste(latex8, collapse = "\n"))
cat("\n\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Tabla 8 guardada: %s\n", latex8_file))

# ==============================================================================
# GENERAR TABLA 9 - RESUMEN POR ÁREA DE CORTA (año)
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("GENERANDO TABLA 9 (por Área de Corta)...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Agrupar por año de corta y género
tabla9_data <- tabla8_data %>%
  group_by(ano_corta, genero_grupo) %>%
  summarise(
    pos_genero = sum(pos_genero, na.rm = TRUE),
    vol_infra_genero = sum(vol_infraestructura_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ano_corta, genero_grupo)

# Subtotales por año
subtotales_ano <- tabla9_data %>%
  group_by(ano_corta) %>%
  summarise(
    pos_subtotal = sum(pos_genero, na.rm = TRUE),
    vol_infra_subtotal = sum(vol_infra_genero, na.rm = TRUE),
    pos_infra_total = pos_subtotal + vol_infra_subtotal,
    .groups = "drop"
  )

# Especies aprovechadas por género
especies_pinus <- c(
  "\\textit{Pinus pseudostrobus}",
  "\\textit{Pinus teocote}"
)
especies_quercus <- c(
  "\\textit{Quercus laeta}",
  "\\textit{Quercus rysophylla}"
)

especies_pinus_str <- paste(especies_pinus, collapse = ", ")
especies_quercus_str <- paste(especies_quercus, collapse = ", ")

# Iniciar tabla 9
latex9 <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Posibilidad anual y plan de cortas, resumen (Tabla 9 - Sección 4.12.1.2 NOM-152)}",
  "\t\\label{tab:posibilidad_resumen}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{c@{\\hspace{8pt}}c@{\\hspace{8pt}}c@{\\hspace{8pt}}c@{\\hspace{8pt}}c@{\\hspace{8pt}}l}",
  "\t\t\\toprule",
  "\t\t\\textbf{Área de} & \\multicolumn{2}{c}{\\textbf{Posibilidad}} & \\textbf{Vol. por infraest.} & \\textbf{Posibilidad + vol.} & \\textbf{Especies por} \\\\",
  "\t\t\\textbf{Corta No.} & \\textbf{Especie} & \\textbf{m$^3$ VTA} & \\textbf{(m$^3$ VTA)} & \\textbf{por infraest. (m$^3$ VTA)} & \\textbf{aprovechar} \\\\",
  "\t\t\\midrule"
)

# Años únicos
anos_unicos <- sort(unique(tabla9_data$ano_corta))

# Iterar por año
for (ano in anos_unicos) {
  # Datos de este año
  datos_ano <- tabla9_data %>%
    filter(ano_corta == ano) %>%
    arrange(genero_grupo)
  
  subtotal_ano <- subtotales_ano %>%
    filter(ano_corta == ano) %>%
    slice(1)  # Asegurar una sola fila
  
  # Fila por género
  for (j in 1:nrow(datos_ano)) {
    d <- datos_ano[j, ]
    
    # Posibilidad + infraestructura por género
    pos_infra_genero <- as.numeric(d$pos_genero) + as.numeric(d$vol_infra_genero)
    
    # Especies selon le género
    especies_genero <- if (d$genero_grupo == "Pinus") {
      sprintf("\\parbox{3cm}{%s}", especies_pinus_str)
    } else {
      sprintf("\\parbox{3cm}{%s}", especies_quercus_str)
    }
    
    latex9 <- c(latex9,
                sprintf("\t\t%s & \\textit{%s} & %s & %s & %s & %s \\\\",
                        ifelse(j == 1, sprintf("%d", INICIO_SIMULACION + ano - 1), ""),
                        d$genero_grupo,
                        fmt(d$pos_genero),
                        fmt(d$vol_infra_genero),
                        fmt(pos_infra_genero),
                        especies_genero)
    )
  }
  
  # Fila SUBTOTAL (grisée)
  latex9 <- c(latex9,
              sprintf("\t\t\\rowcolor{gray!15} \\textbf{Subtotal} & \\textbf{Subtotal} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\\\",
                      fmt(as.numeric(subtotal_ano$pos_subtotal)),
                      fmt(as.numeric(subtotal_ano$vol_infra_subtotal)),
                      fmt(as.numeric(subtotal_ano$pos_infra_total)))
  )
  
  # Separador
  if (ano != max(anos_unicos)) {
    latex9 <- c(latex9, "\t\t\\midrule")
  }
}

# FILA TOTAL
latex9 <- c(latex9, "\t\t\\midrule")

# Total par género
for (genero in c("Pinus", "Quercus")) {
  datos_genero <- totales_genero %>% filter(genero_grupo == genero)
  
  pos_total_genero <- datos_genero$pos_total
  vol_infra_genero <- datos_genero$vol_infra_total
  total_genero <- pos_total_genero + vol_infra_genero
  
  # Especies selon le género
  especies_genero <- if (genero == "Pinus") {
    sprintf("\\parbox{3cm}{%s}", especies_pinus_str)
  } else {
    sprintf("\\parbox{3cm}{%s}", especies_quercus_str)
  }
  
  latex9 <- c(latex9,
              sprintf("\t\t%s & \\textit{\\textbf{%s}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & %s \\\\",
                      ifelse(genero == "Pinus", "\\textbf{Total}", ""),
                      genero,
                      fmt(pos_total_genero),
                      fmt(vol_infra_genero),
                      fmt(total_genero),
                      especies_genero)
  )
}

# Fila PREDIO (grisée)
latex9 <- c(latex9,
            sprintf("\t\t\\rowcolor{gray!35} \\textbf{PREDIO} & \\textbf{PREDIO} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\\\",
                    fmt(total_posibilidad),
                    fmt(total_infraestructura),
                    fmt(gran_total))
)

# Cerrar tabla
latex9 <- c(latex9,
            "\t\t\\bottomrule",
            "\t\\end{tabular}",
            "\t\\\\[0.3cm]",
            "\t{\\scriptsize m$^3$ VTA: metros cúbicos de volumen total árbol}",
            "\\end{table}"
)

# Escribir archivo
latex9_file <- "resultados/tablas/tabla_9_posibilidad_resumen.tex"
writeLines(latex9, latex9_file)
writeLines(latex9, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/tablas/tabla_9_posibilidad_resumen.tex")


cat(paste(latex9, collapse = "\n"))
cat("\n\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Tabla 9 guardada: %s\n", latex9_file))

# ==============================================================================
# GENERAR CSV
# ==============================================================================

csv8_file <- "resultados/tablas/tabla_8_posibilidad_umm.csv"
write_csv(tabla8_data, csv8_file)
cat(sprintf("✓ CSV Tabla 8 guardado: %s\n", csv8_file))

csv9_file <- "resultados/tablas/tabla_9_posibilidad_resumen.csv"
write_csv(tabla9_data, csv9_file)
cat(sprintf("✓ CSV Tabla 9 guardado: %s\n\n", csv9_file))

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║       ✓ TABLAS 8 Y 9 GENERADAS                          ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat(sprintf("  📄 %s\n", latex8_file))
cat(sprintf("  📄 %s\n", latex9_file))
cat(sprintf("  📊 %s\n", csv8_file))
cat(sprintf("  📊 %s\n\n", csv9_file))

cat("RESUMEN POSIBILIDAD:\n")
pinus_data <- totales_genero %>% filter(genero_grupo == "Pinus")
quercus_data <- totales_genero %>% filter(genero_grupo == "Quercus")

cat(sprintf("  Pinus:   %.2f m³ VTA (Posibilidad) + %.2f m³ (Infraestructura) = %.2f m³\n", 
            pinus_data$pos_total, pinus_data$vol_infra_total,
            pinus_data$pos_total + pinus_data$vol_infra_total))
cat(sprintf("  Quercus: %.2f m³ VTA (Posibilidad) + %.2f m³ (Infraestructura) = %.2f m³\n", 
            quercus_data$pos_total, quercus_data$vol_infra_total,
            quercus_data$pos_total + quercus_data$vol_infra_total))
cat(sprintf("  TOTAL:   %.2f m³ VTA (Posibilidad) + %.2f m³ (Infraestructura) = %.2f m³\n\n", 
            total_posibilidad, total_infraestructura, gran_total))