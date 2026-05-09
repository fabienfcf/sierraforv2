# ==============================================================================
# TABLA ANEXA - DISTRIBUCIÓN DIAMÉTRICA DE CORTAS POR UMM
# Genera LaTeX para tabla anexa del PMF
# ==============================================================================

library(tidyverse)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA ANEXA - DISTRIBUCIÓN DIAMÉTRICA CORTAS          ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 1. CARGAR DATOS
# ==============================================================================

cat("[1/3] Cargando datos de distribución diamétrica...\n")

# Cargar distribución diamétrica de cortas
cortas_dist <- read_csv("resultados/cortas_distribucion_diametrica.csv", 
                        show_col_types = FALSE) %>%
  filter(rodal_cortado != 0) %>%  # Excluir UMM 0 si existe
  select(rodal_cortado, ano_corta, genero_grupo, clase_d,
         n_arboles_por_ha, n_arboles_umm, vol_m3_ha, vol_total_umm_m3) %>%
  # Redondear n_arboles_umm al entero inferior
  mutate(n_arboles_umm = floor(n_arboles_umm))

cat(sprintf("  ✓ %d registros cargados\n", nrow(cortas_dist)))
cat(sprintf("  ✓ UMMs: %s\n", 
            paste(sort(unique(cortas_dist$rodal_cortado)), collapse = ", ")))
cat(sprintf("  ✓ Clases diamétricas: %d\n", 
            n_distinct(cortas_dist$clase_d)))

# ==============================================================================
# 2. PREPARAR DATOS PARA TABLA
# ==============================================================================

cat("\n[2/3] Preparando estructura de tabla...\n")

# Obtener todas las combinaciones posibles
umms <- sort(unique(cortas_dist$rodal_cortado))
clases_d <- sort(unique(cortas_dist$clase_d))

# Pivot wider para tener Pinus y Quercus como columnas
tabla_data <- cortas_dist %>%
  # Crear columnas combinadas para cada género
  mutate(
    # Columna principal: n_arboles_umm
    valor_n = sprintf("%d", n_arboles_umm),
    # Columna secundaria: vol_total_umm_m3
    valor_vol = sprintf("%.2f", vol_total_umm_m3)
  ) %>%
  select(rodal_cortado, ano_corta, clase_d, genero_grupo, 
         n_arboles_umm, vol_total_umm_m3) %>%
  # Pivot para tener Pinus y Quercus separados
  pivot_wider(
    names_from = genero_grupo,
    values_from = c(n_arboles_umm, vol_total_umm_m3),
    values_fill = list(n_arboles_umm = NA, vol_total_umm_m3 = NA)
  ) %>%
  arrange(rodal_cortado, ano_corta, clase_d)

cat(sprintf("  ✓ Tabla preparada: %d filas\n", nrow(tabla_data)))

# ==============================================================================
# 3. FUNCIÓN FORMATEAR
# ==============================================================================

# Función para formatear números (4 cifras sig para >= 1000, 3 para < 1000)
fmt <- function(x) {
  if (is.null(x) || length(x) == 0) return("---")
  if (length(x) > 1) x <- x[1]
  if (is.na(x)) return("---")
  if (x == 0) return("0")
  
  # 4 cifras para >= 1000, 3 para < 1000
  n_sig <- ifelse(abs(x) >= 1000, 4, 3)
  x_sig <- signif(x, n_sig)
  
  # Formatear
  resultado <- format(x_sig, scientific = FALSE, big.mark = ",", trim = TRUE)
  
  # Si es entero, quitar decimales
  if (x_sig >= 100 && x_sig == floor(x_sig)) {
    resultado <- format(floor(x_sig), big.mark = ",", trim = TRUE)
  }
  
  return(resultado)
}

# Función para formatear enteros (n_arboles)
fmt_int <- function(x) {
  if (is.null(x) || length(x) == 0) return("---")
  if (length(x) > 1) x <- x[1]
  if (is.na(x)) return("---")
  
  format(as.integer(x), big.mark = ",", trim = TRUE)
}

# ==============================================================================
# 4. GENERAR TABLA LATEX
# ==============================================================================

cat("\n[3/3] Generando tabla LaTeX...\n")

latex_lines <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Distribución diamétrica de cortas por unidad mínima de manejo y género}",
  "\t\\label{tab:distribucion_diametrica_cortas}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{8pt}}c@{\\hspace{4pt}}c@{\\hspace{8pt}}c@{\\hspace{4pt}}c}",
  "\t\t\\toprule",
  "\t\t\\textbf{UMM} & \\textbf{Año de} & \\textbf{Clase} & \\multicolumn{2}{c}{\\textbf{\\textit{Quercus}}} & \\multicolumn{2}{c}{\\textbf{\\textit{Pinus}}} \\\\",
  "\t\t\\textbf{No.} & \\textbf{Corta} & \\textbf{Diamétrica (cm)} & \\textbf{N árb.} & \\textbf{Vol. (m$^3$)} & \\textbf{N árb.} & \\textbf{Vol. (m$^3$)} \\\\",
  "\t\t\\midrule"
)

# Iterar por UMM
for (umm in umms) {
  # Datos de este UMM
  datos_umm <- tabla_data %>%
    filter(rodal_cortado == umm) %>%
    arrange(clase_d)
  
  if (nrow(datos_umm) == 0) next
  
  ano_corta <- first(datos_umm$ano_corta)
  
  # Iterar por clase diamétrica
  for (i in 1:nrow(datos_umm)) {
    d <- datos_umm[i, ]
    
    # Formatear valores
    umm_str <- ifelse(i == 1, as.character(umm), "")
    ano_str <- ifelse(i == 1, sprintf("%d", INICIO_SIMULACION + ano_corta - 1), "")
    
    # Quercus
    n_quercus <- fmt_int(d$n_arboles_umm_Quercus)
    vol_quercus <- fmt(d$vol_total_umm_m3_Quercus)
    
    # Pinus
    n_pinus <- fmt_int(d$n_arboles_umm_Pinus)
    vol_pinus <- fmt(d$vol_total_umm_m3_Pinus)
    
    # Agregar línea
    latex_lines <- c(latex_lines,
                     sprintf("\t\t%s & %s & %s & %s & %s & %s & %s \\\\",
                             umm_str,
                             ano_str,
                             d$clase_d,
                             n_quercus,
                             vol_quercus,
                             n_pinus,
                             vol_pinus)
    )
  }
  
  # Calcular subtotal por UMM
  subtotal_n_quercus <- sum(datos_umm$n_arboles_umm_Quercus, na.rm = TRUE)
  subtotal_vol_quercus <- sum(datos_umm$vol_total_umm_m3_Quercus, na.rm = TRUE)
  subtotal_n_pinus <- sum(datos_umm$n_arboles_umm_Pinus, na.rm = TRUE)
  subtotal_vol_pinus <- sum(datos_umm$vol_total_umm_m3_Pinus, na.rm = TRUE)
  
  # Línea subtotal (grisée)
  latex_lines <- c(latex_lines,
                   sprintf("\t\t\\rowcolor{gray!15} \\multicolumn{3}{l}{\\textbf{Subtotal UMM %d}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                           umm,
                           fmt_int(subtotal_n_quercus),
                           fmt(subtotal_vol_quercus),
                           fmt_int(subtotal_n_pinus),
                           fmt(subtotal_vol_pinus))
  )
  
  # Separador entre UMMs
  if (umm != max(umms)) {
    latex_lines <- c(latex_lines, "\t\t\\midrule")
  }
}

# TOTAL GENERAL
latex_lines <- c(latex_lines, "\t\t\\midrule")

total_n_quercus <- sum(tabla_data$n_arboles_umm_Quercus, na.rm = TRUE)
total_vol_quercus <- sum(tabla_data$vol_total_umm_m3_Quercus, na.rm = TRUE)
total_n_pinus <- sum(tabla_data$n_arboles_umm_Pinus, na.rm = TRUE)
total_vol_pinus <- sum(tabla_data$vol_total_umm_m3_Pinus, na.rm = TRUE)

latex_lines <- c(latex_lines,
                 sprintf("\t\t\\rowcolor{gray!35} \\multicolumn{3}{l}{\\textbf{TOTAL PREDIO}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                         fmt_int(total_n_quercus),
                         fmt(total_vol_quercus),
                         fmt_int(total_n_pinus),
                         fmt(total_vol_pinus))
)

# Cerrar tabla
latex_lines <- c(latex_lines,
                 "\t\t\\bottomrule",
                 "\t\\end{tabular}",
                 "\t\\\\[0.3cm]",
                 "\t{\\scriptsize N árb.: Número de árboles a cortar en la UMM}",
                 "\t\\\\",
                 "\t{\\scriptsize Vol.: Volumen total a extraer en metros cúbicos (m$^3$ VTA)}",
                 "\\end{table}"
)

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex_file <- "resultados/tablas/tabla_anexa_distribucion_diametrica.tex"
writeLines(latex_lines, latex_file)
writeLines(latex_lines, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/anexos/tabla_anexa_distribucion_diametrica.tex")

# Mostrar tabla
cat("\n═══════════════════════════════════════════════════════════\n")
cat("TABLA GENERADA:\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat(paste(latex_lines, collapse = "\n"))
cat("\n\n")

# ==============================================================================
# GUARDAR CSV
# ==============================================================================

csv_file <- "resultados/tablas/tabla_anexa_distribucion_diametrica.csv"
write_csv(tabla_data, csv_file)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║       ✓ TABLA ANEXA GENERADA                            ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat(sprintf("  📄 LaTeX: %s\n", latex_file))
cat(sprintf("  📊 CSV:   %s\n\n", csv_file))

cat("RESUMEN CORTAS:\n")
cat(sprintf("  Quercus: %s árboles, %.2f m³ VTA\n", 
            fmt_int(total_n_quercus), total_vol_quercus))
cat(sprintf("  Pinus:   %s árboles, %.2f m³ VTA\n", 
            fmt_int(total_n_pinus), total_vol_pinus))
cat(sprintf("  TOTAL:   %s árboles, %.2f m³ VTA\n\n", 
            fmt_int(total_n_quercus + total_n_pinus), 
            total_vol_quercus + total_vol_pinus))

cat("DESGLOSE POR UMM:\n")
for (umm in umms) {
  datos_umm <- tabla_data %>% filter(rodal_cortado == umm)
  n_q <- sum(datos_umm$n_arboles_umm_Quercus, na.rm = TRUE)
  v_q <- sum(datos_umm$vol_total_umm_m3_Quercus, na.rm = TRUE)
  n_p <- sum(datos_umm$n_arboles_umm_Pinus, na.rm = TRUE)
  v_p <- sum(datos_umm$vol_total_umm_m3_Pinus, na.rm = TRUE)
  
  cat(sprintf("  UMM %d: %s árb. (Q:%s + P:%s), %.2f m³ (Q:%.2f + P:%.2f)\n",
              umm,
              fmt_int(n_q + n_p), fmt_int(n_q), fmt_int(n_p),
              v_q + v_p, v_q, v_p))
}
cat("\n")