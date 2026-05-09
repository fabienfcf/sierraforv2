#!/usr/bin/env Rscript
# ==============================================================================
# ANÁLISIS DE REGENERACIÓN NATURAL SEGÚN NOM-152-SEMARNAT-2023
# Evaluación de criterios de éxito por UMM según memoria de cálculo oficial
# Sección 4.14 - Compromisos de reforestación
# ==============================================================================

# Configurar directorio
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5")

PROYECTO_ROOT <- getwd()
cat(sprintf("\n✓ Directorio base: %s\n", PROYECTO_ROOT))

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   ANÁLISIS REGENERACIÓN NATURAL (NOM-152)               ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 0. CONFIGURACIÓN Y CONSTANTES
# ==============================================================================

# Factor de expansión: cuadrante de 9 m² a plantas por hectárea
FACTOR_EXP <- 10000 / 9  # = 1,111.11 plantas/ha

# Criterios NOM-152-SEMARNAT-2023 (Sección 4.14.c)
CRITERIO_D_PINUS <- 1100     # plantas/ha
CRITERIO_D_TOTAL <- 1500     # plantas/ha  
CRITERIO_F <- 70             # porcentaje
CRITERIO_IQ <- 80            # porcentaje (no evaluado en inventario actual)
CRITERIO_DISTANCIA <- 3      # metros (no evaluado en inventario actual)

cat("[0/7] Configuración cargada\n")
cat(sprintf("  • Factor expansión: %.2f plantas/ha por planta en cuadrante 9m²\n", FACTOR_EXP))
cat(sprintf("  • Criterio D_Pinus: ≥ %d plantas/ha\n", CRITERIO_D_PINUS))
cat(sprintf("  • Criterio D_Total: ≥ %d plantas/ha\n", CRITERIO_D_TOTAL))
cat(sprintf("  • Criterio Frecuencia: ≥ %.0f%%\n\n", CRITERIO_F))

# ==============================================================================
# 1. CARGAR DATOS DE REGENERACIÓN OBSERVADA
# ==============================================================================

cat("[1/7] Cargando datos de regeneración observada...\n")

# Cargar datos desde CSV
regen_data <- read.csv("resultados/desc_09_regeneracion_observada.csv", 
                       stringsAsFactors = FALSE)

# Verificar columnas
cat("  • Columnas disponibles:\n")
cat(sprintf("    %s\n", paste(names(regen_data), collapse = ", ")))

# Resumen de datos cargados
cat(sprintf("\n  ✓ Cargados %d registros de regeneración\n", nrow(regen_data)))
cat(sprintf("  • UMMs presentes: %s\n", 
            paste(sort(unique(regen_data$UMM)), collapse = ", ")))
cat(sprintf("  • Géneros presentes: %s\n", 
            paste(sort(unique(regen_data$genero)), collapse = ", ")))

# ==============================================================================
# 2. CARGAR ESTADÍSTICAS DE UMM
# ==============================================================================

cat("\n[2/7] Cargando estadísticas de UMM...\n")

# Cargar UMM stats (número de puntos de muestreo)
umm_stats <- read.csv("UMM_stats.csv", stringsAsFactors = FALSE)

# Filtrar solo UMMs válidas (id != 0)
umm_stats <- umm_stats[umm_stats$id != 0, ]

cat(sprintf("  ✓ Cargados datos de %d UMM\n", nrow(umm_stats)))

# Tabla resumen
cat("\n  Puntos de muestreo por UMM:\n")
for (i in 1:nrow(umm_stats)) {
  cat(sprintf("    UMM %d: %d cuadrantes (%.2f ha)\n", 
              umm_stats$id[i], 
              umm_stats$num_puntos[i],
              umm_stats$SUPERFICIE[i]))
}

# ==============================================================================
# 3. CALCULAR DENSIDADES POR CUADRANTE
# ==============================================================================

cat("\n[3/7] Calculando densidades por cuadrante...\n")

# Primero: sumar individuos por cuadrante y género
individuos_por_cuadrante <- aggregate(
  num_individuos ~ muestreo + UMM + genero,
  data = regen_data,
  FUN = sum
)

# Pivotar para tener Pinus y Quercus como columnas
cuadrantes_pinus <- individuos_por_cuadrante[individuos_por_cuadrante$genero == "Pinus", ]
cuadrantes_quercus <- individuos_por_cuadrante[individuos_por_cuadrante$genero == "Quercus", ]

# Crear tabla completa de cuadrantes
cuadrantes_completa <- merge(
  data.frame(
    muestreo = cuadrantes_pinus$muestreo,
    UMM = cuadrantes_pinus$UMM,
    total_pinus = cuadrantes_pinus$num_individuos
  ),
  data.frame(
    muestreo = cuadrantes_quercus$muestreo,
    UMM = cuadrantes_quercus$UMM,
    total_quercus = cuadrantes_quercus$num_individuos
  ),
  by = c("muestreo", "UMM"),
  all = TRUE
)

# Rellenar NA con 0
cuadrantes_completa$total_pinus[is.na(cuadrantes_completa$total_pinus)] <- 0
cuadrantes_completa$total_quercus[is.na(cuadrantes_completa$total_quercus)] <- 0

# Total de individuos por cuadrante (todos los géneros)
total_por_cuadrante <- aggregate(
  num_individuos ~ muestreo + UMM,
  data = regen_data,
  FUN = sum
)

cuadrantes_completa <- merge(
  cuadrantes_completa,
  total_por_cuadrante,
  by = c("muestreo", "UMM"),
  all.x = TRUE
)
names(cuadrantes_completa)[names(cuadrantes_completa) == "num_individuos"] <- "total_individuos"

cat(sprintf("  ✓ Procesados %d cuadrantes con regeneración\n", nrow(cuadrantes_completa)))

# DIAGNÓSTICO
cat("\n  DIAGNÓSTICO - Distribución de cuadrantes:\n")
cuadrantes_por_umm <- table(cuadrantes_completa$UMM)
for (umm in names(cuadrantes_por_umm)) {
  cat(sprintf("    UMM %s: %d cuadrantes con regeneración\n", 
              umm, cuadrantes_por_umm[umm]))
}

# ==============================================================================
# 4. CALCULAR DENSIDADES Y FRECUENCIAS POR UMM
# ==============================================================================

cat("\n[4/7] Calculando densidades y frecuencias por UMM...\n")

# Inicializar lista de resultados
resultados_umm <- data.frame()

for (i in 1:nrow(umm_stats)) {
  umm_id <- umm_stats$id[i]
  n_puntos_total <- umm_stats$num_puntos[i]  # ← TOTAL de cuadrantes (incluye los sin regen)
  
  # Cuadrantes de esta UMM (solo los que tienen regeneración)
  cuadrantes_umm <- cuadrantes_completa[cuadrantes_completa$UMM == umm_id, ]
  
  # Suma de individuos por género
  suma_pinus <- sum(cuadrantes_umm$total_pinus, na.rm = TRUE)
  suma_quercus <- sum(cuadrantes_umm$total_quercus, na.rm = TRUE)
  suma_total <- sum(cuadrantes_umm$total_individuos, na.rm = TRUE)
  
  # Número de cuadrantes con presencia (del total de cuadrantes observados)
  n_cuadrantes_con_pinus <- sum(cuadrantes_umm$total_pinus > 0)
  n_cuadrantes_con_quercus <- sum(cuadrantes_umm$total_quercus > 0)
  n_cuadrantes_con_regen <- sum(cuadrantes_umm$total_individuos > 0)
  
  # MEMORIA DE CÁLCULO NOM-152
  # D_promedio = (suma_individuos / n_cuadrantes_TOTAL) * FACTOR_EXP
  # CRÍTICO: usar n_puntos_total (incluye cuadrantes con 0 regeneración)
  
  # Densidades (plantas/ha)
  D_pinus <- (suma_pinus / n_puntos_total) * FACTOR_EXP
  D_quercus <- (suma_quercus / n_puntos_total) * FACTOR_EXP
  D_total <- (suma_total / n_puntos_total) * FACTOR_EXP
  
  # Frecuencia (%)
  # F = (cuadrantes_con_presencia / total_cuadrantes) * 100
  # CRÍTICO: denominador es n_puntos_total
  F_pinus <- (n_cuadrantes_con_pinus / n_puntos_total) * 100
  F_quercus <- (n_cuadrantes_con_quercus / n_puntos_total) * 100
  F_total <- (n_cuadrantes_con_regen / n_puntos_total) * 100
  
  # Almacenar resultados
  resultados_umm <- rbind(resultados_umm, data.frame(
    UMM = umm_id,
    n_cuadrantes_total = n_puntos_total,
    n_cuadrantes_con_pinus = n_cuadrantes_con_pinus,
    n_cuadrantes_con_quercus = n_cuadrantes_con_quercus,
    n_cuadrantes_con_regen = n_cuadrantes_con_regen,
    suma_pinus = suma_pinus,
    suma_quercus = suma_quercus,
    suma_total = suma_total,
    D_pinus = D_pinus,
    D_quercus = D_quercus,
    D_total = D_total,
    F_pinus = F_pinus,
    F_quercus = F_quercus,
    F_total = F_total
  ))
}

cat(sprintf("  ✓ Densidades calculadas para %d UMM\n", nrow(resultados_umm)))

# DIAGNÓSTICO - Mostrar densidades
cat("\n  DIAGNÓSTICO - Densidades por UMM:\n")
for (i in 1:nrow(resultados_umm)) {
  cat(sprintf("    UMM %d: D_Pinus=%.0f, D_Quercus=%.0f, D_Total=%.0f plantas/ha\n",
              resultados_umm$UMM[i],
              resultados_umm$D_pinus[i],
              resultados_umm$D_quercus[i],
              resultados_umm$D_total[i]))
}

# ==============================================================================
# 5. EVALUAR CRITERIOS NOM-152
# ==============================================================================

cat("\n[5/7] Evaluando criterios NOM-152...\n")

# Evaluar cada criterio
resultados_umm$cumple_D_pinus <- resultados_umm$D_pinus >= CRITERIO_D_PINUS
resultados_umm$cumple_D_total <- resultados_umm$D_total >= CRITERIO_D_TOTAL
resultados_umm$cumple_F <- resultados_umm$F_total >= CRITERIO_F

# Criterios no evaluados
resultados_umm$cumple_IQ <- NA  # Requiere evaluación de vigor en campo
resultados_umm$cumple_distancia <- NA  # Requiere medición de distancia

# Decisión preliminar (solo con criterios disponibles)
resultados_umm$criterios_disponibles <- with(resultados_umm, 
                                             cumple_D_pinus & cumple_D_total & cumple_F
)

# Generar decisión detallada
resultados_umm$decision_preliminar <- apply(resultados_umm, 1, function(row) {
  cumple_D_pinus <- as.logical(row["cumple_D_pinus"])
  cumple_D_total <- as.logical(row["cumple_D_total"])
  cumple_F <- as.logical(row["cumple_F"])
  
  if (cumple_D_pinus && cumple_D_total && cumple_F) {
    return("EXITOSA")
  } else if (!cumple_D_pinus && !cumple_D_total) {
    return("FALLIDA")
  } else if (!cumple_D_pinus) {
    return("Falla Pinus")
  } else if (!cumple_D_total) {
    return("Falla total")
  } else if (!cumple_F) {
    return("Falla distrib.")
  } else {
    return("INDETERMINADA")
  }
})

cat("  ✓ Criterios evaluados\n")

# DIAGNÓSTICO - Resumen de cumplimiento
cat("\n  DIAGNÓSTICO - Cumplimiento de criterios:\n")
cat(sprintf("    D_Pinus ≥%d: %d/%d UMM cumplen\n", 
            CRITERIO_D_PINUS,
            sum(resultados_umm$cumple_D_pinus),
            nrow(resultados_umm)))
cat(sprintf("    D_Total ≥%d: %d/%d UMM cumplen\n",
            CRITERIO_D_TOTAL,
            sum(resultados_umm$cumple_D_total),
            nrow(resultados_umm)))
cat(sprintf("    F ≥%.0f%%: %d/%d UMM cumplen\n",
            CRITERIO_F,
            sum(resultados_umm$cumple_F),
            nrow(resultados_umm)))
cat(sprintf("    Todos los criterios: %d/%d UMM (%.1f%%)\n",
            sum(resultados_umm$criterios_disponibles),
            nrow(resultados_umm),
            100 * sum(resultados_umm$criterios_disponibles) / nrow(resultados_umm)))

# ==============================================================================
# 6. GENERAR TABLA LATEX
# ==============================================================================

cat("\n[6/7] Generando tabla LaTeX...\n")

# Función de formateo (3 cifras significativas)
fmt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("---")
  if (x == 0) return("0")
  
  # 3 cifras significativas
  x_sig <- signif(x, 3)
  
  # Formatear
  if (x_sig >= 100) {
    # Sin decimales para valores grandes
    return(format(round(x_sig), big.mark = "{,}", scientific = FALSE))
  } else {
    # Con decimales para valores pequeños
    return(format(x_sig, scientific = FALSE))
  }
}

# Iniciar tabla LaTeX
latex_table <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Evaluación de criterios de regeneración natural por UMM según NOM-152-SEMARNAT-2023. Valores calculados con cuadrantes de 9 m² y factor de expansión 1{,}111.11 plantas/ha. $\\checkmark$ = cumple criterio, $\\times$ = no cumple.}",
  "\t\\label{tab:evaluacion_criterios_regeneracion}",
  "\t\\small",
  "\t\\begin{tabular}{c@{\\hspace{8pt}}c@{\\hspace{8pt}}ccc@{\\hspace{8pt}}c@{\\hspace{8pt}}ccc@{\\hspace{8pt}}l}",
  "\t\t\\toprule",
  "\t\t\\textbf{UMM} & \\textbf{n} & \\multicolumn{3}{c}{\\textbf{Densidad (plantas/ha)}} & \\textbf{F} & \\multicolumn{3}{c}{\\textbf{Cumple criterio}} & \\textbf{Decisión} \\\\",
  "\t\t\\cmidrule(lr){3-5} \\cmidrule(lr){7-9}",
  "\t\t & \\textbf{cuad.} & \\textit{Pinus} & \\textit{Quercus} & \\textbf{Total} & (\\%) & $D_P \\geq 1{,}100$ & $D_T \\geq 1{,}500$ & $F \\geq 70$ & \\textbf{preliminar} \\\\",
  "\t\t\\midrule"
)

# Generar filas
for (i in 1:nrow(resultados_umm)) {
  row <- resultados_umm[i, ]
  
  # Símbolos de cumplimiento
  simbolo_pinus <- ifelse(row$cumple_D_pinus, "$\\checkmark$", "$\\times$")
  simbolo_total <- ifelse(row$cumple_D_total, "$\\checkmark$", "$\\times$")
  simbolo_F <- ifelse(row$cumple_F, "$\\checkmark$", "$\\times$")
  
  # Decisión corta para tabla
  decision_corta <- switch(row$decision_preliminar,
                           "EXITOSA" = "Exitosa*",
                           "FALLIDA" = "Fallida",
                           "Falla Pinus" = "Falla Pinus",
                           "Falla total" = "Falla total",
                           "Falla distrib." = "Falla distrib.",
                           "INDETERMINADA")
  
  latex_table <- c(latex_table,
                   sprintf("\t\t%d & %d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                           row$UMM,
                           row$n_cuadrantes_total,
                           fmt(row$D_pinus),
                           fmt(row$D_quercus),
                           fmt(row$D_total),
                           fmt(row$F_total),
                           simbolo_pinus,
                           simbolo_total,
                           simbolo_F,
                           decision_corta))
}

# Cerrar tabla
latex_table <- c(latex_table,
                 "\t\t\\bottomrule",
                 "\t\\end{tabular}",
                 "\t",
                 "\t\\vspace{0.3cm}",
                 "\t",
                 "\t\\raggedright",
                 "\t\\footnotesize",
                 "\t\\textbf{Criterios NOM-152 para regeneración exitosa:}",
                 "\t\\begin{itemize}[leftmargin=*,nosep]",
                 "\t\\item[(1)] $D_{Pinus} \\geq 1{,}100$ plantas/ha",
                 "\t\\item[(2)] $D_{total} \\geq 1{,}500$ plantas/ha",
                 "\t\\item[(3)] $F \\geq 70\\%$ (distribución espacial)",
                 "\t\\item[(4)] $IQ \\geq 80\\%$ (índice de calidad/vigor) --- \\textit{No evaluado}",
                 "\t\\item[(5)] Distancia máxima entre plantas $\\leq 3$ m --- \\textit{No evaluado}",
                 "\t\\end{itemize}",
                 "\t",
                 "\t\\textit{*Exitosa:} Cumple criterios (1), (2) y (3). Pendiente evaluar criterios (4) y (5) mediante inventario de regeneración post-aprovechamiento según protocolo §4.14 NOM-152.",
                 "\t",
                 "\t\\textit{n cuad.:} Número de cuadrantes de 9 m² muestreados en la UMM.",
                 "\t",
                 "\t\\textit{F:} Frecuencia = porcentaje de cuadrantes con presencia de regeneración.",
                 "\\end{table}")

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex_file <- "resultados/tablas/tabla_evaluacion_criterios_regeneracion.tex"
writeLines(latex_table, latex_file)

cat(sprintf("  ✓ Tabla LaTeX guardada: %s\n", latex_file))

# ==============================================================================
# 7. GENERAR RESUMEN ESTADÍSTICO Y ANÁLISIS
# ==============================================================================

cat("\n[7/7] Generando resumen y análisis...\n")

# CSV de resultados
csv_file <- "resultados/tablas/evaluacion_regeneracion_umm.csv"
write.csv(resultados_umm, csv_file, row.names = FALSE)
cat(sprintf("  ✓ CSV guardado: %s\n", csv_file))

# Análisis narrativo
analisis <- paste(
  "# ANÁLISIS DE REGENERACIÓN NATURAL - NOM-152-SEMARNAT-2023",
  "",
  "## Resumen ejecutivo",
  "",
  sprintf("Se evaluó la regeneración natural en %d UMMs utilizando la memoria de cálculo establecida en NOM-152 (Sección 4.14). Se muestrearon **%d cuadrantes de 9 m²** en total, con cobertura variable por UMM. Los resultados muestran **condiciones heterogéneas** de regeneración, con solo %d de %d UMMs (%.1f%%) cumpliendo los criterios mínimos de densidad y distribución espacial disponibles.",
          nrow(resultados_umm),
          sum(resultados_umm$n_cuadrantes_total),
          sum(resultados_umm$criterios_disponibles),
          nrow(resultados_umm),
          100 * sum(resultados_umm$criterios_disponibles) / nrow(resultados_umm)),
  "",
  "## Resultados por criterio",
  "",
  "### Criterio 1: Densidad de Pinus (≥1,100 plantas/ha)",
  "",
  sprintf("**UMMs que cumplen:** %s", 
          paste(resultados_umm$UMM[resultados_umm$cumple_D_pinus], collapse = ", ")),
  sprintf("**UMMs que NO cumplen:** %s",
          paste(resultados_umm$UMM[!resultados_umm$cumple_D_pinus], collapse = ", ")),
  "",
  "### Criterio 2: Densidad total (≥1,500 plantas/ha)",
  "",
  sprintf("**UMMs que cumplen:** %s",
          paste(resultados_umm$UMM[resultados_umm$cumple_D_total], collapse = ", ")),
  sprintf("**UMMs que NO cumplen:** %s",
          paste(resultados_umm$UMM[!resultados_umm$cumple_D_total], collapse = ", ")),
  "",
  "### Criterio 3: Distribución espacial (F ≥70%)",
  "",
  sprintf("**UMMs que cumplen:** %s",
          paste(resultados_umm$UMM[resultados_umm$cumple_F], collapse = ", ")),
  sprintf("**UMMs que NO cumplen:** %s",
          paste(resultados_umm$UMM[!resultados_umm$cumple_F], collapse = ", ")),
  "",
  "## Evaluación por UMM",
  "",
  sep = "\n"
)

# Agregar evaluación detallada por UMM
for (i in 1:nrow(resultados_umm)) {
  row <- resultados_umm[i, ]
  
  analisis <- paste(analisis,
                    sprintf("### UMM %d: %s", row$UMM, row$decision_preliminar),
                    sprintf("- %s D_Pinus: %.0f plantas/ha (criterio: ≥%d)",
                            ifelse(row$cumple_D_pinus, "✓", "✗"),
                            row$D_pinus,
                            CRITERIO_D_PINUS),
                    sprintf("- %s D_Total: %.0f plantas/ha (criterio: ≥%d)",
                            ifelse(row$cumple_D_total, "✓", "✗"),
                            row$D_total,
                            CRITERIO_D_TOTAL),
                    sprintf("- %s Frecuencia: %.1f%% (criterio: ≥%.0f%%)",
                            ifelse(row$cumple_F, "✓", "✗"),
                            row$F_total,
                            CRITERIO_F),
                    sprintf("- Cuadrantes muestreados: %d (con regeneración: %d)",
                            row$n_cuadrantes_total,
                            row$n_cuadrantes_con_regen),
                    "",
                    sep = "\n")
}

# Guardar análisis
analisis_file <- "resultados/tablas/analisis_regeneracion_narrativo.md"
writeLines(analisis, analisis_file)
cat(sprintf("  ✓ Análisis narrativo guardado: %s\n", analisis_file))

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║       ✓ ANÁLISIS DE REGENERACIÓN COMPLETADO             ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat(sprintf("  📄 %s\n", latex_file))
cat(sprintf("  📊 %s\n", csv_file))
cat(sprintf("  📝 %s\n\n", analisis_file))

cat("RESUMEN DE CUMPLIMIENTO:\n")
for (i in 1:nrow(resultados_umm)) {
  row <- resultados_umm[i, ]
  simbolo <- ifelse(row$criterios_disponibles, "✓", "✗")
  cat(sprintf("  %s UMM %d: %s (D_Pinus=%.0f, D_Total=%.0f, F=%.1f%%)\n",
              simbolo,
              row$UMM,
              row$decision_preliminar,
              row$D_pinus,
              row$D_total,
              row$F_total))
}

cat("\n")
cat(sprintf("TOTAL: %d/%d UMMs cumplen criterios disponibles (%.1f%%)\n",
            sum(resultados_umm$criterios_disponibles),
            nrow(resultados_umm),
            100 * sum(resultados_umm$criterios_disponibles) / nrow(resultados_umm)))
cat("\nNOTA: Los criterios 4 (IQ ≥80%) y 5 (distancia ≤3m) deben evaluarse\n")
cat("mediante inventario de regeneración post-aprovechamiento según §4.14 NOM-152.\n\n")