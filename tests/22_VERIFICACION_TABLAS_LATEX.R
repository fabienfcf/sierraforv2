# ==============================================================================
# VERIFICACIÓN Y RESUMEN DE TABLAS LATEX GENERADAS
# ==============================================================================

setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

library(tidyverse)

ruta_latex <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/tablas_latex"

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║       VERIFICACIÓN DE TABLAS LATEX                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# LISTAR ARCHIVOS GENERADOS
# ==============================================================================

archivos_tex <- list.files(ruta_latex, pattern = "\\.tex$", full.names = FALSE)

if (length(archivos_tex) == 0) {
  cat("⚠ No se encontraron archivos .tex en la carpeta tablas_latex/\n")
  cat("   Ejecuta primero el script de simulación.\n\n")
  stop("No hay archivos para verificar")
}

cat(sprintf("Se encontraron %d archivos LaTeX:\n\n", length(archivos_tex)))

# Clasificar archivos
archivos_core <- archivos_tex[grepl("^0[1-4]_", archivos_tex)]
archivos_rodal <- archivos_tex[grepl("^05_corta_rodal", archivos_tex)]

cat("TABLAS PRINCIPALES:\n")
for (f in archivos_core) {
  tamaño <- file.size(file.path(ruta_latex, f))
  cat(sprintf("  ✓ %s (%d bytes)\n", f, tamaño))
}

cat("\nTABLAS POR RODAL:\n")
for (f in archivos_rodal) {
  tamaño <- file.size(file.path(ruta_latex, f))
  cat(sprintf("  ✓ %s (%d bytes)\n", f, tamaño))
}

# ==============================================================================
# GENERAR DOCUMENTO MAESTRO LATEX
# ==============================================================================

cat("\n[*] Generando documento maestro LaTeX...\n")

latex_maestro <- c(
  "% ==============================================================================",
  "% DOCUMENTO MAESTRO - TABLAS PMF",
  "% Generado automáticamente",
  "% ==============================================================================",
  "",
  "\\documentclass[12pt,a4paper]{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[spanish]{babel}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{geometry}",
  "\\geometry{margin=2.5cm}",
  "\\usepackage{float}",
  "",
  "\\title{Programa de Manejo Forestal\\\\",
  "Ejido Las Alazanas\\\\",
  "Tablas de Simulación 10 Años}",
  "\\author{Elaborado conforme a la NOM-152-SEMARNAT-2023}",
  "\\date{\\today}",
  "",
  "\\begin{document}",
  "",
  "\\maketitle",
  "\\tableofcontents",
  "\\newpage",
  "",
  "% ==============================================================================",
  "% SECCIÓN 1: INVENTARIO INICIAL",
  "% ==============================================================================",
  "",
  "\\section{Inventario Inicial}",
  "",
  "La Tabla~\\ref{tab:inventario_inicial} presenta el estado inicial del inventario forestal,",
  "desagregado por rodal y género.",
  "",
  "\\input{01_inventario_inicial.tex}",
  "",
  "% ==============================================================================",
  "% SECCIÓN 2: PROYECCIÓN 10 AÑOS",
  "% ==============================================================================",
  "",
  "\\section{Proyección a 10 Años}",
  "",
  "La Tabla~\\ref{tab:comparacion_10años} muestra la comparación entre el estado inicial",
  "y el estado proyectado después de 10 años de crecimiento, mortalidad, reclutamiento",
  "y aplicación de cortas programadas.",
  "",
  "\\input{02_comparacion_inicial_final.tex}",
  "",
  "% ==============================================================================",
  "% SECCIÓN 3: INTENSIDAD DE CORTE",
  "% ==============================================================================",
  "",
  "\\section{Programa de Aprovechamiento}",
  "",
  "\\subsection{Resumen de Intensidad de Corte}",
  "",
  "La Tabla~\\ref{tab:intensidad_corte} presenta la intensidad de corte por rodal y género.",
  "",
  "\\input{03_intensidad_corte_rodal.tex}",
  "",
  "\\subsection{Detalle por Clase Diamétrica}",
  "",
  "La Tabla~\\ref{tab:corte_detalle} desglosa el programa de cortas por género",
  "y clase diamétrica.",
  "",
  "\\input{04_corte_por_clase_diametrica.tex}",
  "",
  "% ==============================================================================",
  "% SECCIÓN 4: TABLAS POR RODAL",
  "% ==============================================================================",
  "",
  "\\section{Programa de Cortas por Rodal}",
  "",
  "A continuación se presentan las tablas detalladas de corta para cada rodal,",
  "especificando el año de intervención y la distribución por género y clase diamétrica.",
  ""
)

# Agregar tablas de rodales
if (length(archivos_rodal) > 0) {
  rodales <- str_extract(archivos_rodal, "\\d+") %>% as.numeric() %>% unique() %>% sort()
  
  for (rodal_id in rodales) {
    archivo <- sprintf("05_corta_rodal_%02d.tex", rodal_id)
    if (archivo %in% archivos_rodal) {
      latex_maestro <- c(
        latex_maestro,
        sprintf("\\subsection{Rodal %d}", rodal_id),
        "",
        sprintf("\\input{%s}", archivo),
        ""
      )
    }
  }
}

# Cerrar documento
latex_maestro <- c(
  latex_maestro,
  "",
  "\\end{document}"
)

# Escribir documento maestro
writeLines(latex_maestro, file.path(ruta_latex, "00_documento_maestro.tex"))

cat("  ✓ Generado: 00_documento_maestro.tex\n")

# ==============================================================================
# GENERAR ARCHIVO README
# ==============================================================================

readme <- c(
  "# TABLAS LATEX - PROGRAMA DE MANEJO FORESTAL",
  "",
  "## Descripción",
  "",
  "Este directorio contiene las tablas generadas automáticamente para el",
  "Programa de Manejo Forestal del Ejido Las Alazanas, conforme a la",
  "NOM-152-SEMARNAT-2023.",
  "",
  "## Archivos principales",
  "",
  "### Tablas principales",
  "",
  "1. `01_inventario_inicial.tex` - Estado inicial del inventario por rodal y género",
  "2. `02_comparacion_inicial_final.tex` - Comparación estado inicial vs proyección 10 años",
  "3. `03_intensidad_corte_rodal.tex` - Resumen de intensidad de corte",
  "4. `04_corte_por_clase_diametrica.tex` - Detalle de corta por clase diamétrica",
  "",
  "### Tablas por rodal",
  "",
  paste0("- `05_corta_rodal_XX.tex` - Programa de corta específico para cada rodal (", 
         length(archivos_rodal), " rodales)"),
  "",
  "## Uso",
  "",
  "### Opción 1: Documento completo",
  "",
  "Compilar el documento maestro que incluye todas las tablas:",
  "",
  "```bash",
  "pdflatex 00_documento_maestro.tex",
  "```",
  "",
  "### Opción 2: Incluir tablas individuales",
  "",
  "En tu documento LaTeX principal:",
  "",
  "```latex",
  "\\input{tablas_latex/01_inventario_inicial.tex}",
  "```",
  "",
  "## Requisitos LaTeX",
  "",
  "- Paquete `booktabs` para tablas profesionales",
  "- Paquete `longtable` para tablas que ocupan múltiples páginas",
  "- Paquete `babel` con opción `spanish` para formato en español",
  "",
  "## Notas",
  "",
  paste0("- Archivos generados: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- Total de archivos: ", length(archivos_tex)),
  paste0("- Tablas principales: ", length(archivos_core)),
  paste0("- Tablas por rodal: ", length(archivos_rodal)),
  "",
  "## Regeneración",
  "",
  "Para regenerar las tablas, ejecutar:",
  "",
  "```r",
  "source('simulaciones/30_SIMULACION_10AÑOS_COMPLETA.R')",
  "```",
  ""
)

writeLines(readme, file.path(ruta_latex, "README.md"))

cat("  ✓ Generado: README.md\n")

# ==============================================================================
# VERIFICAR CONTENIDO DE LAS TABLAS
# ==============================================================================

cat("\n[*] Verificando contenido de las tablas...\n\n")

# Función para contar filas en tabla LaTeX
contar_filas_tabla <- function(archivo) {
  lineas <- readLines(file.path(ruta_latex, archivo))
  # Buscar líneas con datos (que tengan &)
  lineas_datos <- grep("&", lineas, value = FALSE)
  # Excluir líneas de encabezado
  lineas_datos <- lineas_datos[!grepl("\\\\hline", lineas[lineas_datos])]
  return(length(lineas_datos))
}

cat("CONTENIDO DE TABLAS:\n\n")

for (archivo in archivos_core) {
  n_filas <- contar_filas_tabla(archivo)
  cat(sprintf("  %s: ~%d filas de datos\n", archivo, n_filas))
}

if (length(archivos_rodal) > 0) {
  cat("\n")
  for (archivo in archivos_rodal) {
    n_filas <- contar_filas_tabla(archivo)
    cat(sprintf("  %s: ~%d filas de datos\n", archivo, n_filas))
  }
}

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              VERIFICACIÓN COMPLETADA                      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("RESUMEN:\n")
cat(sprintf("  ✓ Total archivos .tex:     %d\n", length(archivos_tex)))
cat(sprintf("  ✓ Tablas principales:      %d\n", length(archivos_core)))
cat(sprintf("  ✓ Tablas por rodal:        %d\n", length(archivos_rodal)))
cat(sprintf("  ✓ Documento maestro:       1\n"))
cat(sprintf("  ✓ README:                  1\n\n"))

cat("SIGUIENTE PASO:\n")
cat("  1. Revisar las tablas .tex generadas\n")
cat("  2. Compilar 00_documento_maestro.tex para generar PDF completo\n")
cat("  3. O incluir tablas individuales en tu PMF usando \\input{}\n\n")

cat("UBICACIÓN:\n")
cat(sprintf("  %s/\n\n", ruta_latex))

cat("Para compilar el documento completo:\n")
cat("  cd tablas_latex\n")
cat("  pdflatex 00_documento_maestro.tex\n\n")