# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# EJECUTAR GENERACIÓN DE TABLA DESCRIPTIVA DE SITIOS CON VOL/HA Y AB/HA
# ==============================================================================

library(tidyverse)
library(kableExtra)

# Establecer directorio de trabajo
setwd("~/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/")

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║     TABLA DESCRIPTIVA DE SITIOS CON MÉTRICAS             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 1. CARGAR CONFIGURACIÓN
# ==============================================================================

cat("[1/4] Cargando configuración...\n")
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
cat("  ✓ CONFIG cargado\n")

# ==============================================================================
# 2. IMPORTAR INVENTARIO
# ==============================================================================

cat("\n[2/4] Importando inventario...\n")
source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))

inventario <- importar_inventario_completo(
  ruta_archivo = "inventario_forestal.xlsx",
  ruta_umm = "UMM_stats.csv"
)

# ==============================================================================
# 3. CONSTRUIR DATASET DE ÁRBOLES CON CÁLCULOS
# ==============================================================================

cat("\n[3/4] Construyendo dataset de árboles...\n")
arboles_analisis <- construir_arboles_analisis(inventario, CONFIG)

cat(sprintf("  ✓ Árboles procesados: %d\n", nrow(arboles_analisis)))
cat(sprintf("  ✓ Árboles vivos: %d\n", 
            sum(es_arbol_vivo(arboles_analisis$dominancia))))

# ==============================================================================
# 4. GENERAR TABLA CON MÉTRICAS
# ==============================================================================

cat("\n[4/4] Generando tabla con métricas dasométricas...\n")
source("modelov5/50_crear_tabla_descriptiva_muestreo.r")

tabla_latex <- crear_tabla_distribucion_sitios(
  inventario = inventario,
  arboles_analisis = arboles_analisis,
  config = CONFIG
)

# La función guarda automáticamente los datos raw en DATOS_SITIOS_RAW

# ==============================================================================
# 5. VISUALIZAR Y GUARDAR
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                    RESULTADO FINAL                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

print(tabla_latex)

# Guardar LaTeX
guardar_tabla_distribucion(
  tabla_latex, 
  "tablas_latex/tabla_distribucion_sitios_extendida.tex"
)

# Guardar CSV (usando los datos guardados en global)
exportar_csv_sitios(
  DATOS_SITIOS_RAW,
  "resultados/tabla_sitios_metricas.csv"
)

cat("\n✅ PROCESO COMPLETADO EXITOSAMENTE\n")
cat("   📊 Tabla con información física + dasométrica\n")
cat("   📍 Columnas: UMM, Superficie, Sitio, Coords, Elevación,\n")
cat("                Pendiente, Exposición, AB/ha, Vol/ha\n")
cat("   🌲 Solo Pinus + Quercus vivos\n")
cat("   📄 LaTeX: tablas_latex/tabla_distribucion_sitios_extendida.tex\n")
cat("   📄 CSV:   resultados/tabla_sitios_metricas.csv\n\n")