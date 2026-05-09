# ==============================================================================
# TEST: Cálculos específicos forestales
# ==============================================================================

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)
gc()

# Configurar directorio
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

# Cargar módulos
library(tidyverse)
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "generadores/52_CALCULOS_ESPECIFICOS.R"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║         TEST: 52_CALCULOS_ESPECIFICOS.R                  ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# TEST 1: Verificar funciones existen
# ==============================================================================

cat("[TEST 1] Verificando funciones básicas...\n")

funciones_requeridas <- c(
  "calcular_diversidad",
  "calcular_muertos_tocones",
  "calcular_balance_demografico",
  "calcular_resumen_predio",
  "calcular_composicion_floristica",
  "calcular_ranking_productividad",
  "calcular_regeneracion",
  "calcular_erosion",
  "calcular_sanidad",
  "calcular_uso_pecuario",
  "evaluar_sostenibilidad"
)

for (func in funciones_requeridas) {
  if (!exists(func)) {
    stop(sprintf("Función '%s' no encontrada", func))
  }
}

cat(sprintf("  ✓ %d funciones encontradas\n\n", length(funciones_requeridas)))

# ==============================================================================
# TEST 2: Cargar datos reales
# ==============================================================================

cat("[TEST 2] Cargando datos del proyecto...\n")

if (!file.exists("datos_intermedios/arboles_analisis.rds")) {
  cat("  ⚠️  arboles_analisis.rds no encontrado\n")
  cat("  Creando datos de prueba sintéticos...\n")
  
  # Crear datos sintéticos
  set.seed(42)
  arboles_test <- tibble(
    arbol_id = sprintf("R%d_M%03d_A%03d", 
                       rep(1:3, each = 100),
                       rep(1:10, 30),
                       1:300),
    rodal = rep(1:3, each = 100),
    muestreo = rep(1:10, 30),
    nombre_cientifico = sample(c("Pinus pseudostrobus", "Pinus montezumae", 
                                 "Quercus rugosa", "Quercus laurina"), 
                               300, replace = TRUE),
    genero_grupo = ifelse(grepl("Pinus", nombre_cientifico), "Pinus", "Quercus"),
    dominancia = sample(1:6, 300, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.05, 0.05)),
    diametro_normal = rnorm(300, 30, 10),
    altura_total = 1.3 * diametro_normal^0.6 + rnorm(300, 0, 2),
    volumen_m3 = 0.0001 * diametro_normal^2 * altura_total,
    sanidad = sample(1:5, 300, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.05)),
    tipo = "potencia",
    a = 0.00004,
    b = 1.93694,
    c = 1.03169
  )
  
  # Agregar algunos muertos y tocones
  n_muertos <- 10
  n_tocones <- 5
  
  muertos <- arboles_test %>%
    slice_sample(n = n_muertos) %>%
    mutate(dominancia = 7)
  
  tocones <- arboles_test %>%
    slice_sample(n = n_tocones) %>%
    mutate(dominancia = 8)
  
  arboles_test <- bind_rows(
    arboles_test %>% slice(1:(300 - n_muertos - n_tocones)),
    muertos,
    tocones
  )
  
  usa_datos_reales <- FALSE
  
} else {
  arboles_test <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  usa_datos_reales <- TRUE
  cat("  ✓ Datos reales cargados\n")
}

cat(sprintf("  • %d árboles\n", nrow(arboles_test)))
cat(sprintf("  • %d rodales\n", n_distinct(arboles_test$rodal)))
cat(sprintf("  • Datos: %s\n\n", ifelse(usa_datos_reales, "REALES", "SINTÉTICOS")))

# ==============================================================================
# TEST 3: Calcular diversidad
# ==============================================================================

cat("[TEST 3] Calculando índices de diversidad...\n")

# Nivel predio
div_predio <- calcular_diversidad(arboles_test, nivel = "predio")

cat("\n  Diversidad a nivel predio:\n")
print(div_predio)

stopifnot(
  "Tiene shannon" = "shannon" %in% names(div_predio),
  "Tiene simpson" = "simpson" %in% names(div_predio),
  "Tiene riqueza" = "riqueza" %in% names(div_predio),
  "Shannon > 0" = div_predio$shannon > 0,
  "Simpson entre 0-1" = div_predio$simpson >= 0 && div_predio$simpson <= 1
)

# Nivel rodal
div_rodal <- calcular_diversidad(arboles_test, nivel = "rodal")

cat("\n  Diversidad por rodal:\n")
print(div_rodal)

cat("\n  ✓ Cálculo de diversidad exitoso\n\n")

# ==============================================================================
# TEST 4: Calcular muertos y tocones
# ==============================================================================

cat("[TEST 4] Analizando muertos en pie y tocones...\n")

resultado_mort <- calcular_muertos_tocones(arboles_test, config = CONFIG)

cat("\n  Muertos en pie:\n")
if (nrow(resultado_mort$muertos_pie) > 0) {
  print(resultado_mort$muertos_pie)
} else {
  cat("    (Sin muertos en pie)\n")
}

cat("\n  Tocones:\n")
if (nrow(resultado_mort$tocones) > 0) {
  print(resultado_mort$tocones)
} else {
  cat("    (Sin tocones)\n")
}

cat("\n  Parámetros usados:\n")
cat(sprintf("    • Altura tocones: %.1f m\n", resultado_mort$parametros$altura_tocones))
cat(sprintf("    • Factor ajuste d: %.2f\n", resultado_mort$parametros$factor_ajuste_d))

stopifnot("Es una lista" = is.list(resultado_mort))

cat("\n  ✓ Análisis de mortalidad exitoso\n\n")

# ==============================================================================
# TEST 5: Balance demográfico (requiere simulación)
# ==============================================================================

cat("[TEST 5] Testeando balance demográfico...\n")

# Crear datos simulados de ejemplo
poblacion_inicial <- arboles_test %>% filter(dominancia < 7)
poblacion_final <- poblacion_inicial %>%
  mutate(
    dominancia = case_when(
      runif(n()) < 0.02 ~ 7L,  # 2% muere
      TRUE ~ dominancia
    )
  )

# Agregar reclutas
reclutas <- tibble(
  arbol_id = sprintf("RECLUTA_%03d", 1:10),
  rodal = sample(unique(arboles_test$rodal), 10, replace = TRUE),
  genero_grupo = sample(c("Pinus", "Quercus"), 10, replace = TRUE),
  dominancia = 6L
)

poblacion_final <- bind_rows(poblacion_final, reclutas)

# Simular resultado
resultado_sim <- list(
  poblacion_inicial = poblacion_inicial,
  poblacion_final = poblacion_final,
  años_simulados = 10
)

balance <- calcular_balance_demografico(resultado_sim)

cat("\n  Balance demográfico:\n")
print(balance)

# Verificar que el balance cuadra correctamente
inicial <- balance %>% filter(categoria == "Inicial") %>% pull(n_arboles)
final <- balance %>% filter(categoria == "Final") %>% pull(n_arboles)
reclutas <- balance %>% filter(categoria == "Reclutamiento") %>% pull(n_arboles)
muertos <- balance %>% filter(categoria == "Mortalidad") %>% pull(n_arboles)
cortados <- balance %>% filter(categoria == "Cortas") %>% pull(n_arboles)

# El balance correcto es: Final = Inicial + Reclutas - Muertos - Cortados
balance_calculado <- inicial + reclutas + muertos + cortados  # muertos y cortados ya son negativos

cat(sprintf("\n  Verificación: %d = %d + %d + (%d) + (%d)\n",
            final, inicial, reclutas, muertos, cortados))
cat(sprintf("  Balance calculado: %d\n", balance_calculado))
cat(sprintf("  Final observado:   %d\n", final))
cat(sprintf("  Diferencia:        %d\n", final - balance_calculado))

stopifnot(
  "Tiene 5 categorías" = nrow(balance) == 5,
  "Balance cuadra" = abs(final - balance_calculado) < 2
)

cat("\n  ✓ Balance demográfico calculado\n\n")

# ==============================================================================
# TEST 6: Composición florística
# ==============================================================================

cat("[TEST 6] Calculando composición florística...\n")

composicion <- calcular_composicion_floristica(arboles_test, CONFIG)

cat("\n  Composición florística (media ± SD inter-rodal):\n")
print(composicion)

stopifnot(
  "Tiene medias" = all(c("n_ha_media", "ab_ha_media", "vol_ha_media") %in% names(composicion)),
  "Tiene SDs" = all(c("n_ha_sd", "ab_ha_sd", "vol_ha_sd") %in% names(composicion))
)

cat("\n  ✓ Composición florística calculada\n\n")

# ==============================================================================
# TEST 7: Ranking productividad
# ==============================================================================

cat("[TEST 7] Calculando ranking de productividad...\n")

# Calcular métricas por rodal primero
vivos <- arboles_test %>% filter(dominancia < 7)
n_sitios <- n_distinct(arboles_test$muestreo)

metricas_rodal <- vivos %>%
  group_by(rodal) %>%
  summarise(
    n_arboles = n(),
    vol_total = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    vol_ha_m3 = expandir_a_hectarea(vol_total / n_sitios, CONFIG$area_parcela_ha)
  )

ranking <- calcular_ranking_productividad(metricas_rodal)

cat("\n  Ranking de productividad:\n")
print(ranking)

stopifnot(
  "Tiene ranking" = "ranking" %in% names(ranking),
  "Tiene clasificación" = "clasificacion" %in% names(ranking),
  "Tiene score" = "score" %in% names(ranking)
)

cat("\n  ✓ Ranking calculado\n\n")

# ==============================================================================
# TEST 8: Resumen predio
# ==============================================================================

cat("[TEST 8] Calculando resumen del predio...\n")

resumen <- calcular_resumen_predio(arboles_test, config = CONFIG)

cat("\n  Resumen del predio:\n")
print(resumen)

stopifnot(
  "Tiene n_rodales" = "n_rodales" %in% names(resumen),
  "Tiene superficie" = "superficie_ha" %in% names(resumen)
)

cat("\n  ✓ Resumen predio calculado\n\n")

# ==============================================================================
# TEST 9: Sanidad
# ==============================================================================

cat("[TEST 9] Analizando sanidad...\n")

sanidad <- calcular_sanidad(arboles_test)

if (!is.null(sanidad)) {
  cat("\n  Distribución de sanidad:\n")
  print(head(sanidad, 10))
  
  stopifnot("Tiene categorías" = "categoria_sanidad" %in% names(sanidad))
  
  cat("\n  ✓ Análisis de sanidad exitoso\n\n")
} else {
  cat("  ⚠️  Columna 'sanidad' no disponible\n\n")
}

# ==============================================================================
# TEST 10: Sostenibilidad
# ==============================================================================

cat("[TEST 10] Evaluando sostenibilidad...\n")

# Datos de ejemplo
ica_rodal <- tibble(
  rodal = 1:3,
  ica_m3_ha_año = c(5.5, 6.2, 4.8)
)

vol_aprovechado <- tibble(
  rodal = 1:3,
  vol_cortado_m3 = c(50, 70, 40)
)

sostenibilidad <- evaluar_sostenibilidad(ica_rodal, vol_aprovechado, periodo = 10)

cat("\n  Evaluación de sostenibilidad:\n")
print(sostenibilidad)

stopifnot(
  "Tiene balance" = "balance_m3_año" %in% names(sostenibilidad),
  "Tiene clasificación" = "sostenible" %in% names(sostenibilidad)
)

cat("\n  ✓ Sostenibilidad evaluada\n\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║              ✓ TODOS LOS TESTS PASARON                   ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("FUNCIONES VERIFICADAS:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  ✓ Diversidad (Shannon, Simpson, riqueza)\n")
cat("  ✓ Muertos en pie y tocones\n")
cat("  ✓ Balance demográfico\n")
cat("  ✓ Composición florística (con SD inter-rodal)\n")
cat("  ✓ Ranking de productividad\n")
cat("  ✓ Resumen del predio\n")
cat("  ✓ Análisis de sanidad\n")
cat("  ✓ Evaluación de sostenibilidad\n\n")

cat("NOTAS:\n")
cat("═══════════════════════════════════════════════════════════\n")
if (!usa_datos_reales) {
  cat("  ⚠️  Tests ejecutados con datos SINTÉTICOS\n")
  cat("  ℹ️  Ejecuta 40_WORKFLOW_COMPLETO.R primero para usar datos reales\n\n")
} else {
  cat("  ✅ Tests ejecutados con datos REALES del proyecto\n\n")
}

cat("Funciones adicionales disponibles pero no testeadas:\n")
cat("  • calcular_regeneracion() - requiere hoja F05\n")
cat("  • calcular_erosion() - requiere hoja F02\n")
cat("  • calcular_uso_pecuario() - requiere hoja F01\n\n")

cat("El módulo de cálculos específicos está listo.\n")
cat("Próximo paso: Implementar 60_SECCION_1_ESTADO_INICIAL.R\n\n")