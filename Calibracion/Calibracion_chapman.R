# ==============================================================================
# 60_RECALIBRAR_CHAPMAN_RICHARDS.R
# Recalibración Chapman-Richards: UNA CURVA POR ESPECIE
# ==============================================================================

library(tidyverse)
library(minpack.lm)

rm(list = ls())
gc()

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║     RECALIBRACIÓN CHAPMAN-RICHARDS CON INVENTARIO         ║\n")
cat("║              UNA CURVA POR ESPECIE                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("[1/4] Cargando sistema...\n")
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))

cat("[2/4] Importando inventario...\n")
inventario <- importar_inventario_completo(
  ruta_archivo = "inventario_forestal.xlsx",
  ruta_umm = "UMM_stats.csv"
)

arboles_analisis <- construir_arboles_analisis(inventario, CONFIG)
arboles_vivos <- filtrar_arboles_vivos(arboles_analisis)

# Preparar datos para calibración
arboles_calibracion <- arboles_vivos %>%
  filter(!is.na(diametro_normal), !is.na(altura_total)) %>%
  filter(diametro_normal > 0, altura_total > 0) %>%
  filter(genero_grupo %in% c("Quercus", "Pinus"))

cat(sprintf("  ✓ Árboles para calibración: %d\n", nrow(arboles_calibracion)))

# Resumen
resumen <- arboles_calibracion %>%
  count(genero_grupo, nombre_cientifico) %>%
  arrange(genero_grupo, nombre_cientifico)

cat("\n  Especies disponibles:\n")
print(resumen, n = Inf)

# ==============================================================================
# FUNCIÓN DE CALIBRACIÓN
# ==============================================================================

calibrar_chapman_richards <- function(d, h, nombre_especie) {
  
  cat(sprintf("\n  Calibrando: %s\n", nombre_especie))
  
  # Limpiar NAs
  valid_idx <- !is.na(d) & !is.na(h) & d > 0 & h > 0
  d <- d[valid_idx]
  h <- h[valid_idx]
  
  cat(sprintf("    n = %d observaciones\n", length(d)))
  
  if (length(d) < 15) {
    cat(sprintf("    ✗ Insuficientes datos (mínimo 15)\n"))
    return(NULL)
  }
  
  cat(sprintf("    d: [%.1f - %.1f] cm\n", min(d), max(d)))
  cat(sprintf("    h: [%.1f - %.1f] m\n", min(h), max(h)))
  
  # Ajuste
  tryCatch({
    
    modelo <- nlsLM(
      h ~ a * (1 - exp(-b * d))^c,
      data = data.frame(d = d, h = h),
      start = list(a = max(h) * 1.2, b = 0.02, c = 0.7),
      control = nls.lm.control(maxiter = 500)
    )
    
    params <- coef(modelo)
    h_pred <- predict(modelo)
    r2 <- 1 - sum((h - h_pred)^2) / sum((h - mean(h))^2)
    rmse <- sqrt(mean((h - h_pred)^2))
    mae <- mean(abs(h - h_pred))
    
    cat(sprintf("    ✓ a = %.3f, b = %.5f, c = %.4f\n", params["a"], params["b"], params["c"]))
    cat(sprintf("    ✓ R² = %.4f, RMSE = %.3f m\n", r2, rmse))
    
    return(list(
      a = params["a"], b = params["b"], c = params["c"],
      r2 = r2, rmse = rmse, mae = mae, n = length(d),
      modelo = modelo, datos = data.frame(d = d, h = h)
    ))
    
  }, error = function(e) {
    cat(sprintf("    ✗ Error: %s\n", e$message))
    return(NULL)
  })
}

# ==============================================================================
# CALIBRAR TODAS LAS ESPECIES
# ==============================================================================

cat("\n[3/4] Calibrando modelos...\n")

# Calculer directement depuis arboles_calibracion
especies_list <- arboles_calibracion %>%
  count(nombre_cientifico, genero_grupo) %>%
  filter(n >= 7) %>%
  arrange(genero_grupo, nombre_cientifico)

cat("\nEspecies a calibrar:\n")
print(especies_list)

resultados <- list()

# Boucler sur les lignes de especies_list
for (i in 1:nrow(especies_list)) {
  
  especie_nombre <- especies_list$nombre_cientifico[i]
  genero_nombre <- especies_list$genero_grupo[i]
  
  cat(sprintf("\n────────────────────────────────────────────────────────\n"))
  cat(sprintf("[%d/%d] Procesando: %s\n", i, nrow(especies_list), especie_nombre))
  
  # Filtrar datos - usar variable local explícita
  datos_esp <- arboles_calibracion %>%
    filter(nombre_cientifico == especie_nombre)
  
  cat(sprintf("  → Filas encontradas: %d\n", nrow(datos_esp)))
  
  if (nrow(datos_esp) == 0) {
    cat("  ✗ ADVERTENCIA: 0 filas después del filter!\n")
    next
  }
  
  # Calibrar
  res <- calibrar_chapman_richards(
    d = datos_esp$diametro_normal,
    h = datos_esp$altura_total,
    nombre_especie = especie_nombre
  )
  
  if (!is.null(res)) {
    resultados[[especie_nombre]] <- res
    resultados[[especie_nombre]]$especie <- especie_nombre
    resultados[[especie_nombre]]$genero <- genero_nombre
  }
}

# ==============================================================================
# TABLA DE RESULTADOS
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              RESULTADOS DE CALIBRACIÓN                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

tabla_resultados <- map_dfr(seq_along(resultados), function(i) {
  res <- resultados[[i]]
  tibble(
    especie = res$especie,
    genero = res$genero,
    n = res$n,
    a = res$a, b = res$b, c = res$c,
    r2 = res$r2, rmse = res$rmse, mae = res$mae
  )
}) %>% arrange(genero, especie)

print(tabla_resultados, n = Inf)

# ==============================================================================
# GENERAR GRÁFICOS
# ==============================================================================

cat("\n[4/4] Generando gráficos...\n")

dir_graficos <- file.path(PROYECTO_ROOT, "resultados/graficos_calibracion")
dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)

# Gráfico por especie
for (i in seq_along(resultados)) {
  
  res <- resultados[[i]]
  especie_nom <- res$especie  # Usar el nombre almacenado en el resultado
  
  cat(sprintf("  Graficando: %s...", especie_nom))
  
  # Filtrar datos
  datos_obs <- arboles_calibracion %>% 
    filter(nombre_cientifico == especie_nom)
  
  if (nrow(datos_obs) == 0) {
    cat(" ✗ No se encontraron datos\n")
    next
  }
  
  d_seq <- seq(min(datos_obs$diametro_normal), max(datos_obs$diametro_normal), length.out = 200)
  h_pred <- res$a * (1 - exp(-res$b * d_seq))^res$c
  
  color_curva <- ifelse(res$genero == "Pinus", "#2ca02c", "#d62728")
  
  p <- ggplot() +
    geom_point(data = datos_obs, aes(x = diametro_normal, y = altura_total),
               alpha = 0.4, size = 2, color = "gray30") +
    geom_line(data = data.frame(d = d_seq, h = h_pred), aes(x = d, y = h),
              color = color_curva, size = 1.5) +
    labs(
      title = bquote(italic(.(especie_nom))),
      subtitle = sprintf(
        "h = %.3f × (1 - exp(-%.5f × d))^%.4f\n%s | n = %d | R² = %.4f | RMSE = %.3f m",
        res$a, res$b, res$c, res$genero, res$n, res$r2, res$rmse
      ),
      x = "Diámetro normal (cm)", y = "Altura total (m)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(family = "mono", size = 9))
  
  archivo <- file.path(dir_graficos, sprintf("%s.png", str_replace_all(especie_nom, " ", "_")))
  ggsave(archivo, p, width = 9, height = 7, dpi = 300)
  cat(" ✓\n")
}

# Gráfico comparativo
if (length(resultados) > 0) {
  d_global <- seq(5, 60, length.out = 200)
  
  predicciones <- map_dfr(seq_along(resultados), function(i) {
    res <- resultados[[i]]
    tibble(
      d = d_global,
      h = res$a * (1 - exp(-res$b * d_global))^res$c,
      especie = res$especie,
      genero = res$genero
    )
  })
  
  p_comp <- ggplot(predicciones, aes(x = d, y = h, color = especie, linetype = genero)) +
    geom_line(size = 1.2) +
    scale_linetype_manual(values = c("Pinus" = "solid", "Quercus" = "dashed")) +
    labs(title = "Comparación de Curvas Altura-Diámetro",
         x = "Diámetro normal (cm)", y = "Altura total (m)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  archivo_comp <- file.path(dir_graficos, "comparacion_todas_especies.png")
  ggsave(archivo_comp, p_comp, width = 12, height = 8, dpi = 300)
  cat(sprintf("  ✓ Gráfico comparativo guardado\n"))
}

# ==============================================================================
# CÓDIGO PARA 02_config_especies.R
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║         CÓDIGO PARA ACTUALIZAR CONFIG ESPECIES            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("PARAMETROS_ALTURA_DIAMETRO <- tribble(\n")
cat("  ~especie, ~n, ~r2, ~a, ~b, ~c,\n")
for (i in 1:nrow(tabla_resultados)) {
  row <- tabla_resultados[i, ]
  cat(sprintf('  "%s", %d, %.4f, %.3f, %.5f, %.4f,\n',
              row$especie, row$n, row$r2, row$a, row$b, row$c))
}
cat(")\n")

# Exportar CSV
archivo_csv <- file.path(PROYECTO_ROOT, "resultados/parametros_chapman_richards_calibrados.csv")
write_csv(tabla_resultados, archivo_csv)

cat(sprintf("\n✓ Tabla exportada: %s\n", basename(archivo_csv)))
cat(sprintf("\n✓ Calibración completa: %d especies, R² promedio = %.4f\n",
            nrow(tabla_resultados), mean(tabla_resultados$r2)))