# ==============================================================================
# DISTRIBUCIÓN DIAMÉTRICA INICIAL - HISTOGRAMAS APILADOS + LIOCOURT
# Clases de 5 cm, escala vertical fija, con curva J-invertida ideal
# ==============================================================================

library(tidyverse)
library(patchwork)

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

source("config/01_parametros_configuracion.R")
source("config/05_config_programa_cortas.R")  # ← Cargar parámetros Liocourt

if (!dir.exists("graficos")) dir.create("graficos", recursive = TRUE)

cat("\n[1/5] Cargando datos iniciales...\n")

historial <- readRDS("resultados/historial_completo_10anos.rds")

inicial <- historial %>%
  filter(ano_simulacion == 0, es_arbol_vivo(dominancia))

cat(sprintf("  ✓ %d árboles vivos en año 0\n", nrow(inicial)))
cat(sprintf("  ✓ Parámetros Liocourt: Q = %.2f, Clase ref = %d cm\n", 
            Q_FACTOR, CLASE_REFERENCIA_LIOCOURT))

# ==============================================================================
# CALCULAR DENSIDAD POR UMM - CLASES DE 5 CM
# ==============================================================================

cat("\n[2/5] Calculando densidad por UMM (clases de 5 cm)...\n")

asignar_clase_5cm <- function(d) {
  breaks <- seq(5, 70, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  cut(d, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
}

# Área muestreada por UMM
area_muestreo <- inicial %>%
  group_by(rodal) %>%
  summarise(num_sitios = n_distinct(muestreo), .groups = "drop") %>%
  mutate(area_ha = num_sitios * CONFIG$area_parcela_ha)

# Densidad por UMM, género y clase
densidad_umm <- inicial %>%
  mutate(clase_d = asignar_clase_5cm(diametro_normal)) %>%
  filter(!is.na(clase_d)) %>%
  group_by(rodal, genero_grupo, clase_d) %>%
  summarise(n_arboles = n(), .groups = "drop") %>%
  left_join(area_muestreo, by = "rodal") %>%
  mutate(arboles_ha = n_arboles / area_ha)

n_umm <- n_distinct(densidad_umm$rodal)
cat(sprintf("  ✓ %d UMM procesados\n", n_umm))

# ==============================================================================
# CALCULAR DISTRIBUCIÓN LIOCOURT POR UMM - N_REF FIJO
# ==============================================================================

cat("\n[3/5] Calculando distribución Liocourt ideal por UMM...\n")

# Densidad total por UMM y clase (Pinus + Quercus)
densidad_total_umm <- densidad_umm %>%
  group_by(rodal, clase_d) %>%
  summarise(arboles_ha = sum(arboles_ha), .groups = "drop")

# PARÁMETROS LIOCOURT
N_REF_LIOCOURT <- 10  # arboles/ha en clase 40-45 cm
CLASE_REF_CM <- 42.5  # Centro de la clase 40-45

cat(sprintf("  ✓ N referencia: %d árb/ha en clase 40-45 cm\n", N_REF_LIOCOURT))
cat(sprintf("  ✓ Q-factor: %.2f\n", Q_FACTOR))

# Calcular Liocourt estandarizado
calcular_liocourt_estandar <- function(n_ref, q, clase_ref_centro) {
  
  # Todas las clases (centros)
  breaks <- seq(5, 70, by = 5)
  clases_centro <- breaks[-length(breaks)] + 2.5
  
  # Calcular densidad Liocourt para cada clase
  # N(d) = N_ref × q^((d_ref - d) / amplitud_clase)
  
  # ⚠️ PREGUNTA: Q_FACTOR es para clases de 5 cm o 10 cm?
  # Si Q para 10 cm → usar /10
  # Si Q para 5 cm → usar /5
  amplitud <- 5  # ← AJUSTAR según tu definición de Q_FACTOR
  
  data.frame(
    clase_centro = clases_centro
  ) %>%
    mutate(
      exponente = (clase_ref_centro - clase_centro) / amplitud,
      arboles_ha_liocourt = n_ref * q^exponente
    ) %>%
    select(clase_centro, arboles_ha_liocourt)
}

# Generar curva Liocourt estandarizada (igual para todos los UMM)
liocourt_estandar <- calcular_liocourt_estandar(
  N_REF_LIOCOURT, 
  Q_FACTOR, 
  CLASE_REF_CM
)

# Replicar para cada UMM
umm_lista <- unique(densidad_total_umm$rodal)

liocourt_por_umm <- expand_grid(
  rodal = umm_lista,
  liocourt_estandar
) %>%
  rename(clase_num = clase_centro)

cat(sprintf("  ✓ Curva Liocourt estandarizada aplicada a %d UMM\n", 
            length(umm_lista)))

# Mostrar algunos valores
cat("\n  Valores Liocourt (árb/ha):\n")
cat("  ════════════════════════════\n")
print(
  liocourt_estandar %>%
    filter(clase_centro %in% c(12.5, 22.5, 32.5, 42.5, 52.5, 62.5)) %>%  # ← AVANT select
    mutate(clase_d = paste0(clase_centro - 2.5, "-", clase_centro + 2.5)) %>%
    select(clase_d, arboles_ha_liocourt)
)
# ==============================================================================
# CALCULAR ESCALA VERTICAL COMÚN
# ==============================================================================

cat("\n[4/5] Calculando escala vertical común...\n")

# Máximo entre datos reales y Liocourt
max_real <- densidad_total_umm %>%
  pull(arboles_ha) %>%
  max()

max_liocourt <- liocourt_por_umm %>%
  pull(arboles_ha_liocourt) %>%
  max()

max_densidad <- max(max_real)
max_y <- ceiling(max_densidad / 10) * 10

cat(sprintf("  ✓ Densidad máxima real: %.1f árb/ha\n", max_real))
cat(sprintf("  ✓ Densidad máxima Liocourt: %.1f árb/ha\n", max_liocourt))
cat(sprintf("  ✓ Eje Y común: 0 - %d árb/ha\n", max_y))

# ==============================================================================
# FUNCIÓN CREAR HISTOGRAMA CON CURVA LIOCOURT
# ==============================================================================

crear_histograma_con_liocourt <- function(datos, datos_liocourt, umm_id, ylim_max) {
  
  df <- datos %>% filter(rodal == umm_id)
  df_lio <- datos_liocourt %>% filter(rodal == umm_id)
  
  if (nrow(df) == 0) return(NULL)
  
  # Todas las clases de 5 cm
  breaks <- seq(5, 70, by = 5)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  # Completar combinaciones faltantes
  df <- df %>%
    complete(
      genero_grupo = c("Pinus", "Quercus"),
      clase_d = labels,
      fill = list(arboles_ha = 0)
    ) %>%
    mutate(clase_d = factor(clase_d, levels = labels))
  
  # Preparar eje X para Liocourt (centros de clase)
  if (nrow(df_lio) > 0) {
    df_lio <- df_lio %>%
      mutate(
        clase_d = cut(
          clase_num, 
          breaks = seq(5, 70, by = 5),
          labels = labels,
          include.lowest = TRUE,
          right = FALSE
        )
      )
  }
  
  # Gráfico base
  p <- ggplot(df, aes(x = clase_d, y = arboles_ha, fill = genero_grupo)) +
    geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
    scale_fill_manual(
      values = c("Pinus" = "#2E7D32", "Quercus" = "#D84315"),
      name = "Género"
    ) +
    labs(
      title = sprintf("UMM %d", umm_id),
      x = "Clase diamétrica (cm)",
      y = "Densidad (árb/ha)"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8)
    ) +
    scale_y_continuous(
      limits = c(0, ylim_max),
      expand = expansion(mult = c(0, 0.02))
    )
  
  # Agregar curva Liocourt si existe
  if (nrow(df_lio) > 0) {
    p <- p + 
      geom_line(
        data = df_lio,
        aes(x = clase_d, y = arboles_ha_liocourt, group = 1),
        color = "blue",
        linewidth = 0.8,
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      geom_point(
        data = df_lio,
        aes(x = clase_d, y = arboles_ha_liocourt),
        color = "blue",
        size = 1.5,
        inherit.aes = FALSE
      )
  }
  
  return(p)
}

# ==============================================================================
# GENERAR GRÁFICOS
# ==============================================================================

cat("\n[5/5] Generando histogramas con curvas Liocourt...\n")

umm_lista <- sort(unique(densidad_umm$rodal))

graficos <- list()

for (umm_id in umm_lista) {
  graficos[[length(graficos) + 1]] <- crear_histograma_con_liocourt(
    densidad_umm,
    liocourt_por_umm,
    umm_id,
    max_y
  )
}

graficos <- graficos[!sapply(graficos, is.null)]

n_graficos <- length(graficos)
n_cols <- 2
n_rows <- ceiling(n_graficos / n_cols)

cat(sprintf("  ✓ Generando %d histogramas en layout %d × %d\n", 
            n_graficos, n_rows, n_cols))

# Combinar
p_final <- wrap_plots(graficos, ncol = n_cols) +
  plot_annotation(
    title = "Distribución Diamétrica Inicial vs Distribución Ideal Liocourt",
    subtitle = sprintf("Clases de 5 cm - Pinus (verde) + Quercus (naranja) - Línea azul: J-invertida ideal (Q = %.2f)", Q_FACTOR),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30")
    )
  )

altura <- 3.5 * n_rows

ggsave(
  "graficos/distribucion_diametrica_INICIAL_vs_LIOCOURT.png",
  p_final,
  width = 12,
  height = altura,
  dpi = 300
)

cat("\n╔═══════════════════════════════════════════════════════╗\n")
cat("║  ✓ GRÁFICO GENERADO                                   ║\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVO:\n")
cat("  📊 graficos/distribucion_diametrica_INICIAL_vs_LIOCOURT.png\n\n")

cat("CONFIGURACIÓN:\n")
cat(sprintf("  • Dimensiones:  12 × %.1f pulgadas\n", altura))
cat(sprintf("  • Resolución:   300 DPI\n"))
cat(sprintf("  • Layout:       %d filas × %d columnas\n", n_rows, n_cols))
cat(sprintf("  • UMM:          %d histogramas\n", n_graficos))
cat(sprintf("  • Liocourt:     Q = %.2f, Clase ref = %d cm\n", 
            Q_FACTOR, CLASE_REFERENCIA_LIOCOURT))

# ==============================================================================
# ANÁLISIS COMPARATIVO
# ==============================================================================

cat("\nCOMPARACIÓN REAL vs LIOCOURT POR UMM:\n")
cat("═══════════════════════════════════════════════════════\n\n")

comparacion <- densidad_total_umm %>%
  group_by(rodal) %>%
  summarise(dens_real = sum(arboles_ha), .groups = "drop") %>%
  left_join(
    liocourt_por_umm %>%
      group_by(rodal) %>%
      summarise(dens_liocourt = sum(arboles_ha_liocourt), .groups = "drop"),
    by = "rodal"
  ) %>%
  mutate(
    diferencia = dens_real - dens_liocourt,
    pct_diff = (diferencia / dens_liocourt) * 100
  )

print(comparacion, n = Inf)

cat("\n")