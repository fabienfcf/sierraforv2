# ==============================================================================
# ANÁLISIS DE RIESGO DE INCENDIO FORESTAL
# Basado en protocolo FIREMON y método de intercepción planar
# ==============================================================================

library(tidyverse)

# Función principal para calcular carga de combustibles
calcular_carga_combustibles <- function(df) {
  
  # Constantes del método de intercepción planar (Brown 1974, van Wagner 1968)
  k_1h <- 0.0241      # Para piezas de 1 hora (0-0.5 cm)
  k_10h <- 0.0956     # Para piezas de 10 horas (0.5-2.5 cm)
  k_100h <- 0.383     # Para piezas de 100 horas (2.5-7.5 cm)
  
  # Longitudes de transecto
  L_1h <- 3           # últimos 3m para finos y regulares
  L_100h <- 12        # 12m completos para medianos
  L_1000h <- 12       # 12m completos para gruesos
  
  df %>%
    rowwise() %>%
    mutate(
      # Carga de combustibles finos (1 hora) en ton/ha
      # NA = no se encontraron combustibles → 0 ton/ha
      carga_1h = (coalesce(combustibles_finos, 0) * k_1h) / L_1h,

      # Carga de combustibles regulares (10 horas) en ton/ha
      carga_10h = (coalesce(combustibles_regulares, 0) * k_10h) / L_1h,

      # Carga de combustibles medianos (100 horas) en ton/ha
      carga_100h = (coalesce(combustibles_medianos, 0) * k_100h) / L_100h,
      
      # Carga de combustibles gruesos (1000 horas) en ton/ha
      # Sumar diámetros al cuadrado de todas las piezas gruesas
      suma_diam_cuadrado = sum(c(
        combustibles_G_diam1, combustibles_G_diam2, combustibles_G_diam3,
        combustibles_G_diam4, combustibles_G_diam5, combustibles_G_diam6,
        combustibles_G_diam7, combustibles_G_diam8, combustibles_G_diam9,
        combustibles_G_diam10
      )^2, na.rm = TRUE),
      
      carga_1000h = (1.234 * suma_diam_cuadrado) / L_1000h,
      
      # Carga total de combustibles
      carga_total = carga_1h + carga_10h + carga_100h + carga_1000h
    ) %>%
    ungroup() %>%
    select(-suma_diam_cuadrado)
}


# Función para calcular continuidad vertical (escalera de fuego)
calcular_continuidad_vertical <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Promedios y máximos
      hojarasca_prom = mean(c(espesor_hojarasca_3m, espesor_hojarasca_6m, 
                              espesor_hojarasca_9m), na.rm = TRUE),
      pasto_max = max(c(altura_pastos_6m, altura_pastos_9m), na.rm = TRUE),
      hierba_max = max(c(altura_hierbas_6m, altura_hierbas_9m), na.rm = TRUE),
      arbusto_max = max(c(altura_arbustos_6m, altura_arbustos_9m), na.rm = TRUE),
      
      # Contar capas presentes (escalera de fuego)
      capa_hojarasca = if_else(hojarasca_prom > 2, 1, 0),
      capa_pasto = if_else(pasto_max > 10, 1, 0),
      capa_hierba = if_else(hierba_max > 10, 1, 0),
      capa_arbusto = if_else(arbusto_max > 50, 1, 0),
      
      # Índice de continuidad vertical (0-1)
      continuidad_vertical = (capa_hojarasca + capa_pasto + 
                                capa_hierba + capa_arbusto) / 4,
      
      # Altura máxima de vegetación (m)
      altura_veg_max = max(c(pasto_max, hierba_max, arbusto_max), na.rm = TRUE) / 100
    ) %>%
    ungroup()
}


# Función para calcular índice de riesgo de incendio
calcular_riesgo_incendio <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Cobertura promedio del dosel (%)
      cobertura_dosel_prom = mean(c(cobertura_dosel_3m, cobertura_dosel_6m, 
                                    cobertura_dosel_9m), na.rm = TRUE),
      
      # Factor de pendiente (aumenta velocidad de propagación)
      factor_pendiente = 1 + (pendiente_transecto / 100) * 0.5,
      
      # Índice de disponibilidad de combustible fino (crítico para ignición)
      indice_combustible_fino = carga_1h + carga_10h,
      
      # Índice de intensidad potencial (combustibles gruesos)
      indice_intensidad = carga_100h + carga_1000h,
      
      # Cálculo del índice de riesgo compuesto (0-100)
      # Combustibles finos (30% del riesgo)
      riesgo_combustible_fino = pmin((indice_combustible_fino / 2) * 30, 30),
      
      # Combustibles gruesos (20% del riesgo)
      riesgo_combustible_grueso = pmin((indice_intensidad / 10) * 20, 20),
      
      # Pendiente (15% del riesgo)
      riesgo_pendiente = pmin((pendiente_transecto / 50) * 15, 15),
      
      # Continuidad vertical (15% del riesgo)
      riesgo_continuidad = continuidad_vertical * 15,
      
      # Hojarasca (10% del riesgo)
      riesgo_hojarasca = pmin((hojarasca_prom / 10) * 10, 10),
      
      # Apertura del dosel (10% del riesgo) - menos cobertura = más riesgo
      riesgo_apertura_dosel = pmin(((100 - cobertura_dosel_prom) / 100) * 10, 10),
      
      # Índice total de riesgo
      indice_riesgo = riesgo_combustible_fino + riesgo_combustible_grueso + 
        riesgo_pendiente + riesgo_continuidad + 
        riesgo_hojarasca + riesgo_apertura_dosel,
      
      # Categoría de riesgo (NA en índice = datos insuficientes → BAJO)
      categoria_riesgo = case_when(
        is.na(indice_riesgo) ~ "BAJO",
        indice_riesgo < 25   ~ "BAJO",
        indice_riesgo < 50   ~ "MODERADO",
        indice_riesgo < 75   ~ "ALTO",
        TRUE                 ~ "EXTREMO"
      )
    ) %>%
    ungroup()
}


# Función para calcular velocidad de propagación
calcular_velocidad_propagacion <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Velocidad base en m/min basada en combustible fino disponible
      velocidad_base = case_when(
        indice_combustible_fino < 0.5 ~ 0.5,
        indice_combustible_fino < 1.5 ~ 2.0,
        indice_combustible_fino < 3.0 ~ 5.0,
        TRUE ~ 10.0
      ),
      
      # Ajuste por pendiente
      velocidad_ajustada_pendiente = velocidad_base * factor_pendiente,
      
      # Ajuste por apertura de dosel (más apertura = más viento = más velocidad)
      factor_dosel = 1 + ((100 - cobertura_dosel_prom) / 100) * 0.3,
      
      # Velocidad final de propagación (m/min)
      velocidad_propagacion = velocidad_ajustada_pendiente * factor_dosel,
      
      # Conversión a cadenas por hora (1 cadena = 20.1168 m)
      velocidad_cadenas_hora = velocidad_propagacion * 60 / 20.1168
    ) %>%
    ungroup()
}


# Función para generar resumen de riesgo
generar_resumen_riesgo <- function(df) {
  df %>%
    select(
      sitio = muestreo,
      pendiente = pendiente_transecto,
      carga_total,
      indice_combustible_fino,
      indice_intensidad,
      continuidad_vertical,
      hojarasca_prom,
      cobertura_dosel_prom,
      indice_riesgo,
      categoria_riesgo,
      velocidad_propagacion,
      velocidad_cadenas_hora
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
}


# ==============================================================================
# APLICACIÓN DEL ANÁLISIS
# ==============================================================================

# Suponiendo que tu dataframe se llama f06
resultado <- inventario$f06 %>%
  calcular_carga_combustibles() %>%
  calcular_continuidad_vertical() %>%
  calcular_riesgo_incendio() %>%
  calcular_velocidad_propagacion()

# Generar resumen
resumen_riesgo <- generar_resumen_riesgo(resultado)

# Mostrar resultados
print("=" %>% rep(80) %>% paste(collapse = ""))
print("RESUMEN DE ANÁLISIS DE RIESGO DE INCENDIO FORESTAL")
print("=" %>% rep(80) %>% paste(collapse = ""))
print(resumen_riesgo)

# Tabla detallada de cargas de combustible
print("\n\nCARGA DE COMBUSTIBLES POR CATEGORÍA (ton/ha)")
print("=" %>% rep(80) %>% paste(collapse = ""))
resultado %>%
  select(sitio = muestreo, carga_1h, carga_10h, carga_100h, 
         carga_1000h, carga_total) %>%
  mutate(across(where(is.numeric) & !matches("sitio"), ~round(., 2))) %>%
  print()

# ==============================================================================
# VISUALIZACIONES
# ==============================================================================

library(ggplot2)
library(patchwork)

# 1. Gráfico de barras apiladas - Carga de combustibles
p1 <- resultado %>%
  select(sitio = muestreo, carga_1h, carga_10h, carga_100h, carga_1000h) %>%
  pivot_longer(cols = starts_with("carga_"), 
               names_to = "categoria", 
               values_to = "carga") %>%
  mutate(
    categoria = factor(categoria, 
                       levels = c("carga_1h", "carga_10h", "carga_100h", "carga_1000h"),
                       labels = c("1 hora (0-0.5cm)", "10 horas (0.5-2.5cm)",
                                  "100 horas (2.5-7.5cm)", "1000 horas (>7.5cm)"))
  ) %>%
  ggplot(aes(x = factor(sitio), y = carga, fill = categoria)) +
  geom_col(position = "stack", color = "white", size = 1) +
  scale_fill_manual(values = c("#fef3c7", "#fde68a", "#fbbf24", "#f59e0b")) +
  labs(title = "Carga de Combustibles por Categoría",
       subtitle = "Basado en método de intercepción planar",
       x = "Sitio de Muestreo",
       y = "Carga (ton/ha)",
       fill = "Categoría de\nCombustible") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# 2. Gráfico de índice de riesgo
p2 <- resultado %>%
  mutate(
    sitio = factor(muestreo),
    color_riesgo = case_when(
      categoria_riesgo == "BAJO" ~ "#10b981",
      categoria_riesgo == "MODERADO" ~ "#f59e0b",
      categoria_riesgo == "ALTO" ~ "#ef4444",
      categoria_riesgo == "EXTREMO" ~ "#7f1d1d"
    )
  ) %>%
  ggplot(aes(x = sitio, y = indice_riesgo, fill = color_riesgo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(indice_riesgo, 1)), 
            vjust = -0.5, fontface = "bold", size = 5) +
  geom_text(aes(label = categoria_riesgo, y = 5), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_identity() +
  labs(title = "Índice de Riesgo de Incendio",
       subtitle = "Basado en 6 factores principales",
       x = "Sitio de Muestreo",
       y = "Índice de Riesgo (0-100)") +
  ylim(0, 105) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# 3. Gráfico de velocidad de propagación
p3 <- resultado %>%
  ggplot(aes(x = factor(muestreo), y = velocidad_propagacion)) +
  geom_segment(aes(xend = factor(muestreo), y = 0, yend = velocidad_propagacion),
               color = "#3b82f6", size = 2) +
  geom_point(size = 6, color = "#1e40af") +
  geom_text(aes(label = round(velocidad_propagacion, 2)), 
            vjust = -1, fontface = "bold", size = 4) +
  labs(title = "Velocidad Potencial de Propagación del Fuego",
       subtitle = "Considerando pendiente y apertura del dosel",
       x = "Sitio de Muestreo",
       y = "Velocidad (m/min)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# 4. Heatmap de factores de riesgo
p4 <- resultado %>%
  select(
    Sitio = muestreo,
    `Comb. Fino` = indice_combustible_fino,
    `Comb. Grueso` = indice_intensidad,
    `Pendiente (%)` = pendiente_transecto,
    `Continuidad` = continuidad_vertical,
    `Hojarasca (cm)` = hojarasca_prom
  ) %>%
  pivot_longer(cols = -Sitio, names_to = "Factor", values_to = "Valor") %>%
  mutate(
    Valor_normalizado = case_when(
      Factor == "Comb. Fino" ~ Valor / max(Valor, na.rm = TRUE) * 100,
      Factor == "Comb. Grueso" ~ Valor / max(Valor, na.rm = TRUE) * 100,
      Factor == "Pendiente (%)" ~ Valor * 2,
      Factor == "Continuidad" ~ Valor * 100,
      Factor == "Hojarasca (cm)" ~ Valor * 10
    )
  ) %>%
  ggplot(aes(x = Factor, y = factor(Sitio), fill = Valor_normalizado)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = round(Valor, 1)), fontface = "bold", size = 4) +
  scale_fill_gradient2(low = "#10b981", mid = "#f59e0b", high = "#ef4444",
                       midpoint = 50, name = "Nivel\n(normalizado)") +
  labs(title = "Factores de Riesgo por Sitio",
       x = NULL, y = "Sitio de Muestreo") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Combinar gráficos
layout <- (p1 | p2) / (p3 | p4)
print(layout + plot_annotation(
  title = "ANÁLISIS COMPLETO DE RIESGO DE INCENDIO FORESTAL",
  subtitle = "Protocolo FIREMON - Método de intercepción planar",
  theme = theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )
))

# Guardar el dataframe completo con todos los cálculos
write.csv(resultado, "analisis_riesgo_incendio_completo.csv", row.names = FALSE)

print("\n✓ Análisis completado. Resultados guardados en 'analisis_riesgo_incendio_completo.csv'")