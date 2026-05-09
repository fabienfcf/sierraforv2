# ==============================================================================
# TABLA 5 - EXISTENCIAS POR UMM (NOM-152)
# Genera LaTeX y CSV con todas las métricas requeridas
# ==============================================================================

library(tidyverse)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║   TABLA 5 - EXISTENCIAS POR UMM (NOM-152)               ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("[1/5] Cargando datos...\n")

# Evolución (año 0 y año 10)
evolucion <- read_csv("resultados/evolucion_rodal_10anos.csv", show_col_types = FALSE)

# Cargar superficie por UMM desde UMM_stats.csv
umm_stats <- read_csv("UMM_stats.csv", locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  select(rodal = id,
         superficie_ha = `SUPERFICIE UMM`,
         superficie_corta_ha = `Superficie en Producción (ha)`)

cat(sprintf("  ✓ Datos cargados\n"))

# ==============================================================================
# FUNCIÓN ÁREA BASAL (VECTORIZADA)
# ==============================================================================

# AB = π/4 × dg² × densidad / 10000 (para convertir cm² a m²)
calcular_ab_ha <- function(dg_cm, densidad_ha) {
  # Vectorizada: usar ifelse para cada elemento
  ifelse(
    is.na(dg_cm) | is.na(densidad_ha) | densidad_ha == 0,
    0,
    (pi / 4) * (dg_cm^2) * densidad_ha / 10000
  )
}

# ==============================================================================
# PREPARAR DATOS AÑO 0 (EXISTENCIAS REALES)
# ==============================================================================

cat("\n[2/5] Preparando existencias reales (t=0)...\n")

# Año 0 por género
ano0_genero <- evolucion %>%
  filter(ano_simulacion == 0) %>%
  left_join(umm_stats, by = "rodal") %>%
  transmute(
    rodal,
    superficie_ha,
    superficie_corta_ha,
    # PINUS
    pinus_vol_ha = pinus_vol_ha_m3,
    pinus_vol_total = pinus_vol_ha_m3 * superficie_ha,
    pinus_ab_ha = calcular_ab_ha(pinus_dg_cm, pinus_densidad_ha),
    # QUERCUS
    quercus_vol_ha = quercus_vol_ha_m3,
    quercus_vol_total = quercus_vol_ha_m3 * superficie_ha,
    quercus_ab_ha = calcular_ab_ha(quercus_dg_cm, quercus_densidad_ha)
  )

# ==============================================================================
# PREPARAR DATOS AÑO 10 (RESIDUALES)
# ==============================================================================

cat("[3/5] Preparando residuales (t=10)...\n")

ano10_genero <- evolucion %>%
  filter(ano_simulacion == 10) %>%
  left_join(umm_stats, by = "rodal") %>%
  transmute(
    rodal,
    # PINUS
    pinus_res_vol_ha = pinus_vol_ha_m3,
    pinus_res_ab_ha = calcular_ab_ha(pinus_dg_cm, pinus_densidad_ha),
    # QUERCUS
    quercus_res_vol_ha = quercus_vol_ha_m3,
    quercus_res_ab_ha = calcular_ab_ha(quercus_dg_cm, quercus_densidad_ha)
  )

# ==============================================================================
# PREPARAR DATOS CORTAS (POSIBILIDAD) - CALCULADA DESDE ICA
# ==============================================================================

cat("[4/5] Preparando posibilidad...\n")

# Cargar ICA por género y rodal
ica_data <- read_csv("resultados/31_ica_por_genero_rodal.csv", show_col_types = FALSE)

# Renombrar si es necesario
if ("genero" %in% names(ica_data) && !"genero_grupo" %in% names(ica_data)) {
  ica_data <- ica_data %>% rename(genero_grupo = genero)
}

# Cargar programa de cortas
source("config/05_config_programa_cortas.R")

# Agregar configuración de cortas
ica_data <- ica_data %>%
  left_join(
    PROGRAMA_CORTAS %>% select(rodal, intensidad_pct, proporcion_quercus),
    by = "rodal"
  )

# Calcular posibilidad con MISMA LÓGICA que Tabla Existencias ICA
posibilidad_data <- ica_data %>%
  mutate(ICA_10anos = ICA_m3_ha * 10) %>%
  group_by(rodal) %>%
  mutate(
    # PASO 1: Posibilidad TOTAL de la UMM
    posibilidad_total_umm = sum(ICA_10anos) * (first(intensidad_pct) / 100),
    
    # PASO 2: Factor por género según proporción
    factor_genero = case_when(
      genero_grupo == "Quercus" ~ first(proporcion_quercus),
      genero_grupo == "Pinus" ~ (1 - first(proporcion_quercus)),
      TRUE ~ 0
    ),
    
    # PASO 3: Posibilidad por género
    posibilidad_vol_m3_ha = posibilidad_total_umm * factor_genero
  ) %>%
  ungroup()

# Expandir para tener todas las combinaciones rodal × género
rodales <- sort(unique(umm_stats$rodal))
generos <- c("Pinus", "Quercus")

posibilidad_completo <- expand_grid(
  rodal = rodales,
  genero_grupo = generos
) %>%
  left_join(
    posibilidad_data %>% select(rodal, genero_grupo, posibilidad_vol_m3_ha, intensidad_pct, proporcion_quercus),
    by = c("rodal", "genero_grupo")
  ) %>%
  replace_na(list(posibilidad_vol_m3_ha = 0, intensidad_pct = 0, proporcion_quercus = 0))

# Calcular AB de posibilidad desde evolucion año anterior
# Necesitamos dg y densidad del año ANTERIOR a la corta
anos_corta <- PROGRAMA_CORTAS %>%
  select(rodal, ano_corta)

evolucion_ano_anterior <- evolucion %>%
  left_join(anos_corta, by = "rodal") %>%
  mutate(ano_anterior = ano_corta - 1) %>%
  filter(ano_simulacion == ano_anterior) %>%
  select(rodal, pinus_dg_cm, pinus_densidad_ha, quercus_dg_cm, quercus_densidad_ha)

# Para posibilidad, usar densidad proporcional a la intensidad
posibilidad_completo <- posibilidad_completo %>%
  left_join(evolucion_ano_anterior, by = "rodal") %>%
  mutate(
    # Densidad cortada = densidad × intensidad% × factor_género
    densidad_cortada = case_when(
      genero_grupo == "Pinus" ~ pinus_densidad_ha * (intensidad_pct / 100) * (1 - proporcion_quercus),
      genero_grupo == "Quercus" ~ quercus_densidad_ha * (intensidad_pct / 100) * proporcion_quercus,
      TRUE ~ 0
    ),
    # dg del año anterior
    dg_cm = case_when(
      genero_grupo == "Pinus" ~ pinus_dg_cm,
      genero_grupo == "Quercus" ~ quercus_dg_cm,
      TRUE ~ 0
    ),
    # AB de posibilidad
    ab_ha = calcular_ab_ha(dg_cm, densidad_cortada)
  )

# Pivotar
posibilidad <- posibilidad_completo %>%
  pivot_wider(
    id_cols = rodal,
    names_from = genero_grupo,
    values_from = c(posibilidad_vol_m3_ha, ab_ha),
    names_glue = "{tolower(genero_grupo)}_{.value}"
  ) %>%
  rename(
    pinus_pos_vol_m3_ha = pinus_posibilidad_vol_m3_ha,
    pinus_pos_ab_ha = pinus_ab_ha,
    quercus_pos_vol_m3_ha = quercus_posibilidad_vol_m3_ha,
    quercus_pos_ab_ha = quercus_ab_ha
  )

cat(sprintf("  ✓ Posibilidad calculada desde ICA\n"))

# ==============================================================================
# CALCULAR INTENSIDAD DE CORTA (IC%)
# ==============================================================================

cat("[4.5/5] Calculando IC% (año anterior a corta)...\n")

# Obtener años de corta por rodal
anos_corta <- cortas %>%
  group_by(rodal_cortado) %>%
  summarise(ano_corta = first(ano_corta), .groups = "drop") %>%
  rename(rodal = rodal_cortado)

# ER del año ANTERIOR a la corta
er_ano_anterior <- evolucion %>%
  left_join(anos_corta, by = "rodal") %>%
  mutate(ano_anterior = ano_corta - 1) %>%
  filter(ano_simulacion == ano_anterior) %>%
  left_join(umm_stats, by = "rodal") %>%
  transmute(
    rodal,
    pinus_er_vol_ha = pinus_vol_ha_m3,
    quercus_er_vol_ha = quercus_vol_ha_m3,
    total_er_vol_ha = pinus_vol_ha_m3 + quercus_vol_ha_m3
  )

# IC% = (Posibilidad / ER_año_anterior) × 100
intensidad <- posibilidad %>%
  left_join(er_ano_anterior, by = "rodal") %>%
  mutate(
    # IC por género
    pinus_ic_pct = ifelse(!is.na(pinus_er_vol_ha) & pinus_er_vol_ha > 0, 
                          (pinus_pos_vol_m3_ha / pinus_er_vol_ha) * 100, 0),
    quercus_ic_pct = ifelse(!is.na(quercus_er_vol_ha) & quercus_er_vol_ha > 0, 
                            (quercus_pos_vol_m3_ha / quercus_er_vol_ha) * 100, 0),
    # IC total UMM
    posibilidad_total_ha = pinus_pos_vol_m3_ha + quercus_pos_vol_m3_ha,
    ic_umm_pct = ifelse(!is.na(total_er_vol_ha) & total_er_vol_ha > 0, 
                        (posibilidad_total_ha / total_er_vol_ha) * 100, 0)
  ) %>%
  select(rodal, pinus_ic_pct, quercus_ic_pct, ic_umm_pct)

cat(sprintf("  ✓ IC%% calculado para %d UMM\n", nrow(intensidad)))

# ==============================================================================
# CONSOLIDAR DATOS
# ==============================================================================

tabla5_data <- ano0_genero %>%
  left_join(ano10_genero, by = "rodal") %>%
  left_join(posibilidad, by = "rodal") %>%
  left_join(intensidad, by = "rodal")

cat(sprintf("  ✓ Datos consolidados para %d UMM\n", nrow(tabla5_data)))

# ==============================================================================
# CALCULAR TOTALES GENERALES (PONDERADOS)
# ==============================================================================

cat("\n[5/5] Calculando totales generales...\n")

superficie_total <- sum(tabla5_data$superficie_ha)

# Totales ponderados POR GÉNERO - CORREGIDO
totales_genero_predio <- tabla5_data %>%
  mutate(
    # Ponderar todo ANTES del summarise
    pinus_vol_pond = pinus_vol_total,  # Ya está en m³ totales
    pinus_ab_pond = pinus_ab_ha * superficie_ha,
    pinus_res_vol_pond = pinus_res_vol_ha * superficie_ha,
    pinus_res_ab_pond = pinus_res_ab_ha * superficie_ha,
    pinus_pos_vol_pond = pinus_pos_vol_m3_ha * superficie_corta_ha,
    pinus_pos_ab_pond = pinus_pos_ab_ha * superficie_corta_ha,

    quercus_vol_pond = quercus_vol_total,  # Ya está en m³ totales
    quercus_ab_pond = quercus_ab_ha * superficie_ha,
    quercus_res_vol_pond = quercus_res_vol_ha * superficie_ha,
    quercus_res_ab_pond = quercus_res_ab_ha * superficie_ha,
    quercus_pos_vol_pond = quercus_pos_vol_m3_ha * superficie_corta_ha,
    quercus_pos_ab_pond = quercus_pos_ab_ha * superficie_corta_ha
  ) %>%
  summarise(
    superficie_ha = superficie_total,
    
    # PINUS - EXISTENCIAS REALES (t=0)
    pinus_vol_total = sum(pinus_vol_pond, na.rm = TRUE),
    pinus_vol_ha = pinus_vol_total / superficie_total,
    pinus_ab_ha = sum(pinus_ab_pond, na.rm = TRUE) / superficie_total,
    
    # PINUS - RESIDUALES (t=10)
    pinus_res_vol_total = sum(pinus_res_vol_pond, na.rm = TRUE),
    pinus_res_vol_ha = pinus_res_vol_total / superficie_total,
    pinus_res_ab_total = sum(pinus_res_ab_pond, na.rm = TRUE),
    pinus_res_ab_ha = pinus_res_ab_total / superficie_total,
    
    # PINUS - POSIBILIDAD
    pinus_pos_vol_total = sum(pinus_pos_vol_pond, na.rm = TRUE),
    pinus_pos_vol_m3_ha = pinus_pos_vol_total / superficie_total,
    pinus_pos_ab_total = sum(pinus_pos_ab_pond, na.rm = TRUE),
    pinus_pos_ab_ha = pinus_pos_ab_total / superficie_total,
    
    # QUERCUS - EXISTENCIAS REALES (t=0)
    quercus_vol_total = sum(quercus_vol_pond, na.rm = TRUE),
    quercus_vol_ha = quercus_vol_total / superficie_total,
    quercus_ab_ha = sum(quercus_ab_pond, na.rm = TRUE) / superficie_total,
    
    # QUERCUS - RESIDUALES (t=10)
    quercus_res_vol_total = sum(quercus_res_vol_pond, na.rm = TRUE),
    quercus_res_vol_ha = quercus_res_vol_total / superficie_total,
    quercus_res_ab_total = sum(quercus_res_ab_pond, na.rm = TRUE),
    quercus_res_ab_ha = quercus_res_ab_total / superficie_total,
    
    # QUERCUS - POSIBILIDAD
    quercus_pos_vol_total = sum(quercus_pos_vol_pond, na.rm = TRUE),
    quercus_pos_vol_m3_ha = quercus_pos_vol_total / superficie_total,
    quercus_pos_ab_total = sum(quercus_pos_ab_pond, na.rm = TRUE),
    quercus_pos_ab_ha = quercus_pos_ab_total / superficie_total
  )

# ==============================================================================
# CORRECCIÓN: Calcular IC% a nivel predio ANTES de total_predio
# Insertar este código DESPUÉS de la línea:
# cat(sprintf("  ✓ IC%% calculado para %d UMM\n", nrow(intensidad)))
# y ANTES de la línea:
# # TOTAL PREDIO (suma de ambos géneros)
# ==============================================================================

# IC% A NIVEL PREDIO (calcular antes de total_predio)
# Necesitamos ER del año anterior a nivel predio (suma ponderada)
er_predio_ano_anterior <- er_ano_anterior %>%
  left_join(umm_stats, by = "rodal") %>%
  summarise(
    pinus_er_total = sum(pinus_er_vol_ha * superficie_ha, na.rm = TRUE),
    quercus_er_total = sum(quercus_er_vol_ha * superficie_ha, na.rm = TRUE),
    total_er = sum(total_er_vol_ha * superficie_ha, na.rm = TRUE)
  )

# Calcular IC% predio
ic_predio_detallado <- totales_genero_predio %>%
  mutate(
    # IC% por género
    pinus_ic_pct = ifelse(er_predio_ano_anterior$pinus_er_total > 0,
                          (pinus_pos_vol_total / er_predio_ano_anterior$pinus_er_total) * 100,
                          0),
    quercus_ic_pct = ifelse(er_predio_ano_anterior$quercus_er_total > 0,
                            (quercus_pos_vol_total / er_predio_ano_anterior$quercus_er_total) * 100,
                            0),
    # IC% total predio
    pos_total = pinus_pos_vol_total + quercus_pos_vol_total,
    predio_ic_pct = ifelse(er_predio_ano_anterior$total_er > 0,
                           (pos_total / er_predio_ano_anterior$total_er) * 100,
                           0)
  )

cat(sprintf("  ✓ IC%% Pinus predio calculado: %.1f%%\n", ic_predio_detallado$pinus_ic_pct))
cat(sprintf("  ✓ IC%% Quercus predio calculado: %.1f%%\n", ic_predio_detallado$quercus_ic_pct))
cat(sprintf("  ✓ IC%% Total predio calculado: %.1f%%\n", ic_predio_detallado$predio_ic_pct))

# ==============================================================================
# Ahora sí se puede crear total_predio usando ic_predio_detallado
# ==============================================================================

# TOTAL PREDIO (suma de ambos géneros)
total_predio <- totales_genero_predio %>%
  mutate(
    # Volumen y AB de existencias (t=0)
    vol_ha = pinus_vol_ha + quercus_vol_ha,
    vol_total = pinus_vol_total + quercus_vol_total,
    ab_ha = pinus_ab_ha + quercus_ab_ha,
    # Residuales (t=10) - SUMAR TOTALES
    res_vol_total = pinus_res_vol_total + quercus_res_vol_total,
    res_vol_ha = res_vol_total / superficie_ha,
    res_ab_total = pinus_res_ab_total + quercus_res_ab_total,
    res_ab_ha = res_ab_total / superficie_ha,
    # Posibilidad - SUMAR TOTALES
    pos_vol_total = pinus_pos_vol_total + quercus_pos_vol_total,
    pos_vol_ha = pos_vol_total / superficie_ha,
    pos_ab_total = pinus_pos_ab_total + quercus_pos_ab_total,
    pos_ab_ha = pos_ab_total / superficie_ha,
    # IC% por género y total
    pinus_ic_pct = ic_predio_detallado$pinus_ic_pct,
    quercus_ic_pct = ic_predio_detallado$quercus_ic_pct,
    predio_ic_pct = ic_predio_detallado$predio_ic_pct
  )

cat(sprintf("  ✓ Superficie total: %.2f ha\n", superficie_total))
cat(sprintf("  ✓ IC%% Pinus predio: %.1f%%\n", total_predio$pinus_ic_pct))
cat(sprintf("  ✓ IC%% Quercus predio: %.1f%%\n", total_predio$quercus_ic_pct))
cat(sprintf("  ✓ IC%% total predio: %.1f%%\n", total_predio$predio_ic_pct))
# ==============================================================================
# GENERAR TABLA LATEX
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("GENERANDO TABLA LATEX...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Función formatear (3 cifras significativas)
fmt <- function(x) {
  if (is.na(x)) return("0.00")
  if (x == 0) return("0.00")
  
  # 3 cifras significativas
  x_sig <- signif(x, 3)
  
  # Formatear con separador de miles
  resultado <- format(x_sig, scientific = FALSE, big.mark = ",", trim = TRUE)
  
  # Si es entero grande (>= 100), quitar decimales innecesarios
  if (x_sig >= 100 && x_sig == floor(x_sig)) {
    resultado <- format(floor(x_sig), big.mark = ",", trim = TRUE)
  }
  
  return(resultado)
}

# Iniciar tabla
latex <- c(
  "\\begin{table}[H]",
  "\t\\centering",
  "\t\\caption{Existencias por unidad mínima de manejo (Tabla 5 - Sección 4.10.1 NOM-152)}",
  "\t\\label{tab:existencias_umm}",
  "\t\\scriptsize",
  "\t\\begin{tabular}{c@{\\hspace{7pt}}c@{\\hspace{8pt}}l@{\\hspace{7pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{7pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}c@{\\hspace{6pt}}r}",
  "\t\t\\toprule",
  "\t\t\\textbf{UMM} & \\textbf{Sup.} & \\multirow{2}{*}{\\textbf{Especie}} & \\multicolumn{3}{c}{\\textbf{Existencias reales (t=0)}} & \\textbf{IC} & \\multicolumn{2}{c}{\\textbf{Residuales (t=10)}} & \\multicolumn{2}{c}{\\textbf{Posibilidad}} \\\\",
  "\t\t\\cmidrule(lr){4-6} \\cmidrule(lr){8-9} \\cmidrule(lr){10-11}",
  "\t\t\\textbf{N.º} & \\textbf{(ha)} & & \\textbf{m$^3$ VTA/ha} & \\textbf{m$^3$ VTA} & \\textbf{m$^2$/ha} & \\textbf{(\\%)} & \\textbf{m$^3$ VTA/ha} & \\textbf{m$^2$/ha} & \\textbf{m$^3$ VTA/ha} & \\textbf{m$^2$/ha} \\\\",
  "\t\t\\midrule"
)

# Filas por UMM
for (i in 1:nrow(tabla5_data)) {
  d <- tabla5_data[i, ]
  
  # Pinus
  latex <- c(latex,
             sprintf("\t\t\\multirow{3}{*}{%d} & \\multirow{3}{*}{%.1f} & \\textit{Pinus} & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                     d$rodal, d$superficie_ha,
                     fmt(d$pinus_vol_ha), fmt(d$pinus_vol_total), fmt(d$pinus_ab_ha),
                     fmt(d$pinus_ic_pct),
                     fmt(d$pinus_res_vol_ha), fmt(d$pinus_res_ab_ha),
                     fmt(d$pinus_pos_vol_m3_ha), fmt(d$pinus_pos_ab_ha))
  )
  
  # Quercus
  latex <- c(latex,
             sprintf("\t\t& & \\textit{Quercus} & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                     fmt(d$quercus_vol_ha), fmt(d$quercus_vol_total), fmt(d$quercus_ab_ha),
                     fmt(d$quercus_ic_pct),
                     fmt(d$quercus_res_vol_ha), fmt(d$quercus_res_ab_ha),
                     fmt(d$quercus_pos_vol_m3_ha), fmt(d$quercus_pos_ab_ha))
  )
  
  # Subtotal
  vol_total_ha <- d$pinus_vol_ha + d$quercus_vol_ha
  vol_total <- d$pinus_vol_total + d$quercus_vol_total
  ab_total_ha <- d$pinus_ab_ha + d$quercus_ab_ha
  res_vol_ha <- d$pinus_res_vol_ha + d$quercus_res_vol_ha
  res_ab_ha <- d$pinus_res_ab_ha + d$quercus_res_ab_ha
  pos_vol_ha <- d$pinus_pos_vol_m3_ha + d$quercus_pos_vol_m3_ha
  pos_ab_ha <- d$pinus_pos_ab_ha + d$quercus_pos_ab_ha
  
  latex <- c(latex,
             sprintf("\t\t\\rowcolor{gray!15} & & \\textbf{Subtotal} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                     fmt(vol_total_ha), fmt(vol_total), fmt(ab_total_ha),
                     fmt(d$ic_umm_pct),
                     fmt(res_vol_ha), fmt(res_ab_ha),
                     fmt(pos_vol_ha), fmt(pos_ab_ha))
  )
  
  # Separador
  if (i < nrow(tabla5_data)) {
    latex <- c(latex, "\t\t\\midrule")
  }
}

# Total predio - FORMATO: TOTAL, Pinus, Quercus, PREDIO
tp <- totales_genero_predio
total <- total_predio

latex <- c(latex,
           "\t\t\\midrule",
           # Fila TOTAL - Pinus
           sprintf("\t\t\\textbf{TOTAL} & \\textbf{%.0f} & \\textit{\\textbf{Pinus}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   tp$superficie_ha,
                   fmt(tp$pinus_vol_ha), 
                   fmt(tp$pinus_vol_total), 
                   fmt(tp$pinus_ab_ha),
                   fmt(total$pinus_ic_pct),
                   fmt(tp$pinus_res_vol_ha), 
                   fmt(tp$pinus_res_ab_ha),
                   fmt(tp$pinus_pos_vol_m3_ha), 
                   fmt(tp$pinus_pos_ab_ha))
)

# Quercus
latex <- c(latex,
           sprintf("\t\t & & \\textit{\\textbf{Quercus}} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(tp$quercus_vol_ha), 
                   fmt(tp$quercus_vol_total), 
                   fmt(tp$quercus_ab_ha),
                   fmt(total$quercus_ic_pct),
                   fmt(tp$quercus_res_vol_ha), 
                   fmt(tp$quercus_res_ab_ha),
                   fmt(tp$quercus_pos_vol_m3_ha), 
                   fmt(tp$quercus_pos_ab_ha))
)

# PREDIO (subtotal) - usar columnas _total correctas
latex <- c(latex,
           sprintf("\t\t\\rowcolor{gray!35} & & \\textbf{PREDIO} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                   fmt(total$vol_ha), 
                   fmt(total$vol_total), 
                   fmt(total$ab_ha),
                   fmt(total$predio_ic_pct),
                   fmt(total$res_vol_ha), 
                   fmt(total$res_ab_ha),
                   fmt(total$pos_vol_ha), 
                   fmt(total$pos_ab_ha))
)

# Cerrar tabla
latex <- c(latex,
           "\t\t\\bottomrule",
           "\t\\end{tabular}",
           "\t\\\\[0.3cm]",
           "\t{\\scriptsize IC: Intensidad de Corta relativa a la ER del año previo a la corta; m$^3$ VTA: volumen total arból}",
           "\\end{table}"
)

# Escribir archivo
if (!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)
latex_file <- "resultados/tablas/tabla_5_existencias_umm.tex"
writeLines(latex, latex_file)

cat(paste(latex, collapse = "\n"))
cat("\n\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("✓ Tabla guardada: %s\n", latex_file))

# ==============================================================================
# GENERAR CSV
# ==============================================================================

csv_data <- tabla5_data %>%
  mutate(tipo = "umm") %>%
  bind_rows(
    total_predio %>% mutate(rodal = 999, tipo = "total_predio")
  )

csv_file <- "resultados/tablas/tabla_5_existencias_umm.csv"
write_csv(csv_data, csv_file)

cat(sprintf("✓ CSV guardado: %s\n\n", csv_file))

writeLines(latex, "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/LATEX/tablas/tabla_5_existencias_umm.tex")
