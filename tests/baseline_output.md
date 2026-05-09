# Baseline de salida — 40_WORKFLOW_COMPLETO.R
Generado: 2026-04-23  
Commit: e0eef1e (Limpieza Fase 1)  
Estado: **ÉXITO** (sin errores, 1 warning pre-existente)

---

## Inventario importado

| Hoja   | Registros |
|--------|-----------|
| F02 regeneración | 2 256 |
| F03 árboles      | 290 |
| **Árboles con volumen calculado** | **2 022** |

## Resumen general (desc_01)

| Métrica | Valor |
|---------|-------|
| n_sitios | 58 |
| n_rodales | 6 |
| n_especies | 12 |
| n_géneros | 8 |
| densidad_ha | 348.6 árb/ha |
| d_medio_cm | 22.64 |
| dmc_cm | 24.21 |
| ab_media_m2ha | 16.05 |
| vol_medio_m3ha | 75.40 |
| Volumen total inventario | 437.30 m³ |

## ICA por rodal (31_ica_por_rodal.csv)

| Rodal | Sup_ha | ER_m3ha | ICA_m3ha | VC_ha_m3 | ER_rodal_m3 | VC_rodal_m3 |
|-------|--------|---------|---------|---------|------------|------------|
| 1 | 44.76 | 69.97 | 2.104 | 17.94 | 3 131.9 | 782.1 |
| 2 | 16.83 | 50.66 | 1.441 | 12.39 | 852.6 | 195.7 |
| 3 | 25.54 | 90.80 | 2.895 | 24.46 | 2 319.1 | 746.0 |
| 4 | 26.26 | 88.38 | 2.218 | 19.40 | 2 321.0 | 494.8 |
| 5 | 20.28 | 70.05 | 2.403 | 20.05 | 1 420.5 | 403.1 |
| 6 | 28.27 | 63.87 | 1.934 | 16.47 | 1 805.6 | 459.6 |

## Volumen objetivo por intervención Liocourt

| UMM | Vol obj m³/ha | Sup ha | Vol obj total m³ | Vol marcado m³/ha | Diferencia % |
|-----|--------------|--------|-----------------|------------------|-------------|
| 1   | 14.72 | 44.76 | 659.08 | 14.92 | +1.3% |
| 2   | 7.20  | 16.83 | 121.23 | 8.05  | +11.7% |
| 3   | 23.16 | 25.54 | 591.49 | 23.24 | +0.3% |
| 4   | 15.53 | 26.26 | 407.76 | 15.66 | +0.8% |
| 5   | 16.82 | 20.28 | 341.12 | 16.86 | +0.2% |
| 6   | 12.57 | 28.27 | 355.37 | 13.38 | +6.5% |

## Cortas totales (cortas_resumen_total.csv)

| Género  | n_árboles | vol_m³ | dg_cm |
|---------|-----------|--------|-------|
| Pinus   | 30        | 13.08  | 30.71 |
| Quercus | 240       | 77.90  | 29.19 |
| **TOTAL** | **270** | **90.98** | — |

Rodales con corta: **6**

## Población en simulación

| Momento | Árboles vivos |
|---------|--------------|
| Inicio simulación | 2 119 |

## Warnings pre-existentes (no son errores nuevos)

1. `geom_col/geom_text`: filas fuera de rango de escala (cosmético, gráficos)
2. `filter()` en cortas optimizer: `Unknown or uninitialised column: 'arbol_id'` — bug pre-existente en el optimizador Liocourt al filtrar maduros ya marcados
