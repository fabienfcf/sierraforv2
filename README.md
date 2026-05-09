# SIERRAFOR v2

**Sistema Integrado de Estimación y Regulación de Recursos Forestales**

Modelo de simulación forestal de dinámica poblacional (crecimiento, reclutamiento y mortalidad) para bosques de pino-encino de zonas montañosas del noreste de México. Diseñado para la elaboración de Programas de Manejo Forestal conforme a la **NOM-152-SEMARNAT-2023**.

---

## Descripción

SIERRAFOR permite simular la evolución del bosque a 10 años, con o sin cortas de saneamiento, y generar automáticamente las tablas y gráficos requeridos por la normatividad mexicana. Incluye:

- Modelos de crecimiento altura-diámetro (Chapman-Richards) calibrados con el inventario local
- Estimación biológica del ICA (Incremento Corriente Anual)
- Optimización de intensidad de cortas mediante criterio ICA-Liocourt
- Generación de 7+ tablas LaTeX listas para integrar al PMF
- Análisis de distribución diamétrica, erosión potencial y sanidad forestal

## Estructura del proyecto

```
sierraforv2/
├── config/            # Parámetros y configuración modular
│   ├── 00_importar_inventario.R    # Lectura y limpieza del Excel de campo
│   ├── 01_parametros_configuracion.R
│   ├── 02_config_especies.R        # Coeficientes por especie (Pinus/Quercus)
│   ├── 03_config_codigos.R         # Catálogo de códigos de campo
│   ├── 04_config_simulacion.R      # Horizontes y parámetros de simulación
│   └── 05_config_programa_cortas.R # Reglas del programa de cortas
│
├── core/              # Modelos poblacionales
│   ├── 10_modelos_crecimiento.R    # Chapman-Richards por especie
│   ├── 11_modelo_mortalidad.R      # Probabilidad de muerte por clase diamétrica
│   ├── 12_modelo_reclutamiento.R   # Ingreso de nuevos individuos
│   ├── 13_simulador_crecimiento.R  # Motor de simulación año a año
│   ├── 14_optimizador_cortas.R     # Algoritmo ICA-Liocourt
│   ├── 15_core_calculos.R          # Funciones puras de cálculo
│   └── 16_calcular_ica.R           # Estimación del ICA biológico
│
├── analisis/          # Análisis descriptivo y estadístico
│   ├── 20_analisis_descriptivo.R
│   ├── 21_ANALISIS_RESULTADOS_DETALLADO.R
│   ├── 31_stat x rodal.R
│   ├── Fichas.R                    # Catálogo PDF por sitio (129 pp, 58 sitios)
│   └── ANÁLISIS DE REGENERACIÓN NATURAL.R
│
├── simulaciones/      # Simulación 10 años con/sin manejo
│   └── 30_SIMULACION_10AÑOS_COMPLETA.R
│
├── generadores/       # Tablas y gráficos para el PMF
│   ├── 50_GENERADOR_TABLAS.R       # Tablas 5-9 NOM-152 + ICA
│   ├── 51_GENERADOR_GRAFICOS.R     # Distribuciones diamétricas por UMM
│   ├── 52_CALCULOS_ESPECIFICOS.R
│   ├── 53_TABLA_DENSIDAD_ESPECIES.R
│   └── ...
│
├── utils/             # Funciones de apoyo compartidas
│   ├── io.R
│   ├── utils_metricas.R
│   └── utils_validacion.R
│
├── workflows/         # Puntos de entrada principales
│   ├── 40_WORKFLOW_COMPLETO.R      # Ejecutar todo el pipeline
│   └── 41_WORKFLOW_calcular_ica.r  # Solo calcular ICA
│
├── tests/             # Pruebas de integración y verificación
│   ├── 22_VERIFICACION_TABLAS_LATEX.R
│   └── test_*.R
│
├── opcional/          # Módulos opcionales
│   └── 23_Main_incendio.R          # Riesgo de incendio (combustibles)
│
└── inventario_forestal.xlsx        # Datos de ejemplo (58 sitios, UMM Las Alazanas)
```

## Requisitos

- R >= 4.0
- Paquetes: `tidyverse`, `readxl`, `janitor`, `xtable`, `patchwork`

```r
install.packages(c("tidyverse", "readxl", "janitor", "xtable", "patchwork"))
```

## Uso rápido

```r
# Clonar y ejecutar el workflow completo
setwd("sierraforv2/")
source("workflows/40_WORKFLOW_COMPLETO.R")
```

El workflow completo tiene un **control de fases** en la cabecera del script que permite activar/desactivar etapas individuales (`TRUE`/`FALSE`) para acelerar iteraciones durante el análisis:

```r
FASES <- list(
  importar    = TRUE,   # Leer Excel → .rds             ~10s
  descriptivo = TRUE,   # Análisis dasométrico           ~30s
  ica         = TRUE,   # Simulación sin cortas → ICA   ~60s
  simulacion  = TRUE,   # Simulación con cortas → PMF   ~90s
  tablas      = TRUE,   # Tablas LaTeX NOM-152           ~30s
  graficos    = TRUE,   # Gráficos distribución          ~15s
  incendio    = FALSE,  # Riesgo de incendio (opcional)
  fichas      = FALSE   # Fichas PDF por sitio (opcional)
)
```

## Datos de entrada

El inventario forestal se provee en `inventario_forestal.xlsx` con la estructura de campo:

| Campo | Descripción |
|-------|-------------|
| `sitio` | Identificador del sitio de muestreo |
| `umm` | Unidad de Manejo y Muestreo |
| `rodal` | Rodal dentro de la UMM |
| `especie` | Código de especie (Pinus/Quercus) |
| `dap` | Diámetro a la altura del pecho (cm) |
| `altura` | Altura total (m) |
| `dominancia` | Código de posición sociológica (1-9; 7-9 = muertos) |

## Outputs

| Tipo | Descripción |
|------|-------------|
| Tablas LaTeX | 7 tablas NOM-152 listas para integrar al PMF |
| Gráficos PDF | Distribuciones diamétricas iniciales, finales y de cortas por UMM |
| CSV | Trayectoria simulada año a año, ICA por especie y rodal |
| RDS | Inventario limpio (`arboles_analisis.rds`) para reutilización |

## Metodología

### Modelos de crecimiento

El crecimiento individual se modela en dos componentes:

**Diámetro** — tasa base anual por género × modificador de posición sociológica (dominancia):

| Posición | Factor |
|----------|--------|
| Dominante | 1.20 |
| Codominante | 1.00 |
| Intermedio | 0.70 |
| Suprimido | 0.40 |

**Altura** — tasa de incremento anual fija por género, modulada por posición sociológica y clase diamétrica:

| Género | Tasa base (m/año) | Altura máxima |
|--------|-------------------|---------------|
| *Pinus* | 0.20 | 22 m |
| *Quercus* | 0.15 | 17 m |

El modificador por clase diamétrica reduce el incremento conforme el árbol madura (árboles jóvenes < 20 cm: ×1.3; medianos 20-40 cm: ×1.0; grandes > 40 cm: ×0.8). Estos valores se definen en `config/02_config_especies.R` y `core/10_modelos_crecimiento.R`.

### ICA biológico

El Incremento Corriente Anual se estima como la diferencia de volumen entre el estado inicial y el estado proyectado a 1 año bajo crecimiento natural (sin mortalidad ni reclutamiento), agregado por clase diamétrica y rodal.

### Método de cortas ICA-Liocourt

El algoritmo combina un criterio de sustentabilidad (ICA) con uno de estructura (Liocourt) y se configura a través de dos módulos independientes:

**`config/05_config_programa_cortas.R`** — define todos los parámetros de manejo:
- `PROGRAMA_CORTAS`: tabla con el año de intervención, intensidad (% del ICA), y protección de maduros por UMM
- `DMC`: diámetro mínimo de corta por género (Pinus ≥ 20 cm, Quercus ≥ 2 cm)
- `D_MADUREZ`: umbral de madurez (Pinus 55 cm, Quercus 45 cm) — árboles por encima pueden protegerse con `proteger_maduros = TRUE`
- `Q_FACTOR` y `N_REF_LIOCOURT_POR_UMM`: factor de la distribución ideal y densidad objetivo en la clase de referencia (40-45 cm), ajustable por UMM
- `proporcion_quercus`: proporción objetivo de Quercus en el volumen extraído (control de composición)

**`core/14_optimizador_cortas.R`** — ejecuta el algoritmo en tres pasos:
1. **Volumen objetivo** = ICA anual × 10 años × intensidad (%) — garantiza que la extracción no supere la posibilidad biológica
2. **Árboles maduros** — se intervienen primero (si no están protegidos), priorizando la apertura del dosel a la regeneración
3. **Selección Liocourt** — para el volumen restante, cada árbol recibe una "distancia a la curva ideal"; se cortan primero los de mayor excedente (distancia > 0), avanzando hacia clases con sobredensidad hasta alcanzar el volumen objetivo

### Cumplimiento NOM-152-SEMARNAT-2023

Genera automáticamente:
- Tabla 5: Posibilidad anual por especie y rodal
- Tabla 6: Programa cronológico de cortas
- Tabla 7: Densidad e incrementos por clase diamétrica
- Tabla 8-9: Posibilidad e infraestructura
- Tabla ICA: Incremento corriente anual detallado
- Distribución de productos por género (*Pinus*/*Quercus*)
- Distribución diamétrica de cortas por UMM

## Contexto de aplicación

Desarrollado para el ejido **Las Alazanas**, municipio de Iturbide, Nuevo León, México. PMF vigente 2026-2036. Los datos de ejemplo incluidos corresponden a 58 sitios de muestreo en bosque de pino-encino de la Sierra Madre Oriental.

## Autor

**Dr. Fabien Charbonnier**  
Facultad de Ciencias Forestales — Universidad Autónoma de Nuevo León (UANL)  
fabien.charbonnierx@uanl.edu.mx

## Licencia

GNU General Public License v3.0 — ver [LICENSE](LICENSE) para detalles.
