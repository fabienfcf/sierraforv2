# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MOTOR GENÉRICO DE GENERACIÓN DE GRÁFICOS - SIERRAFOR
# ==============================================================================
#
# Este módulo proporciona funciones genéricas para crear y exportar gráficos
# con estilo visual consistente.
#
# Funciones principales:
#   - generar_grafico(): Crear gráfico con tipo especificado
#   - tema_sierrafor(): Tema ggplot2 estándar
#   - exportar_grafico(): Exportar en múltiples formatos
#
# Uso:
#   source(file.path(PROYECTO_ROOT, "generadores/51_GENERADOR_GRAFICOS.R"))
#   grafico <- generar_grafico(datos, id, titulo, tipo = "barras")
#   exportar_grafico(grafico)
#
# ==============================================================================

library(tidyverse)
library(scales)
library(patchwork)

# ==============================================================================
# PALETA DE COLORES ESTÁNDAR SIERRAFOR
# ==============================================================================

COLORES_SIERRAFOR <- list(
  # Géneros principales
  pinus = "#228B22",        # Verde bosque
  quercus = "#8B4513",      # Café madera
  otros = "#808080",        # Gris
  
  # Estados temporales
  inicial = "#4575b4",      # Azul
  final = "#d73027",        # Rojo
  proyeccion = "#fee08b",   # Amarillo
  
  # Procesos
  crecimiento = "#91cf60",  # Verde claro
  mortalidad = "#fc8d59",   # Naranja
  reclutamiento = "#91bfdb", # Azul claro
  corta = "#ffffbf",        # Amarillo pálido
  
  # Categorías
  bueno = "#1a9850",        # Verde oscuro
  regular = "#fee08b",      # Amarillo
  malo = "#d73027",         # Rojo
  
  # Escala continua divergente
  divergente_bajo = "#4575b4",
  divergente_medio = "#ffffbf",
  divergente_alto = "#d73027",
  
  # Escala secuencial
  secuencial = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", 
                 "#74c476", "#41ab5d", "#238b45", "#005a32")
)

#' Obtener paleta de colores por género
#'
#' @param generos Vector con géneros
#' @return Vector nombrado con colores
paleta_generos <- function(generos = c("Pinus", "Quercus", "Otros")) {
  colores <- c(
    "Pinus" = COLORES_SIERRAFOR$pinus,
    "Quercus" = COLORES_SIERRAFOR$quercus,
    "Otros" = COLORES_SIERRAFOR$otros
  )
  return(colores[generos])
}

# ==============================================================================
# TEMA VISUAL ESTÁNDAR SIERRAFOR
# ==============================================================================

#' Tema ggplot2 estándar para gráficos SIERRAFOR
#'
#' @param base_size Tamaño base de fuente (default: 11)
#' @param base_family Familia de fuente (default: "")
#' @param grid_major Color de grid mayor (default: "gray90")
#' @param grid_minor Mostrar grid menor (default: TRUE)
#' @return theme object
tema_sierrafor <- function(base_size = 11,
                           base_family = "",
                           grid_major = "gray90",
                           grid_minor = TRUE) {
  
  tema <- theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Títulos
      plot.title = element_text(
        face = "bold",
        size = base_size + 3,
        hjust = 0,
        margin = margin(b = base_size)
      ),
      plot.subtitle = element_text(
        color = "gray40",
        size = base_size,
        hjust = 0,
        margin = margin(b = base_size)
      ),
      plot.caption = element_text(
        color = "gray50",
        size = base_size - 2,
        hjust = 1,
        margin = margin(t = base_size)
      ),
      
      # Ejes
      axis.title = element_text(face = "bold", size = base_size),
      axis.title.x = element_text(margin = margin(t = base_size * 0.5)),
      axis.title.y = element_text(margin = margin(r = base_size * 0.5)),
      axis.text = element_text(size = base_size - 1),
      axis.line = element_line(color = "gray30", linewidth = 0.5),
      axis.ticks = element_line(color = "gray30", linewidth = 0.5),
      
      # Leyenda
      legend.position = "top",
      legend.title = element_text(face = "bold", size = base_size),
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(1.2, "lines"),
      legend.margin = margin(b = base_size),
      legend.box.spacing = unit(0.5, "lines"),
      
      # Panel
      panel.grid.major = element_line(color = grid_major, linewidth = 0.5),
      panel.grid.minor = if (grid_minor) {
        element_line(color = grid_major, linewidth = 0.25, linetype = "dotted")
      } else {
        element_blank()
      },
      panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
      panel.spacing = unit(1, "lines"),
      
      # Facets
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(face = "bold", size = base_size),
      
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  return(tema)
}

# ==============================================================================
# CLASE: grafico_sierrafor
# ==============================================================================

#' Crear objeto de clase grafico_sierrafor
#'
#' @param plot Objeto ggplot
#' @param id ID único del gráfico
#' @param titulo Título del gráfico
#' @param subtitulo Subtítulo opcional
#' @param tipo Tipo de gráfico
#' @return Objeto de clase grafico_sierrafor
crear_grafico_sierrafor <- function(plot, id, titulo, 
                                    subtitulo = NULL, 
                                    tipo = "generico") {
  
  estructura <- list(
    id = id,
    titulo = titulo,
    subtitulo = subtitulo,
    tipo = tipo,
    plot = plot,
    timestamp = Sys.time()
  )
  
  class(estructura) <- c("grafico_sierrafor", "list")
  
  return(estructura)
}

#' Imprimir grafico_sierrafor
#'
#' @param x Objeto grafico_sierrafor
#' @param ... Argumentos adicionales
print.grafico_sierrafor <- function(x, ...) {
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat(sprintf("GRÁFICO: %s\n", x$id))
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("Título: %s\n", x$titulo))
  if (!is.null(x$subtitulo)) {
    cat(sprintf("Subtítulo: %s\n", x$subtitulo))
  }
  cat(sprintf("Tipo: %s\n", x$tipo))
  cat(sprintf("Timestamp: %s\n", format(x$timestamp, "%Y-%m-%d %H:%M:%S")))
  cat("───────────────────────────────────────────────────────────\n\n")
  print(x$plot)
  cat("\n═══════════════════════════════════════════════════════════\n\n")
}

# ==============================================================================
# FUNCIONES AUXILIARES PARA CREAR DIFERENTES TIPOS DE GRÁFICOS
# ==============================================================================

#' Crear gráfico de barras
#'
#' @param datos Data frame
#' @param x Nombre columna eje X
#' @param y Nombre columna eje Y
#' @param fill Nombre columna para fill (opcional)
#' @param position Posición barras: "stack", "dodge", "fill"
#' @param orientacion "vertical" o "horizontal"
#' @return ggplot object
crear_barras <- function(datos, x, y, fill = NULL, 
                         position = "dodge", 
                         orientacion = "vertical") {
  
  p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  
  if (!is.null(fill)) {
    p <- p + 
      geom_col(aes(fill = .data[[fill]]), 
               position = position, 
               alpha = 0.85,
               width = 0.7)
    
    # Aplicar paleta si es género
    if (all(unique(datos[[fill]]) %in% c("Pinus", "Quercus", "Otros"))) {
      p <- p + scale_fill_manual(values = paleta_generos())
    }
  } else {
    p <- p + 
      geom_col(fill = COLORES_SIERRAFOR$pinus, alpha = 0.85, width = 0.7)
  }
  
  # Orientación horizontal
  if (orientacion == "horizontal") {
    p <- p + coord_flip()
  }
  
  return(p)
}

#' Crear gráfico de líneas
#'
#' @param datos Data frame
#' @param x Nombre columna eje X
#' @param y Nombre columna eje Y
#' @param color Nombre columna para color (opcional)
#' @param group Nombre columna para agrupar (opcional)
#' @param puntos Mostrar puntos (default: TRUE)
#' @return ggplot object
crear_lineas <- function(datos, x, y, color = NULL, group = NULL, 
                         puntos = TRUE) {
  
  if (!is.null(color)) {
    p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]], 
                           color = .data[[color]], 
                           group = .data[[color]]))
  } else if (!is.null(group)) {
    p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]], 
                           group = .data[[group]]))
  } else {
    p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  }
  
  p <- p + geom_line(linewidth = 1.2)
  
  if (puntos) {
    p <- p + geom_point(size = 2.5)
  }
  
  # Aplicar paleta si es género
  if (!is.null(color) && all(unique(datos[[color]]) %in% c("Pinus", "Quercus", "Otros"))) {
    p <- p + scale_color_manual(values = paleta_generos())
  }
  
  return(p)
}

#' Crear histograma
#'
#' @param datos Data frame
#' @param x Nombre columna para histograma
#' @param bins Número de bins (default: 30)
#' @param fill Nombre columna para fill (opcional)
#' @param position Posición: "stack", "dodge", "identity" (default: "stack" si hay fill)
#' @return ggplot object
crear_histograma <- function(datos, x, bins = 30, fill = NULL, 
                             position = NULL) {
  
  # Si no se especifica position y hay fill, usar "stack" por defecto
  if (is.null(position)) {
    position <- if (!is.null(fill)) "stack" else "identity"
  }
  
  p <- ggplot(datos, aes(x = .data[[x]]))
  
  if (!is.null(fill)) {
    p <- p + 
      geom_histogram(aes(fill = .data[[fill]]), 
                     bins = bins, 
                     position = position,
                     alpha = 0.85,
                     color = "white",  # Borde blanco para separar barras
                     linewidth = 0.3)
    
    if (all(unique(datos[[fill]]) %in% c("Pinus", "Quercus", "Otros"))) {
      p <- p + scale_fill_manual(values = paleta_generos())
    }
  } else {
    p <- p + 
      geom_histogram(bins = bins, 
                     fill = COLORES_SIERRAFOR$pinus, 
                     alpha = 0.85,
                     color = "white",
                     linewidth = 0.3)
  }
  
  return(p)
}

#' Crear gráfico de dispersión (scatter)
#'
#' @param datos Data frame
#' @param x Nombre columna eje X
#' @param y Nombre columna eje Y
#' @param color Nombre columna para color (opcional)
#' @param size Nombre columna para tamaño (opcional)
#' @param agregar_regresion Agregar línea de regresión (default: FALSE)
#' @return ggplot object
crear_scatter <- function(datos, x, y, color = NULL, size = NULL, 
                          agregar_regresion = FALSE) {
  
  p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  
  if (!is.null(color) && !is.null(size)) {
    p <- p + geom_point(aes(color = .data[[color]], size = .data[[size]]), 
                        alpha = 0.7)
  } else if (!is.null(color)) {
    p <- p + geom_point(aes(color = .data[[color]]), size = 2.5, alpha = 0.7)
  } else if (!is.null(size)) {
    p <- p + geom_point(aes(size = .data[[size]]), alpha = 0.7)
  } else {
    p <- p + geom_point(size = 2.5, alpha = 0.7, color = COLORES_SIERRAFOR$pinus)
  }
  
  # Regresión lineal
  if (agregar_regresion) {
    p <- p + 
      geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed")
  }
  
  return(p)
}

#' Crear boxplot
#'
#' @param datos Data frame
#' @param x Nombre columna categórica
#' @param y Nombre columna numérica
#' @param fill Nombre columna para fill (opcional)
#' @return ggplot object
crear_boxplot <- function(datos, x, y, fill = NULL) {
  
  if (!is.null(fill)) {
    p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]], 
                           fill = .data[[fill]]))
  } else {
    p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  }
  
  p <- p + 
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1)
  
  if (!is.null(fill) && all(unique(datos[[fill]]) %in% c("Pinus", "Quercus", "Otros"))) {
    p <- p + scale_fill_manual(values = paleta_generos())
  }
  
  return(p)
}

#' Crear gráfico con facets
#'
#' @param datos Data frame
#' @param plot_base Gráfico base (ggplot)
#' @param facet_by Vector con nombres de columnas para facets
#' @param ncol Número de columnas (opcional)
#' @param scales Escalas: "fixed", "free", "free_x", "free_y"
#' @return ggplot object
agregar_facets <- function(plot_base, facet_by, ncol = NULL, 
                           scales = "fixed") {
  
  if (length(facet_by) == 1) {
    formula_facet <- as.formula(paste("~", facet_by))
    plot_base <- plot_base + 
      facet_wrap(formula_facet, ncol = ncol, scales = scales)
  } else if (length(facet_by) == 2) {
    formula_facet <- as.formula(paste(facet_by[1], "~", facet_by[2]))
    plot_base <- plot_base + 
      facet_grid(formula_facet, scales = scales)
  }
  
  return(plot_base)
}

# ==============================================================================
# FUNCIÓN PRINCIPAL: GENERAR GRÁFICO
# ==============================================================================

#' Generar gráfico con estilo estándar SIERRAFOR
#'
#' @param datos Data frame con los datos
#' @param id ID único del gráfico
#' @param titulo Título del gráfico
#' @param subtitulo Subtítulo opcional
#' @param tipo Tipo: "barras", "lineas", "histograma", "scatter", "boxplot"
#' @param x Nombre columna eje X
#' @param y Nombre columna eje Y (no requerido para histograma)
#' @param fill Nombre columna para fill
#' @param color Nombre columna para color
#' @param size Nombre columna para tamaño puntos
#' @param facet_by Vector con columnas para facets
#' @param labs_extra Lista con labs adicionales (xlab, ylab, caption)
#' @param config_extra Lista con configuración adicional
#' @param aplicar_tema Aplicar tema_sierrafor (default: TRUE)
#' @return Objeto grafico_sierrafor
generar_grafico <- function(datos,
                            id,
                            titulo,
                            subtitulo = NULL,
                            tipo = "barras",
                            x = NULL,
                            y = NULL,
                            fill = NULL,
                            color = NULL,
                            size = NULL,
                            facet_by = NULL,
                            labs_extra = list(),
                            config_extra = list(),
                            aplicar_tema = TRUE) {
  
  # Validación
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame")
  }
  
  if (nrow(datos) == 0) {
    warning(sprintf("Gráfico %s: datos vacíos", id))
  }
  
  # Crear gráfico base según tipo
  p <- switch(tipo,
              "barras" = crear_barras(
                datos, x, y, fill,
                position = config_extra$position %||% "dodge",
                orientacion = config_extra$orientacion %||% "vertical"
              ),
              
              "lineas" = crear_lineas(
                datos, x, y, color,
                group = config_extra$group,
                puntos = config_extra$puntos %||% TRUE
              ),
              
              "histograma" = crear_histograma(
                datos, x,
                bins = config_extra$bins %||% 30,
                fill = fill,
                position = config_extra$position %||% "identity"
              ),
              
              "scatter" = crear_scatter(
                datos, x, y, color, size,
                agregar_regresion = config_extra$regresion %||% FALSE
              ),
              
              "boxplot" = crear_boxplot(datos, x, y, fill),
              
              stop(sprintf("Tipo de gráfico '%s' no reconocido", tipo))
  )
  
  # Agregar facets si se especifican
  if (!is.null(facet_by)) {
    p <- agregar_facets(
      p, facet_by,
      ncol = config_extra$facet_ncol,
      scales = config_extra$facet_scales %||% "fixed"
    )
  }
  
  # Inferir labels con unidades automáticamente
  xlab_auto <- if (!is.null(x)) inferir_label_con_unidades(x) else NULL
  ylab_auto <- if (!is.null(y)) inferir_label_con_unidades(y) else NULL
  
  # Usar labels personalizados si se proporcionan, sino usar automáticos
  xlab_final <- labs_extra$xlab %||% xlab_auto %||% x
  ylab_final <- labs_extra$ylab %||% ylab_auto %||% y
  
  # Agregar títulos
  p <- p +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = xlab_final,
      y = ylab_final,
      caption = labs_extra$caption
    )
  
  # Aplicar tema
  if (aplicar_tema) {
    p <- p + tema_sierrafor()
  }
  
  # Configuraciones adicionales
  if (!is.null(config_extra$escala_y_log)) {
    p <- p + scale_y_log10()
  }
  
  if (!is.null(config_extra$limites_y)) {
    p <- p + ylim(config_extra$limites_y)
  }
  
  if (!is.null(config_extra$limites_x)) {
    p <- p + xlim(config_extra$limites_x)
  }
  
  # Formato etiquetas eje Y
  if (!is.null(config_extra$formato_y)) {
    p <- p + scale_y_continuous(labels = config_extra$formato_y)
  }
  
  # Crear objeto grafico_sierrafor
  grafico <- crear_grafico_sierrafor(
    plot = p,
    id = id,
    titulo = titulo,
    subtitulo = subtitulo,
    tipo = tipo
  )
  
  return(grafico)
}

# ==============================================================================
# EXPORTACIÓN
# ==============================================================================

#' Exportar gráfico en formato especificado
#'
#' @param grafico Objeto grafico_sierrafor
#' @param directorio Directorio de salida (default: "graficos")
#' @param formato Formato: "png", "pdf", "svg", "jpg"
#' @param ancho Ancho en pulgadas (default: 10)
#' @param alto Alto en pulgadas (default: 8)
#' @param dpi DPI para formatos raster (default: 300)
#' @param config Configuración global (default: REPORTE_CONFIG)
#' @return Path del archivo generado (invisible)
exportar_grafico <- function(grafico,
                             directorio = NULL,
                             formato = NULL,
                             ancho = NULL,
                             alto = NULL,
                             dpi = NULL,
                             config = NULL) {
  
  # Verificar que es grafico_sierrafor
  if (!inherits(grafico, "grafico_sierrafor")) {
    stop("Objeto debe ser de clase grafico_sierrafor")
  }
  
  # Usar configuración global si está disponible
  if (is.null(config) && exists("REPORTE_CONFIG")) {
    config <- REPORTE_CONFIG
  }
  
  # Directorio
  if (is.null(directorio)) {
    if (!is.null(config)) {
      directorio <- config$dir_graficos
    } else {
      directorio <- "graficos"
    }
  }
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Parámetros por defecto
  if (is.null(formato)) {
    formato <- if (!is.null(config)) config$formato_graficos else "png"
  }
  
  if (is.null(ancho)) {
    ancho <- if (!is.null(config)) config$ancho_default_in else 10
  }
  
  if (is.null(alto)) {
    alto <- if (!is.null(config)) config$alto_default_in else 8
  }
  
  if (is.null(dpi)) {
    dpi <- if (!is.null(config)) config$dpi_graficos else 300
  }
  
  # Construir path de salida
  archivo <- file.path(directorio, paste0(grafico$id, ".", formato))
  
  # Exportar según formato
  ggsave(
    filename = archivo,
    plot = grafico$plot,
    width = ancho,
    height = alto,
    dpi = dpi,
    device = formato
  )
  
  # Mensaje
  if (!is.null(config) && config$mostrar_progreso) {
    cat(sprintf("  ✓ Gráfico: %s\n", archivo))
  }
  
  return(invisible(archivo))
}

# ==============================================================================
# UTILIDADES ADICIONALES
# ==============================================================================

#' Inferir label con unidades automáticamente
#'
#' @param nombre_columna Nombre de la columna
#' @return Label con unidades
inferir_label_con_unidades <- function(nombre_columna) {
  
  # Diccionario de labels comunes con unidades
  labels_dict <- c(
    # Volumen
    "volumen" = "Volumen (m³)",
    "vol_ha" = "Volumen (m³/ha)",
    "vol_ha_m3" = "Volumen (m³/ha)",
    "volumen_m3" = "Volumen (m³)",
    "vol_total" = "Volumen total (m³)",
    
    # Área basal
    "ab_ha" = "Área basal (m²/ha)",
    "ab_ha_m2" = "Área basal (m²/ha)",
    "area_basal" = "Área basal (m²)",
    "ab_total" = "Área basal total (m²)",
    
    # Diámetro
    "diametro" = "Diámetro (cm)",
    "d_medio" = "Diámetro medio (cm)",
    "dmc" = "DMC (cm)",
    "d_medio_cm" = "Diámetro medio (cm)",
    "diametro_normal" = "Diámetro normal (cm)",
    "clase_d" = "Clase diamétrica (cm)",
    
    # Altura
    "altura" = "Altura (m)",
    "h_media" = "Altura media (m)",
    "h_media_m" = "Altura media (m)",
    "altura_total" = "Altura total (m)",
    
    # Densidad
    "n_ha" = "Densidad (árb/ha)",
    "densidad_ha" = "Densidad (árb/ha)",
    "n_arboles" = "N° árboles",
    
    # Temporal
    "año" = "Año",
    "año_simulacion" = "Año de simulación",
    
    # Incrementos
    "ica" = "ICA (m³/ha/año)",
    "ima" = "IMA (m³/ha/año)",
    "incremento" = "Incremento",
    
    # Otros
    "rodal" = "Rodal",
    "genero" = "Género",
    "genero_grupo" = "Género",
    "especie" = "Especie",
    "nombre_cientifico" = "Especie",
    "pct" = "Porcentaje (%)",
    "porcentaje" = "Porcentaje (%)",
    "intensidad" = "Intensidad (%)"
  )
  
  # Buscar en diccionario
  if (nombre_columna %in% names(labels_dict)) {
    return(labels_dict[[nombre_columna]])
  }
  
  # Si no está en diccionario, intentar capitalizar
  label <- gsub("_", " ", nombre_columna)
  label <- tools::toTitleCase(label)
  
  return(label)
}

#' Operador %||% para valores por defecto
#'
#' @param a Valor a evaluar
#' @param b Valor por defecto si a es NULL
#' @return a si no es NULL, b en caso contrario
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# ==============================================================================
# INICIALIZACIÓN
# ==============================================================================

cat("\n✓ Motor de generación de gráficos cargado\n")
cat("  Funciones principales:\n")
cat("    • generar_grafico()        - Crear gráfico con tipo\n")
cat("    • tema_sierrafor()         - Tema visual estándar\n")
cat("    • exportar_grafico()       - Exportar en formato\n")
cat("    • inferir_label_con_unidades() - Unidades automáticas\n")
cat("  Tipos soportados:\n")
cat("    • barras, lineas, histograma, scatter, boxplot\n")
cat("  Paleta de colores:\n")
cat("    • Pinus (verde), Quercus (café), Otros (gris)\n")
cat("  Características:\n")
cat("    • Histograma APILADO por defecto\n")
cat("    • Unidades automáticas en ejes\n\n")