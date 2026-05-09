# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MOTOR GENÉRICO DE GENERACIÓN DE TABLAS - SIERRAFOR
# ==============================================================================
#
# Este módulo proporciona funciones genéricas para crear, formatear y exportar
# tablas sin duplicación de código.
#
# Funciones principales:
#   - generar_tabla(): Crear objeto tabla con datos formateados
#   - formatear_tabla(): Aplicar formateo a columnas
#   - exportar_tabla_latex(): Exportar a formato LaTeX
#   - exportar_tabla_csv(): Exportar a formato CSV
#
# Uso:
#   source(file.path(PROYECTO_ROOT, "generadores/50_GENERADOR_TABLAS.R"))
#   tabla <- generar_tabla(datos, id, titulo, formato_columnas)
#   exportar_tabla_latex(tabla)
#
# ==============================================================================

library(tidyverse)
library(xtable)

# ==============================================================================
# CLASE: tabla_sierrafor
# ==============================================================================

#' Crear objeto de clase tabla_sierrafor
#'
#' @param datos Data frame con los datos de la tabla
#' @param id ID único de la tabla (ej: "T1_1_resumen_predio")
#' @param titulo Título de la tabla
#' @param subtitulo Subtítulo opcional
#' @param notas Notas al pie opcionales
#' @return Objeto de clase tabla_sierrafor
crear_tabla_sierrafor <- function(datos, id, titulo, 
                                  subtitulo = NULL, 
                                  notas = NULL) {
  
  estructura <- list(
    id = id,
    titulo = titulo,
    subtitulo = subtitulo,
    notas = notas,
    datos = datos,
    n_filas = nrow(datos),
    n_columnas = ncol(datos),
    nombres_columnas = names(datos),
    timestamp = Sys.time()
  )
  
  class(estructura) <- c("tabla_sierrafor", "list")
  
  return(estructura)
}

#' Imprimir tabla_sierrafor
#'
#' @param x Objeto tabla_sierrafor
#' @param ... Argumentos adicionales
print.tabla_sierrafor <- function(x, ...) {
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat(sprintf("TABLA: %s\n", x$id))
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("Título: %s\n", x$titulo))
  if (!is.null(x$subtitulo)) {
    cat(sprintf("Subtítulo: %s\n", x$subtitulo))
  }
  cat(sprintf("Dimensiones: %d filas × %d columnas\n", x$n_filas, x$n_columnas))
  cat(sprintf("Timestamp: %s\n", format(x$timestamp, "%Y-%m-%d %H:%M:%S")))
  cat("───────────────────────────────────────────────────────────\n")
  print(x$datos, n = 10)
  cat("═══════════════════════════════════════════════════════════\n\n")
}

# ==============================================================================
# FORMATEO DE COLUMNAS
# ==============================================================================

#' Formatear valor según tipo especificado
#'
#' @param x Vector numérico o character
#' @param tipo Tipo de formato: "entero", "decimal1", "decimal2", "decimal3",
#'             "porcentaje", "cientifico", "texto"
#' @return Vector character formateado
formatear_valor <- function(x, tipo = "decimal2") {
  
  # Manejar NAs
  if (all(is.na(x))) {
    return(rep("—", length(x)))
  }
  
  # Si ya es character, retornar tal cual
  if (is.character(x)) {
    return(x)
  }
  
  # Formatear según tipo
  resultado <- case_when(
    tipo == "entero" ~ sprintf("%.0f", x),
    tipo == "decimal1" ~ sprintf("%.1f", x),
    tipo == "decimal2" ~ sprintf("%.2f", x),
    tipo == "decimal3" ~ sprintf("%.3f", x),
    tipo == "porcentaje" ~ sprintf("%.1f%%", x),
    tipo == "porcentaje2" ~ sprintf("%.2f%%", x),
    tipo == "cientifico" ~ sprintf("%.2e", x),
    tipo == "texto" ~ as.character(x),
    TRUE ~ as.character(x)
  )
  
  # Reemplazar NAs con guión
  resultado[is.na(x)] <- "—"
  
  return(resultado)
}

#' Formatear múltiples columnas de un data frame
#'
#' @param datos Data frame
#' @param formato_columnas Lista nombrada con formato por columna
#'        Ejemplo: list(n_arboles = "entero", volumen = "decimal2")
#' @return Data frame con columnas formateadas como character
formatear_columnas <- function(datos, formato_columnas = NULL) {
  
  if (is.null(formato_columnas)) {
    return(datos)
  }
  
  datos_formateados <- datos
  
  for (col_name in names(formato_columnas)) {
    
    if (!col_name %in% names(datos)) {
      warning(sprintf("Columna '%s' no existe en datos", col_name))
      next
    }
    
    tipo <- formato_columnas[[col_name]]
    
    datos_formateados[[col_name]] <- formatear_valor(
      datos[[col_name]], 
      tipo = tipo
    )
  }
  
  return(datos_formateados)
}

#' Formatear automáticamente según tipo de columna
#'
#' @param datos Data frame
#' @param auto_detectar Si TRUE, detecta automáticamente tipos numéricos
#' @return Data frame formateado
formatear_automatico <- function(datos, auto_detectar = TRUE) {
  
  if (!auto_detectar) {
    return(datos)
  }
  
  datos_formateados <- datos
  
  for (col in names(datos)) {
    
    # Skip si ya es character
    if (is.character(datos[[col]])) next
    
    # Detectar tipo
    if (is.numeric(datos[[col]])) {
      
      # Si todos son enteros
      if (all(datos[[col]] == floor(datos[[col]]), na.rm = TRUE)) {
        datos_formateados[[col]] <- formatear_valor(datos[[col]], "entero")
      } else {
        # Por defecto 2 decimales
        datos_formateados[[col]] <- formatear_valor(datos[[col]], "decimal2")
      }
    }
  }
  
  return(datos_formateados)
}

# ==============================================================================
# OPERACIONES CON TABLAS
# ==============================================================================

#' Agregar fila de totales
#'
#' @param datos Data frame
#' @param columnas_sumar Vector con nombres de columnas a sumar
#' @param etiqueta Etiqueta para la fila de totales (default: "TOTAL")
#' @param columna_etiqueta Nombre de la columna donde poner la etiqueta
#' @return Data frame con fila de totales agregada
agregar_fila_totales <- function(datos, 
                                 columnas_sumar,
                                 etiqueta = "TOTAL",
                                 columna_etiqueta = NULL) {
  
  # Si no se especifica columna_etiqueta, usar la primera
  if (is.null(columna_etiqueta)) {
    columna_etiqueta <- names(datos)[1]
  }
  
  # CRÍTICO: Convertir columna_etiqueta a character para permitir texto
  # Esto evita error "Can't combine <double> and <character>"
  if (is.numeric(datos[[columna_etiqueta]])) {
    datos[[columna_etiqueta]] <- as.character(datos[[columna_etiqueta]])
  }
  
  # Crear fila de totales
  fila_total <- datos %>%
    summarise(across(all_of(columnas_sumar), ~ sum(.x, na.rm = TRUE)))
  
  # Agregar etiqueta
  fila_total[[columna_etiqueta]] <- etiqueta
  
  # Completar columnas faltantes con NA o ""
  for (col in names(datos)) {
    if (!col %in% names(fila_total)) {
      fila_total[[col]] <- NA
    }
  }
  
  # Reordenar columnas para que coincidan
  fila_total <- fila_total[, names(datos)]
  
  # Agregar al final
  datos_con_total <- bind_rows(datos, fila_total)
  
  return(datos_con_total)
}

#' Agregar columna de porcentajes
#'
#' @param datos Data frame
#' @param columna_valor Nombre de la columna con valores absolutos
#' @param nombre_nueva_columna Nombre para la columna de porcentajes
#' @param sufijo Sufijo a agregar (default: "_pct")
#' @return Data frame con columna de porcentaje agregada
agregar_columna_porcentaje <- function(datos, 
                                       columna_valor,
                                       nombre_nueva_columna = NULL,
                                       sufijo = "_pct") {
  
  if (is.null(nombre_nueva_columna)) {
    nombre_nueva_columna <- paste0(columna_valor, sufijo)
  }
  
  total <- sum(datos[[columna_valor]], na.rm = TRUE)
  
  datos[[nombre_nueva_columna]] <- (datos[[columna_valor]] / total) * 100
  
  return(datos)
}

#' Redondear columnas numéricas
#'
#' @param datos Data frame
#' @param decimales Número de decimales (default: 2)
#' @param columnas Vector con nombres de columnas a redondear (NULL = todas)
#' @return Data frame con columnas redondeadas
redondear_columnas <- function(datos, decimales = 2, columnas = NULL) {
  
  if (is.null(columnas)) {
    # Detectar columnas numéricas
    columnas <- names(datos)[sapply(datos, is.numeric)]
  }
  
  datos <- datos %>%
    mutate(across(all_of(columnas), ~ round(.x, decimales)))
  
  return(datos)
}

# ==============================================================================
# FUNCIÓN PRINCIPAL: GENERAR TABLA
# ==============================================================================

#' Generar tabla con formato estándar SIERRAFOR
#'
#' @param datos Data frame con los datos
#' @param id ID único de la tabla
#' @param titulo Título de la tabla
#' @param subtitulo Subtítulo opcional
#' @param notas Notas al pie opcionales
#' @param columnas Vector con nombres de columnas a incluir (NULL = todas)
#' @param formato_columnas Lista nombrada con formato por columna
#' @param ordenar_por Vector con nombres de columnas para ordenar
#' @param ordenar_desc Logical, ordenar descendente (default: FALSE)
#' @param agregar_totales Logical, agregar fila de totales
#' @param columnas_totales Vector con columnas a sumar en totales
#' @param formato_auto Logical, formatear automáticamente (default: TRUE)
#' @return Objeto tabla_sierrafor
generar_tabla <- function(datos,
                          id,
                          titulo,
                          subtitulo = NULL,
                          notas = NULL,
                          columnas = NULL,
                          formato_columnas = NULL,
                          ordenar_por = NULL,
                          ordenar_desc = FALSE,
                          agregar_totales = FALSE,
                          columnas_totales = NULL,
                          formato_auto = TRUE) {
  
  # Validación
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame")
  }
  
  if (nrow(datos) == 0) {
    warning(sprintf("Tabla %s: datos vacíos", id))
  }
  
  # Seleccionar columnas
  if (!is.null(columnas)) {
    columnas_faltantes <- setdiff(columnas, names(datos))
    if (length(columnas_faltantes) > 0) {
      warning(sprintf("Columnas no encontradas: %s", 
                      paste(columnas_faltantes, collapse = ", ")))
      columnas <- intersect(columnas, names(datos))
    }
    datos <- datos %>% select(all_of(columnas))
  }
  
  # Ordenar
  if (!is.null(ordenar_por)) {
    if (ordenar_desc) {
      datos <- datos %>% arrange(desc(across(all_of(ordenar_por))))
    } else {
      datos <- datos %>% arrange(across(all_of(ordenar_por)))
    }
  }
  
  # Agregar totales ANTES de formatear
  if (agregar_totales) {
    if (is.null(columnas_totales)) {
      # Por defecto, sumar todas las columnas numéricas
      columnas_totales <- names(datos)[sapply(datos, is.numeric)]
    }
    datos <- agregar_fila_totales(datos, columnas_totales)
  }
  
  # Formatear columnas
  if (!is.null(formato_columnas)) {
    datos <- formatear_columnas(datos, formato_columnas)
  } else if (formato_auto) {
    datos <- formatear_automatico(datos)
  }
  
  # Crear objeto tabla
  tabla <- crear_tabla_sierrafor(
    datos = datos,
    id = id,
    titulo = titulo,
    subtitulo = subtitulo,
    notas = notas
  )
  
  return(tabla)
}

# ==============================================================================
# EXPORTACIÓN: LaTeX
# ==============================================================================

#' Exportar tabla a formato LaTeX
#'
#' @param tabla Objeto tabla_sierrafor
#' @param directorio Directorio de salida (default: "tablas_latex")
#' @param incluir_rownames Incluir nombres de filas (default: FALSE)
#' @param booktabs Usar estilo booktabs (default: TRUE)
#' @param floating Tabla flotante (default: TRUE)
#' @param caption_placement Posición del caption (default: "top")
#' @param size Tamaño de fuente LaTeX (default: NULL)
#' @param landscape Orientación horizontal (default: FALSE)
#' @param config Configuración global (default: REPORTE_CONFIG)
#' @return Path del archivo generado (invisible)
exportar_tabla_latex <- function(tabla,
                                 directorio = NULL,
                                 incluir_rownames = FALSE,
                                 booktabs = TRUE,
                                 floating = TRUE,
                                 caption_placement = "top",
                                 size = NULL,
                                 landscape = FALSE,
                                 config = NULL) {
  
  # Verificar que es tabla_sierrafor
  if (!inherits(tabla, "tabla_sierrafor")) {
    stop("Objeto debe ser de clase tabla_sierrafor")
  }
  
  # Usar configuración global si está disponible
  if (is.null(config) && exists("REPORTE_CONFIG")) {
    config <- REPORTE_CONFIG
  }
  
  # Directorio
  if (is.null(directorio)) {
    if (!is.null(config)) {
      directorio <- config$dir_tablas_latex
    } else {
      directorio <- "tablas_latex"
    }
  }
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Construir caption
  caption_completo <- tabla$titulo
  
  if (!is.null(tabla$subtitulo)) {
    caption_completo <- paste0(caption_completo, ". ", tabla$subtitulo)
  }
  
  if (!is.null(tabla$notas)) {
    caption_completo <- paste0(caption_completo, ". ", tabla$notas)
  }
  
  # Crear objeto xtable
  xt <- xtable(
    tabla$datos,
    caption = caption_completo,
    label = paste0("tab:", tabla$id)
  )
  
  # Construir path de salida
  archivo <- file.path(directorio, paste0(tabla$id, ".tex"))
  
  # Preparar argumentos para print.xtable
  args_print <- list(
    x = xt,
    file = archivo,
    include.rownames = incluir_rownames,
    floating = floating,
    booktabs = booktabs,
    caption.placement = caption_placement,
    sanitize.text.function = function(x) x  # No sanitizar, ya viene formateado
  )
  
  # Agregar size si se especifica
  if (!is.null(size)) {
    args_print$size <- size
  }
  
  # Tabla en landscape
  if (landscape) {
    args_print$table.placement <- "!htbp"
    args_print$scalebox <- 0.9
  }
  
  # Exportar
  do.call(print.xtable, args_print)
  
  # Mensaje
  if (!is.null(config) && config$mostrar_progreso) {
    cat(sprintf("  ✓ LaTeX: %s\n", archivo))
  }
  
  return(invisible(archivo))
}

# ==============================================================================
# EXPORTACIÓN: CSV
# ==============================================================================

#' Exportar tabla a formato CSV
#'
#' @param tabla Objeto tabla_sierrafor
#' @param directorio Directorio de salida (default: "resultados")
#' @param config Configuración global (default: REPORTE_CONFIG)
#' @return Path del archivo generado (invisible)
exportar_tabla_csv <- function(tabla,
                               directorio = NULL,
                               config = NULL) {
  
  # Verificar que es tabla_sierrafor
  if (!inherits(tabla, "tabla_sierrafor")) {
    stop("Objeto debe ser de clase tabla_sierrafor")
  }
  
  # Usar configuración global si está disponible
  if (is.null(config) && exists("REPORTE_CONFIG")) {
    config <- REPORTE_CONFIG
  }
  
  # Directorio
  if (is.null(directorio)) {
    if (!is.null(config)) {
      directorio <- config$dir_csv
    } else {
      directorio <- "resultados"
    }
  }
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Construir path
  archivo <- file.path(directorio, paste0(tabla$id, ".csv"))
  
  # Exportar
  write_csv(tabla$datos, archivo)
  
  # Mensaje
  if (!is.null(config) && config$mostrar_progreso) {
    cat(sprintf("  ✓ CSV:   %s\n", archivo))
  }
  
  return(invisible(archivo))
}

# ==============================================================================
# EXPORTACIÓN MÚLTIPLE
# ==============================================================================

#' Exportar tabla en múltiples formatos
#'
#' @param tabla Objeto tabla_sierrafor
#' @param latex Exportar LaTeX (default: TRUE)
#' @param csv Exportar CSV (default: TRUE)
#' @param config Configuración global
#' @return Lista con paths generados
exportar_tabla <- function(tabla,
                           latex = TRUE,
                           csv = TRUE,
                           config = NULL) {
  
  if (is.null(config) && exists("REPORTE_CONFIG")) {
    config <- REPORTE_CONFIG
  }
  
  archivos <- list()
  
  if (latex) {
    archivos$latex <- exportar_tabla_latex(tabla, config = config)
  }
  
  if (csv) {
    archivos$csv <- exportar_tabla_csv(tabla, config = config)
  }
  
  return(invisible(archivos))
}

# ==============================================================================
# UTILIDADES
# ==============================================================================

#' Cambiar nombres de columnas a español
#'
#' @param datos Data frame
#' @param diccionario Lista nombrada con traducciones
#' @return Data frame con columnas renombradas
renombrar_columnas_espanol <- function(datos, diccionario = NULL) {
  
  if (is.null(diccionario)) {
    # Diccionario por defecto
    diccionario <- c(
      "rodal" = "Rodal",
      "genero_grupo" = "Género",
      "nombre_cientifico" = "Especie",
      "n_arboles" = "N árboles",
      "n_ha" = "N/ha",
      "ab_ha" = "AB (m²/ha)",
      "vol_ha" = "Vol (m³/ha)",
      "d_medio" = "Ø medio (cm)",
      "h_media" = "Altura media (m)",
      "densidad_ha" = "Densidad (arb/ha)",
      "volumen_m3" = "Volumen (m³)",
      "area_basal" = "Área basal (m²)",
      "diametro_normal" = "Diámetro (cm)",
      "altura_total" = "Altura (m)",
      "clase_d" = "Clase diamétrica"
    )
  }
  
  # Renombrar solo las que existen
  nombres_actuales <- names(datos)
  nombres_nuevos <- nombres_actuales
  
  for (i in seq_along(nombres_actuales)) {
    if (nombres_actuales[i] %in% names(diccionario)) {
      nombres_nuevos[i] <- diccionario[[nombres_actuales[i]]]
    }
  }
  
  names(datos) <- nombres_nuevos
  
  return(datos)
}

#' Crear tabla con estadísticas descriptivas
#'
#' @param datos Data frame
#' @param columna Nombre de la columna numérica
#' @param grupo_por Vector con columnas para agrupar (opcional)
#' @return Data frame con estadísticas
tabla_estadisticas <- function(datos, columna, grupo_por = NULL) {
  
  if (!is.null(grupo_por)) {
    stats <- datos %>%
      group_by(across(all_of(grupo_por))) %>%
      summarise(
        n = n(),
        media = mean(.data[[columna]], na.rm = TRUE),
        sd = sd(.data[[columna]], na.rm = TRUE),
        min = min(.data[[columna]], na.rm = TRUE),
        q25 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
        mediana = median(.data[[columna]], na.rm = TRUE),
        q75 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
        max = max(.data[[columna]], na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    stats <- datos %>%
      summarise(
        n = n(),
        media = mean(.data[[columna]], na.rm = TRUE),
        sd = sd(.data[[columna]], na.rm = TRUE),
        min = min(.data[[columna]], na.rm = TRUE),
        q25 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
        mediana = median(.data[[columna]], na.rm = TRUE),
        q75 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
        max = max(.data[[columna]], na.rm = TRUE)
      )
  }
  
  return(stats)
}

# ==============================================================================
# INICIALIZACIÓN
# ==============================================================================

cat("\n✓ Motor de generación de tablas cargado\n")
cat("  Funciones principales:\n")
cat("    • generar_tabla()        - Crear tabla con formato\n")
cat("    • formatear_columnas()   - Formatear tipos de datos\n")
cat("    • exportar_tabla_latex() - Exportar a LaTeX\n")
cat("    • exportar_tabla_csv()   - Exportar a CSV\n")
cat("    • exportar_tabla()       - Exportar múltiples formatos\n\n")