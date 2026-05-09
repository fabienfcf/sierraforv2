# ==============================================================================
# UTILS: INPUT/OUTPUT
# Funciones compartidas para lectura/escritura de archivos
# ==============================================================================

library(tidyverse)

# ==============================================================================
# FORMATEO DE NÚMEROS (VECTORIZADO)
# ==============================================================================

#' Formatear número con 3 cifras significativas (VECTORIZADO)
#'
#' @param x Valor numérico o vector a formatear
#' @return String o vector de strings con formato de 3 cifras significativas
formatear_3_cifras_sig <- function(x) {
  # Inicializar resultado
  result <- character(length(x))
  
  # Manejar NA
  na_idx <- is.na(x)
  result[na_idx] <- "--"
  
  # Manejar ceros
  zero_idx <- !na_idx & x == 0
  result[zero_idx] <- "0"
  
  # Procesar valores válidos
  valid_idx <- !na_idx & x != 0
  
  if (any(valid_idx)) {
    valores <- x[valid_idx]
    orden <- floor(log10(abs(valores)))
    
    # Grandes números: 100+
    idx_grande <- orden >= 2
    if (any(idx_grande)) {
      result[valid_idx][idx_grande] <- sprintf("%.0f", round(valores[idx_grande], 0))
    }
    
    # Números medios: 10-99
    idx_medio <- orden >= 1 & orden < 2
    if (any(idx_medio)) {
      result[valid_idx][idx_medio] <- sprintf("%.1f", round(valores[idx_medio], 1))
    }
    
    # Números pequeños: 1-9
    idx_pequeno <- orden >= 0 & orden < 1
    if (any(idx_pequeno)) {
      result[valid_idx][idx_pequeno] <- sprintf("%.2f", round(valores[idx_pequeno], 2))
    }
    
    # Muy pequeños: <1
    idx_muy_pequeno <- orden < 0
    if (any(idx_muy_pequeno)) {
      result[valid_idx][idx_muy_pequeno] <- sprintf("%.3f", round(valores[idx_muy_pequeno], 3))
    }
  }
  
  return(result)
}

#' Formatear dataframe completo con 3 cifras significativas
formatear_df_3_cifras_sig <- function(df) {
  df %>%
    mutate(across(where(is.numeric), formatear_3_cifras_sig))
}

# ==============================================================================
# EXPORTACIÓN CSV
# ==============================================================================

#' Exportar dataframe a CSV
exportar_csv <- function(df, nombre_archivo, carpeta = "resultados", timestamp = FALSE) {
  if (!is.data.frame(df)) {
    stop("❌ Error: 'df' debe ser un dataframe")
  }
  
  if (nrow(df) == 0) {
    warning("⚠️  Advertencia: dataframe vacío, no se guardará")
    return(invisible(NULL))
  }
  
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  if (timestamp) {
    timestamp_str <- format(Sys.time(), "_%Y%m%d_%H%M%S")
    nombre_final <- paste0(nombre_archivo, timestamp_str, ".csv")
  } else {
    nombre_final <- paste0(nombre_archivo, ".csv")
  }
  
  ruta_completa <- file.path(carpeta, nombre_final)
  
  write.csv(df, ruta_completa, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat(sprintf("✓ CSV guardado: %s (%d filas)\n", ruta_completa, nrow(df)))
  
  return(invisible(ruta_completa))
}

#' Guardar múltiples objetos a RDS
guardar_resultados <- function(lista_objetos, carpeta = "resultados", timestamp = FALSE) {
  if (!is.list(lista_objetos)) {
    stop("❌ Error: 'lista_objetos' debe ser una lista")
  }
  
  if (is.null(names(lista_objetos))) {
    stop("❌ Error: 'lista_objetos' debe ser una lista nombrada")
  }
  
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  rutas_guardadas <- character(length(lista_objetos))
  
  for (i in seq_along(lista_objetos)) {
    nombre <- names(lista_objetos)[i]
    objeto <- lista_objetos[[i]]
    
    if (timestamp) {
      timestamp_str <- format(Sys.time(), "_%Y%m%d_%H%M%S")
      nombre_final <- paste0(nombre, timestamp_str, ".rds")
    } else {
      nombre_final <- paste0(nombre, ".rds")
    }
    
    archivo <- file.path(carpeta, nombre_final)
    saveRDS(objeto, archivo)
    
    cat(sprintf("✓ RDS guardado: %s (%.1f KB)\n", archivo, file.info(archivo)$size / 1024))
    
    rutas_guardadas[i] <- archivo
  }
  
  return(invisible(rutas_guardadas))
}

#' Cargar múltiples objetos desde RDS
cargar_resultados <- function(archivos, carpeta = "resultados") {
  resultados <- list()
  
  for (archivo in archivos) {
    if (!grepl("\\.rds$", archivo)) {
      archivo_completo <- paste0(archivo, ".rds")
    } else {
      archivo_completo <- archivo
    }
    
    ruta <- file.path(carpeta, archivo_completo)
    
    if (!file.exists(ruta)) {
      warning(sprintf("⚠️  No encontrado: %s", ruta))
      next
    }
    
    nombre <- gsub("\\.rds$", "", basename(ruta))
    resultados[[nombre]] <- readRDS(ruta)
    
    cat(sprintf("✓ Cargado: %s\n", ruta))
  }
  
  return(resultados)
}

#' Verificar existencia de archivos de resultados
verificar_archivos <- function(archivos, carpeta = "resultados") {
  estado <- data.frame(
    archivo = archivos,
    existe = FALSE,
    tamaño_kb = NA,
    fecha_modificacion = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(archivos)) {
    archivo <- archivos[i]
    
    if (!grepl("\\.(csv|rds)$", archivo)) {
      ruta_csv <- file.path(carpeta, paste0(archivo, ".csv"))
      ruta_rds <- file.path(carpeta, paste0(archivo, ".rds"))
      
      if (file.exists(ruta_csv)) {
        ruta <- ruta_csv
      } else if (file.exists(ruta_rds)) {
        ruta <- ruta_rds
      } else {
        next
      }
    } else {
      ruta <- file.path(carpeta, archivo)
    }
    
    if (file.exists(ruta)) {
      info <- file.info(ruta)
      estado$existe[i] <- TRUE
      estado$tamaño_kb[i] <- round(info$size / 1024, 1)
      estado$fecha_modificacion[i] <- info$mtime
    }
  }
  
  return(estado)
}

cat("\n✓ Módulo I/O cargado (formateo vectorizado)\n")
cat("  Funciones disponibles:\n")
cat("    - formatear_3_cifras_sig() [VECTORIZADO]\n")
cat("    - formatear_df_3_cifras_sig()\n")
cat("    - exportar_csv()\n")
cat("    - guardar_resultados()\n")
cat("    - cargar_resultados()\n")
cat("    - verificar_archivos()\n\n")