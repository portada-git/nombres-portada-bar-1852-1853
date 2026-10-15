# Script para separación de nombres y apellidos con GPT-4o-mini y respaldo JSONL
# Cada fila procesada se guarda inmediatamente para evitar pérdida de datos

library(httr)
library(jsonlite)
library(dplyr)

# =============================================================================
# CONFIGURACIÓN INICIAL
# =============================================================================

# Tu API key de OpenAI (REEMPLAZAR con tu clave real)
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# =============================================================================
# FUNCIONES DE LIMPIEZA Y NORMALIZACIÓN DE NOMBRES
# =============================================================================

limpiar_nombre <- function(nombre) {
  # Verificar si el nombre está vacío o es NA
  if (is.na(nombre) || nombre == "" || nchar(trimws(nombre)) == 0) {
    return("")
  }
  
  # Convertir a string y limpiar espacios
  nombre <- trimws(as.character(nombre))
  
  # Convertir a mayúsculas para normalización
  nombre <- toupper(nombre)
  
  # Corregir errores evidentes de OCR y tipeo
  # Separar letras pegadas (ejemplo: "JUANPEREZ" -> "JUAN PEREZ")
  nombre <- gsub("([a-z])([A-Z])", "\\1 \\2", nombre)
  
  # Normalizar espacios múltiples
  nombre <- gsub("\\s+", " ", nombre)
  
  # Diccionario de correcciones comunes
  correcciones <- list(
    "JSOE" = "JOSE",
    "JSUÉ" = "JESUS", 
    "JUANCRUZ" = "JUANA CRUZ",
    "MNAUEL" = "MANUEL",
    "ANTNOIO" = "ANTONIO",
    "FRACISCO" = "FRANCISCO",
    "GUTIE RREZ" = "GUTIERREZ",
    "SÁNCHE Z" = "SANCHEZ",
    "FERN ÁNDEZ" = "FERNANDEZ",
    "MARTÍ NEZ" = "MARTINEZ",
    "GONZA LEZ" = "GONZALEZ",
    "RODRI GUEZ" = "RODRIGUEZ",
    "JUANPEREZ" = "JUAN PEREZ",
    "JUANBAUISTA" = "JUAN BAUTISTA"
  )
  
  # Aplicar correcciones
  for (error in names(correcciones)) {
    nombre <- gsub(error, correcciones[[error]], nombre, fixed = TRUE)
  }
  
  # Limpiar puntuación problemática
  nombre <- gsub("([A-Z])\\.,([A-Z])", "\\1. \\2", nombre)  # "J.,PÉREZ" -> "J. PÉREZ"
  nombre <- gsub("([A-Z])\\.;([A-Z])", "\\1. \\2", nombre)  # "D.;GARCÍA" -> "D. GARCÍA"
  
  return(trimws(nombre))
}

# =============================================================================
# FUNCIÓN PRINCIPAL DE SEPARACIÓN CON GPT-4O-MINI
# =============================================================================

separar_nombre_apellido <- function(nombre_completo, api_key) {
  
  # Limpiar el nombre primero
  nombre_limpio <- limpiar_nombre(nombre_completo)
  
  if (nombre_limpio == "") {
    return(list(MASTER_FORENAME = "", MASTER_SURNAME = ""))
  }
  
  # Aplicar reglas simples primero (según especificaciones)
  partes <- strsplit(nombre_limpio, "\\s+")[[1]]
  
  # REGLA 1: Un solo elemento = APELLIDO (nombre vacío)
  if (length(partes) == 1) {
    return(list(MASTER_FORENAME = "", MASTER_SURNAME = partes[1]))
  }
  
  # REGLA 2: Dos elementos = NOMBRE + APELLIDO
  if (length(partes) == 2) {
    return(list(MASTER_FORENAME = partes[1], MASTER_SURNAME = partes[2]))
  }
  
  # REGLA 3: Tres o más elementos = Usar GPT-4o-mini
  prompt <- paste0("Eres un experto en separación de nombres y apellidos de capitanes y patrones de barcos.\n\n",
                   "REGLAS ESPECÍFICAS:\n",
                   "1. Si hay UN solo elemento: Es el APELLIDO (nombre vacío)\n",
                   "2. Si hay DOS elementos: Primer elemento = NOMBRE, Segundo elemento = APELLIDO\n", 
                   "3. Si hay TRES o más elementos: Usa tu criterio considerando:\n",
                   "   - Nombres pueden ser simples o compuestos (ej: José María)\n",
                   "   - Iniciales pueden ser de nombre (ej: J. GARCIA, J.M. LOPEZ)\n",
                   "   - Capitanes extranjeros pueden aparecer solo con apellido\n",
                   "   - Algunas iniciales pueden ser dobles (ej: J M, A B)\n",
                   "   - Tratamientos como 'D.' (Don) suelen ir con el nombre\n\n",
                   "FORMATO DE RESPUESTA (solo JSON válido, sin explicaciones):\n",
                   '{\"MASTER_FORENAME\": \"nombre o inicial\", \"MASTER_SURNAME\": \"apellido\"}\n\n',
                   "Si el nombre está vacío, usa cadena vacía \"\".\n\n",
                   "EJEMPLOS:\n",
                   "• 'JOSÉ MARÍAS LÓPEZ FERNÁNDEZ' -> {\"MASTER_FORENAME\": \"JOSE MARIA\", \"MASTER_SURNAME\": \"LOPEZ FERNANDEZ\"}\n",
                   "• 'J. RODRÍGUEZ GARCÍA' -> {\"MASTER_FORENAME\": \"J.\", \"MASTER_SURNAME\": \"RODRIGUEZ GARCIA\"}\n",
                   "• 'J. JOSEPEREZ' -> {\"MASTER_FORENAME\": \"J. JOSE\", \"MASTER_SURNAME\": \"PEREZ\"}\n",
                   "• 'JUANPEREZ RODRIGUEZ' -> {\"MASTER_FORENAME\": \"JUAN\", \"MASTER_SURNAME\": \"PEREZ\"}\n",
                   "• 'D. ANTONIC PÉREZLÓPEZ' -> {\"MASTER_FORENAME\": \"D. ANTONIO\", \"MASTER_SURNAME\": \"PEREZ LOPEZ\"}\n",
                   "• 'A.B. MARTÍNEZ GONZÁLEZ RUIZ' -> {\"MASTER_FORENAME\": \"A.B.\", \"MASTER_SURNAME\": \"MARTINEZ GONZALEZ RUIZ\"}\n\n",
                   "NOMBRE A PROCESAR: '", nombre_limpio, "'")
  
  # Configuración de la petición a OpenAI
  url <- "https://api.openai.com/v1/chat/completions"
  
  headers <- add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )
  
  body <- list(
    model = "gpt-4o-mini",
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    max_tokens = 150,
    temperature = 0.01,
    response_format = list(type = "json_object")
  )
  
  # Realizar petición con manejo de errores y reintentos
  max_reintentos <- 3
  
  for (intento in 1:max_reintentos) {
    tryCatch({
      response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
      
      if (status_code(response) == 200) {
        content <- content(response, "parsed")
        resultado_json <- content$choices[[1]]$message$content
        
        # Parsear la respuesta JSON
        resultado <- fromJSON(resultado_json)
        
        forename <- trimws(as.character(resultado$MASTER_FORENAME %||% ""))
        surname <- trimws(as.character(resultado$MASTER_SURNAME %||% ""))
        
        return(list(MASTER_FORENAME = forename, MASTER_SURNAME = surname))
        
      } else {
        if (intento == max_reintentos) {
          # Fallback: usar lógica simple
          if (length(partes) >= 3) {
            # Asumir que el último es apellido, el resto nombre
            forename <- paste(partes[1:(length(partes)-1)], collapse = " ")
            surname <- partes[length(partes)]
            return(list(MASTER_FORENAME = forename, MASTER_SURNAME = surname))
          }
        }
      }
    }, error = function(e) {
      if (intento == max_reintentos) {
        # Fallback final
        if (length(partes) >= 3) {
          forename <- paste(partes[1:(length(partes)-1)], collapse = " ")
          surname <- partes[length(partes)]
          return(list(MASTER_FORENAME = forename, MASTER_SURNAME = surname))
        } else {
          return(list(MASTER_FORENAME = "", MASTER_SURNAME = nombre_limpio))
        }
      }
      
      # Esperar antes del siguiente intento
      Sys.sleep(1)
    })
  }
  
  # Fallback final si todo falla
  return(list(MASTER_FORENAME = "", MASTER_SURNAME = nombre_limpio))
}

# =============================================================================
# FUNCIONES DE MANEJO JSONL (ADAPTADAS)
# =============================================================================

# Crear archivo JSONL con metadatos iniciales
crear_archivo_jsonl_nombres <- function(nombre_archivo, df, columna_nombres) {
  
  # Crear metadatos del procesamiento
  metadatos <- list(
    tipo = "metadatos",
    proposito = "separacion_nombres_apellidos",
    fecha_inicio = Sys.time(),
    total_filas = nrow(df),
    columna_nombres = columna_nombres,
    version_script = "1.0",
    modelo_usado = "gpt-4o-mini"
  )
  
  # Escribir metadatos como primera línea
  write(toJSON(metadatos, auto_unbox = TRUE), file = nombre_archivo)
  
  cat("✓ Archivo JSONL creado:", nombre_archivo, "\n")
  cat("✓ Metadatos guardados\n")
}

# Guardar una fila procesada en JSONL
guardar_fila_jsonl_nombres <- function(nombre_archivo, indice_fila, nombre_original, 
                                      forename, surname, estado, tiempo_procesamiento = NULL,
                                      columnas_adicionales = NULL) {
  
  # Crear registro JSON para esta fila
  registro <- list(
    tipo = "resultado",
    fila = indice_fila,
    timestamp = Sys.time(),
    nombre_original = nombre_original,
    MASTER_FORENAME = forename,
    MASTER_SURNAME = surname,
    estado = estado,  # "exito", "error", "texto_vacio", "fallback"
    tiempo_procesamiento_seg = tiempo_procesamiento
  )
  
  # Agregar columnas adicionales si existen
  if (!is.null(columnas_adicionales)) {
    registro <- c(registro, columnas_adicionales)
  }
  
  # Escribir como nueva línea en el archivo JSONL
  write(toJSON(registro, auto_unbox = TRUE), file = nombre_archivo, append = TRUE)
}

# Leer progreso desde archivo JSONL
leer_progreso_jsonl_nombres <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    return(list(filas_procesadas = c(), ultima_fila = 0))
  }
  
  # Leer todas las líneas del archivo
  lineas <- readLines(nombre_archivo, warn = FALSE)
  
  if (length(lineas) == 0) {
    return(list(filas_procesadas = c(), ultima_fila = 0))
  }
  
  # Filtrar solo las líneas de resultado (no metadatos)
  resultados <- lineas[grepl('"tipo":"resultado"', lineas)]
  
  if (length(resultados) == 0) {
    return(list(filas_procesadas = c(), ultima_fila = 0))
  }
  
  # Extraer números de fila procesadas
  filas_procesadas <- c()
  for (linea in resultados) {
    json_obj <- fromJSON(linea)
    filas_procesadas <- c(filas_procesadas, json_obj$fila)
  }
  
  ultima_fila <- max(filas_procesadas)
  
  cat("✓ Progreso leído desde JSONL:", length(filas_procesadas), "filas ya procesadas\n")
  cat("✓ Última fila procesada:", ultima_fila, "\n")
  
  return(list(filas_procesadas = filas_procesadas, ultima_fila = ultima_fila))
}

# Recuperar dataframe desde archivo JSONL
recuperar_dataframe_desde_jsonl_nombres <- function(nombre_archivo, df_original) {
  
  progreso <- leer_progreso_jsonl_nombres(nombre_archivo)
  
  if (length(progreso$filas_procesadas) == 0) {
    return(df_original)
  }
  
  # Crear columnas resultado si no existen
  if (!"MASTER_FORENAME" %in% names(df_original)) {
    df_original$MASTER_FORENAME <- NA_character_
  }
  if (!"MASTER_SURNAME" %in% names(df_original)) {
    df_original$MASTER_SURNAME <- NA_character_
  }
  
  # Leer archivo JSONL línea por línea
  lineas <- readLines(nombre_archivo, warn = FALSE)
  resultados <- lineas[grepl('"tipo":"resultado"', lineas)]
  
  # Restaurar resultados en el dataframe
  for (linea in resultados) {
    json_obj <- fromJSON(linea)
    fila <- json_obj$fila
    df_original$MASTER_FORENAME[fila] <- json_obj$MASTER_FORENAME
    df_original$MASTER_SURNAME[fila] <- json_obj$MASTER_SURNAME
  }
  
  cat("✓ Dataframe restaurado con", length(progreso$filas_procesadas), "resultados\n")
  
  return(df_original)
}

# =============================================================================
# FUNCIÓN PRINCIPAL DE PROCESAMIENTO CON RESPALDO JSONL
# =============================================================================

procesar_nombres_con_respaldo <- function(df, 
                                         columna_nombres, 
                                         api_key = OPENAI_API_KEY,
                                         pausa_segundos = 1.0,
                                         fila_inicio = 1,
                                         fila_fin = NULL,
                                         nombre_archivo_jsonl = NULL,
                                         continuar_procesamiento = TRUE,
                                         columnas_extra_guardar = NULL) {
  
  # Validaciones iniciales
  if (!columna_nombres %in% names(df)) {
    stop(paste("Error: La columna '", columna_nombres, "' no existe en el dataframe"))
  }
  
  if (is.null(fila_fin)) {
    fila_fin <- nrow(df)
  }
  
  # Crear nombre de archivo JSONL si no se especifica
  if (is.null(nombre_archivo_jsonl)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    nombre_archivo_jsonl <- paste0("separacion_nombres_", timestamp, ".jsonl")
  }
  
  # Crear columnas resultado si no existen
  if (!"MASTER_FORENAME" %in% names(df)) {
    df$MASTER_FORENAME <- NA_character_
  }
  if (!"MASTER_SURNAME" %in% names(df)) {
    df$MASTER_SURNAME <- NA_character_
  }
  
  # Verificar si continuar procesamiento existente
  filas_ya_procesadas <- c()
  if (continuar_procesamiento && file.exists(nombre_archivo_jsonl)) {
    cat("🔄 Archivo JSONL existente detectado, recuperando progreso...\n")
    progreso <- leer_progreso_jsonl_nombres(nombre_archivo_jsonl)
    filas_ya_procesadas <- progreso$filas_procesadas
    df <- recuperar_dataframe_desde_jsonl_nombres(nombre_archivo_jsonl, df)
  } else {
    # Crear nuevo archivo JSONL
    crear_archivo_jsonl_nombres(nombre_archivo_jsonl, df, columna_nombres)
  }
  
  # Información del procesamiento
  total_filas <- fila_fin - fila_inicio + 1
  filas_pendientes <- setdiff(fila_inicio:fila_fin, filas_ya_procesadas)
  
  cat("\n=== SEPARACIÓN DE NOMBRES CON RESPALDO JSONL ===\n")
  cat("Archivo JSONL:", nombre_archivo_jsonl, "\n")
  cat("Columna de nombres:", columna_nombres, "\n")
  cat("Rango total:", fila_inicio, "a", fila_fin, "(", total_filas, "filas)\n")
  cat("Filas ya procesadas:", length(filas_ya_procesadas), "\n")
  cat("Filas pendientes:", length(filas_pendientes), "\n")
  cat("Pausa entre peticiones:", pausa_segundos, "segundos\n")
  cat("Modelo usado: GPT-4o-mini\n")
  cat("================================================\n\n")
  
  if (length(filas_pendientes) == 0) {
    cat("✅ ¡Todas las filas ya están procesadas!\n")
    return(df)
  }
  
  # Contadores
  exitos <- 0
  errores <- 0
  textos_vacios <- 0
  fallbacks <- 0
  
  # Procesar solo las filas pendientes
  for (i in filas_pendientes) {
    inicio_tiempo <- Sys.time()
    fila_actual <- which(filas_pendientes == i)
    
    cat("[", sprintf("%3d/%3d", fila_actual, length(filas_pendientes)), "] ")
    cat("Procesando fila", i, "...")
    
    # Obtener el nombre de la fila actual
    nombre <- df[[columna_nombres]][i]
    
    # Preparar columnas adicionales para guardar si se especificaron
    cols_extra <- NULL
    if (!is.null(columnas_extra_guardar)) {
      cols_extra <- list()
      for (col in columnas_extra_guardar) {
        if (col %in% names(df)) {
          cols_extra[[col]] <- df[[col]][i]
        }
      }
    }
    
    # Verificar si el nombre está vacío o es NA
    if (is.na(nombre) || nchar(trimws(as.character(nombre))) == 0) {
      df$MASTER_FORENAME[i] <- ""
      df$MASTER_SURNAME[i] <- ""
      textos_vacios <- textos_vacios + 1
      
      tiempo_transcurrido <- as.numeric(difftime(Sys.time(), inicio_tiempo, units = "secs"))
      
      # Guardar inmediatamente en JSONL
      guardar_fila_jsonl_nombres(nombre_archivo_jsonl, i, as.character(nombre), 
                               "", "", "texto_vacio", tiempo_transcurrido, cols_extra)
      
      cat(" TEXTO VACÍO ✓\n")
      next
    }
    
    # Separar el nombre
    tryCatch({
      resultado <- separar_nombre_apellido(nombre, api_key)
      tiempo_transcurrido <- as.numeric(difftime(Sys.time(), inicio_tiempo, units = "secs"))
      
      # Guardar resultado en el dataframe
      df$MASTER_FORENAME[i] <- resultado$MASTER_FORENAME
      df$MASTER_SURNAME[i] <- resultado$MASTER_SURNAME
      
      # Determinar estado
      estado <- "exito"
      if (resultado$MASTER_FORENAME == "" && resultado$MASTER_SURNAME == limpiar_nombre(nombre)) {
        estado <- "fallback"
        fallbacks <- fallbacks + 1
      } else {
        exitos <- exitos + 1
      }
      
      # Guardar en JSONL
      guardar_fila_jsonl_nombres(nombre_archivo_jsonl, i, as.character(nombre),
                               resultado$MASTER_FORENAME, resultado$MASTER_SURNAME,
                               estado, tiempo_transcurrido, cols_extra)
      
      if (estado == "fallback") {
        cat(" FALLBACK: '", resultado$MASTER_FORENAME, "' | '", resultado$MASTER_SURNAME, "' ✓\n", sep="")
      } else {
        cat(" ÉXITO: '", resultado$MASTER_FORENAME, "' | '", resultado$MASTER_SURNAME, "' ✓\n", sep="")
      }
      
    }, error = function(e) {
      # Error en el procesamiento
      df$MASTER_FORENAME[i] <- ""
      df$MASTER_SURNAME[i] <- as.character(nombre)
      errores <- errores + 1
      
      tiempo_transcurrido <- as.numeric(difftime(Sys.time(), inicio_tiempo, units = "secs"))
      
      guardar_fila_jsonl_nombres(nombre_archivo_jsonl, i, as.character(nombre),
                               "", as.character(nombre), "error", 
                               tiempo_transcurrido, cols_extra)
      
      cat(" ERROR:", e$message, " ✓\n")
    })
    
    # Mostrar progreso cada 10 filas
    if (fila_actual %% 10 == 0) {
      porcentaje <- round((fila_actual / length(filas_pendientes)) * 100, 1)
      cat("\n--- PROGRESO:", porcentaje, "% ---\n")
      cat("Éxitos:", exitos, "| Fallbacks:", fallbacks, "| Errores:", errores, "| Textos vacíos:", textos_vacios, "\n")
      cat("💾 Datos guardados automáticamente en:", nombre_archivo_jsonl, "\n\n")
    }
    
    # Pausa para evitar rate limits
    if (i < max(filas_pendientes)) {
      Sys.sleep(pausa_segundos)
    }
  }
  
  # Resumen final
  total_procesado_ahora <- length(filas_pendientes)
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  cat("📁 Archivo JSONL:", nombre_archivo_jsonl, "\n")
  cat("Procesado ahora:", total_procesado_ahora, "filas\n")
  cat("Total en archivo:", length(filas_ya_procesadas) + total_procesado_ahora, "filas\n")
  cat("✓ Éxitos:", exitos, "\n")
  cat("🔄 Fallbacks:", fallbacks, "\n")
  cat("✗ Errores:", errores, "\n")
  cat("○ Textos vacíos:", textos_vacios, "\n")
  if (total_procesado_ahora > 0) {
    tasa_exito <- round(((exitos + fallbacks) / total_procesado_ahora) * 100, 1)
    cat("Tasa de procesamiento exitoso:", tasa_exito, "%\n")
  }
  cat("💾 Todos los datos están respaldados en JSONL\n")
  cat("==================================\n")
  
  return(df)
}

# =============================================================================
# FUNCIONES DE UTILIDAD PARA JSONL (ADAPTADAS)
# =============================================================================

# Ver estadísticas del archivo JSONL
estadisticas_jsonl_nombres <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    cat("❌ El archivo JSONL no existe\n")
    return()
  }
  
  lineas <- readLines(nombre_archivo, warn = FALSE)
  resultados <- lineas[grepl('"tipo":"resultado"', lineas)]
  
  if (length(resultados) == 0) {
    cat("📊 No hay resultados en el archivo JSONL\n")
    return()
  }
  
  # Analizar estados
  estados <- sapply(resultados, function(linea) {
    json_obj <- fromJSON(linea)
    return(json_obj$estado)
  })
  
  cat("📊 ESTADÍSTICAS DEL ARCHIVO JSONL\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Total de registros:", length(resultados), "\n")
  cat("✓ Éxitos:", sum(estados == "exito"), "\n")
  cat("🔄 Fallbacks:", sum(estados == "fallback"), "\n")
  cat("✗ Errores:", sum(estados == "error"), "\n")
  cat("○ Textos vacíos:", sum(estados == "texto_vacio"), "\n")
  
  if (length(resultados) > 0) {
    tasa_procesamiento <- round(((sum(estados == "exito") + sum(estados == "fallback")) / length(resultados)) * 100, 1)
    cat("Tasa de procesamiento exitoso:", tasa_procesamiento, "%\n")
  }
}

# Exportar JSONL a CSV
jsonl_nombres_a_csv <- function(nombre_archivo_jsonl, nombre_archivo_csv = NULL) {
  if (!file.exists(nombre_archivo_jsonl)) {
    cat("❌ El archivo JSONL no existe\n")
    return()
  }
  
  if (is.null(nombre_archivo_csv)) {
    nombre_archivo_csv <- gsub("\\.jsonl$", "_resultados.csv", nombre_archivo_jsonl)
  }
  
  lineas <- readLines(nombre_archivo_jsonl, warn = FALSE)
  resultados <- lineas[grepl('"tipo":"resultado"', lineas)]
  
  if (length(resultados) == 0) {
    cat("❌ No hay resultados para exportar\n")
    return()
  }
  
  # Convertir a dataframe
  df_resultados <- data.frame()
  
  for (linea in resultados) {
    json_obj <- fromJSON(linea)
    fila_df <- data.frame(
      fila = json_obj$fila,
      timestamp = json_obj$timestamp,
      nombre_original = json_obj$nombre_original,
      MASTER_FORENAME = json_obj$MASTER_FORENAME,
      MASTER_SURNAME = json_obj$MASTER_SURNAME,
      estado = json_obj$estado,
      tiempo_procesamiento_seg = ifelse(is.null(json_obj$tiempo_procesamiento_seg), 
                                       NA, json_obj$tiempo_procesamiento_seg),
      stringsAsFactors = FALSE
    )
    
    df_resultados <- rbind(df_resultados, fila_df)
  }
  
  # Ordenar por fila
  df_resultados <- df_resultados[order(df_resultados$fila), ]
  
  write.csv(df_resultados, nombre_archivo_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✅ Exportado a CSV:", nombre_archivo_csv, "\n")
  cat("📊", nrow(df_resultados), "registros exportados\n")
}

# =============================================================================
# FUNCIONES PARA PROBAR NOMBRES INDIVIDUALES
# =============================================================================

# Función para probar nombres individuales sin JSONL
probar_separacion_individual <- function(nombres, api_key = OPENAI_API_KEY) {
  cat("=== PRUEBA DE SEPARACIÓN INDIVIDUAL ===\n\n")
  
  for (nombre in nombres) {
    cat("Original: '", nombre, "'\n", sep="")
    resultado <- separar_nombre_apellido(nombre, api_key)
    cat("Nombre: '", resultado$MASTER_FORENAME, "'\n", sep="")
    cat("Apellido: '", resultado$MASTER_SURNAME, "'\n", sep="")
    cat("----------------------------------------\n")
    Sys.sleep(0.5)  # Pequeña pausa entre llamadas
  }
}

# =============================================================================
# EJEMPLO DE USO
# =============================================================================

# PASO 1: Configurar tu API key
# OPENAI_API_KEY <- "tu-clave-api-real-aqui"

# PASO 2: Cargar tu dataframe existente
# mi_dataframe <- read.csv("mi_archivo.csv", stringsAsFactors = FALSE)

# PASO 3: Procesar con respaldo automático (RECOMENDADO)
# mi_dataframe_procesado <- procesar_nombres_con_respaldo(
#   df = mi_dataframe,
#   columna_nombres = "nombre_completo",
#   api_key = OPENAI_API_KEY,
#   pausa_segundos = 1.0,
#   nombre_archivo_jsonl = "separacion_nombres.jsonl",
#   continuar_procesamiento = TRUE,
#   columnas_extra_guardar = c("id", "barco", "fecha")
# )

# PASO 4: Ver estadísticas del procesamiento
# estadisticas_jsonl_nombres("separacion_nombres.jsonl")

# PASO 5: Exportar resultados a CSV si se desea
# jsonl_nombres_a_csv("separacion_nombres.jsonl", "nombres_separados.csv")

# =============================================================================
# DATAFRAME DE EJEMPLO PARA PRUEBAS
# =============================================================================

crear_dataframe_ejemplo_nombres <- function() {
  df_ejemplo <- data.frame(
    id = 1:13,
    nombre_completo = c(
      "JUAN GARCÍA",                          # 2 elementos -> simple
      "d GONZALEZ MARTINEZ LOPEZ",              # 3 elementos -> múltiple apellidos
      "JOSÉ MARÍA LÓPEZ FERNÁNDEZ",           # 4 elementos -> complejo
      "J RODRÍGUEZ",                        # 2 elementos -> inicial + apellido
      "SMITH",                               # 1 elemento -> solo apellido
      "A B. MARTÍNEZ GONZÁLEZ",              # 4 elementos -> doble inicial
      "D. ANTONIO PÉREZ LÓPEZ",              # 4 elementos -> tratamiento
      "FRANCISCO JSOE PÉREZ",                # Con error -> corrección
      "JUANPEREZ COMPANC",                          # Pegado -> separación
      "JOSÉ MARÍA",                         # 2 elementos -> nombre compuesto
      "J. M FERNÁNDEZ GARCÍA RUIZ",         # 5 elementos -> muy complejo
      "",                                   # Vacío -> texto vacío
      "GONZALEZ MARTINEZ LOPEZ"              # 3 elementos -> múltiple apellidos
    ),
    barco = c("Barco1", "Barco2", "Barco3", "Barco4", "Barco5", "Barco6", 
              "Barco7", "Barco8", "Barco9", "Barco10", "Barco11", "Barco12", "Barco13"),
    puerto = c("Valencia", "Barcelona", "Palma", "Alicante", "Tarragona", "Mahón", 
               "Cádiz", "Sevilla", "Málaga", "Santander", "La Coruña", "Bilbao", "Gijón"),
    stringsAsFactors = FALSE
  )
  
  return(df_ejemplo)
}

# =============================================================================
# PRUEBA RÁPIDA CON EJEMPLOS
# =============================================================================

# Función para ejecutar prueba completa con ejemplos
ejecutar_prueba_completa <- function() {
  
  cat("🧪 EJECUTANDO PRUEBA COMPLETA DE SEPARACIÓN DE NOMBRES\n")
  cat("======================================================\n\n")
  
  # Crear dataframe de ejemplo
  df_ejemplo <- crear_dataframe_ejemplo_nombres()
  
  cat("📋 Dataframe de ejemplo creado con", nrow(df_ejemplo), "filas\n")
  print(df_ejemplo)
  cat("\n")
  
  # Procesar con respaldo JSONL
  cat("🚀 Iniciando procesamiento con respaldo JSONL...\n\n")
  
  df_resultado <- procesar_nombres_con_respaldo(
    df = df_ejemplo,
    columna_nombres = "nombre_completo",
    nombre_archivo_jsonl = "prueba_separacion_nombres.jsonl",
    pausa_segundos = 0.8,  # Pausa más corta para pruebas
    columnas_extra_guardar = c("barco", "puerto")
  )
  
  cat("\n📊 RESULTADOS DEL PROCESAMIENTO:\n")
  cat("=================================\n")
  
  # Mostrar tabla de resultados
  resultado_tabla <- df_resultado[, c("id", "nombre_completo", "MASTER_FORENAME", "MASTER_SURNAME")]
  print(resultado_tabla)
  
  cat("\n📈 Estadísticas del procesamiento:\n")
  estadisticas_jsonl_nombres("prueba_separacion_nombres.jsonl")
  
  cat("\n💾 Exportando a CSV...\n")
  jsonl_nombres_a_csv("prueba_separacion_nombres.jsonl", "prueba_resultados.csv")
  
  cat("\n✅ PRUEBA COMPLETA FINALIZADA\n")
  cat("Archivos generados:\n")
  cat("- prueba_separacion_nombres.jsonl (respaldo)\n")
  cat("- prueba_resultados.csv (resultados)\n\n")
  
  return(df_resultado)
}

# =============================================================================
# FUNCIONES ADICIONALES PARA ANÁLISIS
# =============================================================================

# Analizar calidad de separación
analizar_calidad_separacion <- function(nombre_archivo_jsonl) {
  if (!file.exists(nombre_archivo_jsonl)) {
    cat("❌ El archivo JSONL no existe\n")
    return()
  }
  
  lineas <- readLines(nombre_archivo_jsonl, warn = FALSE)
  resultados <- lineas[grepl('"tipo":"resultado"', lineas)]
  
  if (length(resultados) == 0) {
    cat("❌ No hay resultados para analizar\n")
    return()
  }
  
  cat("🔍 ANÁLISIS DE CALIDAD DE SEPARACIÓN\n")
  cat("====================================\n\n")
  
  # Contadores por tipo de resultado
  contadores <- list(
    solo_nombre = 0,
    solo_apellido = 0,
    ambos = 0,
    iniciales = 0,
    nombres_compuestos = 0,
    apellidos_compuestos = 0
  )
  
  ejemplos <- list()
  
  for (linea in resultados) {
    json_obj <- fromJSON(linea)
    
    forename <- trimws(json_obj$MASTER_FORENAME)
    surname <- trimws(json_obj$MASTER_SURNAME)
    original <- json_obj$nombre_original
    
    # Clasificar resultado
    if (forename == "" && surname != "") {
      contadores$solo_apellido <- contadores$solo_apellido + 1
      ejemplos$solo_apellido <- c(ejemplos$solo_apellido, paste0("'", original, "' -> Apellido: '", surname, "'"))
    } else if (forename != "" && surname == "") {
      contadores$solo_nombre <- contadores$solo_nombre + 1
      ejemplos$solo_nombre <- c(ejemplos$solo_nombre, paste0("'", original, "' -> Nombre: '", forename, "'"))
    } else if (forename != "" && surname != "") {
      contadores$ambos <- contadores$ambos + 1
      ejemplos$ambos <- c(ejemplos$ambos, paste0("'", original, "' -> '", forename, "' | '", surname, "'"))
    }
    
    # Detectar patrones
    if (grepl("\\.", forename)) {
      contadores$iniciales <- contadores$iniciales + 1
    }
    if (grepl("\\s", forename)) {
      contadores$nombres_compuestos <- contadores$nombres_compuestos + 1
    }
    if (grepl("\\s", surname)) {
      contadores$apellidos_compuestos <- contadores$apellidos_compuestos + 1
    }
  }
  
  # Mostrar estadísticas
  total <- length(resultados)
  cat("Total de registros analizados:", total, "\n\n")
  
  cat("📊 DISTRIBUCIÓN DE RESULTADOS:\n")
  cat("Solo apellido:", contadores$solo_apellido, "(", round(contadores$solo_apellido/total*100, 1), "%)\n")
  cat("Solo nombre:", contadores$solo_nombre, "(", round(contadores$solo_nombre/total*100, 1), "%)\n")
  cat("Nombre + Apellido:", contadores$ambos, "(", round(contadores$ambos/total*100, 1), "%)\n\n")
  
  cat("🏷️ PATRONES DETECTADOS:\n")
  cat("Con iniciales:", contadores$iniciales, "(", round(contadores$iniciales/total*100, 1), "%)\n")
  cat("Nombres compuestos:", contadores$nombres_compuestos, "(", round(contadores$nombres_compuestos/total*100, 1), "%)\n")
  cat("Apellidos compuestos:", contadores$apellidos_compuestos, "(", round(contadores$apellidos_compuestos/total*100, 1), "%)\n\n")
  
  # Mostrar ejemplos (máximo 3 por categoría)
  if (length(ejemplos$solo_apellido) > 0) {
    cat("📝 EJEMPLOS - Solo Apellido:\n")
    for (i in 1:min(3, length(ejemplos$solo_apellido))) {
      cat("  ", ejemplos$solo_apellido[i], "\n")
    }
    cat("\n")
  }
  
  if (length(ejemplos$ambos) > 0) {
    cat("📝 EJEMPLOS - Nombre + Apellido:\n")
    for (i in 1:min(5, length(ejemplos$ambos))) {
      cat("  ", ejemplos$ambos[i], "\n")
    }
    cat("\n")
  }
}

# =============================================================================
# INSTRUCCIONES DE USO PASO A PASO
# =============================================================================

mostrar_instrucciones <- function() {
  cat("📋 INSTRUCCIONES DE USO - SEPARADOR DE NOMBRES\n")
  cat("==============================================\n\n")
  
  cat("🔧 CONFIGURACIÓN INICIAL:\n")
  cat("1. Instalar paquetes necesarios:\n")
  cat("   install.packages(c('httr', 'jsonlite', 'dplyr'))\n\n")
  
  cat("2. Configurar API key de OpenAI:\n")
  cat("   # Opción A: Variable de entorno\n")
  cat("   Sys.setenv(OPENAI_API_KEY = 'tu-clave-aqui')\n")
  cat("   # Opción B: Directamente en código\n")
  cat("   OPENAI_API_KEY <- 'tu-clave-aqui'\n\n")
  
  cat("📊 USO BÁSICO:\n")
  cat("1. Cargar tus datos:\n")
  cat("   mi_df <- read.csv('mi_archivo.csv', stringsAsFactors = FALSE)\n\n")
  
  cat("2. Procesar con respaldo automático:\n")
  cat("   resultado <- procesar_nombres_con_respaldo(\n")
  cat("     df = mi_df,\n")
  cat("     columna_nombres = 'nombre_completo',  # Cambia por tu columna\n")
  cat("     nombre_archivo_jsonl = 'mi_separacion.jsonl'\n")
  cat("   )\n\n")
  
  cat("3. Ver estadísticas:\n")
  cat("   estadisticas_jsonl_nombres('mi_separacion.jsonl')\n\n")
  
  cat("4. Exportar a CSV:\n")
  cat("   jsonl_nombres_a_csv('mi_separacion.jsonl', 'resultados.csv')\n\n")
  
  cat("🔄 CONTINUAR PROCESAMIENTO INTERRUMPIDO:\n")
  cat("Si el proceso se interrumpe, simplemente ejecuta nuevamente:\n")
  cat("   resultado <- procesar_nombres_con_respaldo(\n")
  cat("     df = mi_df,\n")
  cat("     columna_nombres = 'nombre_completo',\n")
  cat("     nombre_archivo_jsonl = 'mi_separacion.jsonl',  # Mismo archivo\n")
  cat("     continuar_procesamiento = TRUE  # Por defecto es TRUE\n")
  cat("   )\n\n")
  
  cat("🧪 EJECUTAR PRUEBA:\n")
  cat("Para probar el sistema con datos de ejemplo:\n")
  cat("   resultado_prueba <- ejecutar_prueba_completa()\n\n")
  
  cat("📋 REGLAS DE SEPARACIÓN:\n")
  cat("- 1 elemento: Solo apellido (nombre vacío)\n")
  cat("- 2 elementos: Primer elemento = nombre, segundo = apellido\n")
  cat("- 3+ elementos: GPT-4o-mini decide usando contexto de capitanes/patrones\n")
  cat("- Se corrigen errores comunes de OCR automáticamente\n")
  cat("- Resultado en columnas: MASTER_FORENAME y MASTER_SURNAME\n\n")
}

# =============================================================================
# FUNCIÓN PRINCIPAL PARA EJECUTAR TODO
# =============================================================================

# Mostrar instrucciones al cargar el script
cat("🚢 SEPARADOR DE NOMBRES DE CAPITANES Y PATRONES\n")
cat("Versión con GPT-4o-mini y respaldo JSONL\n")
cat("===========================================\n\n")

cat("Script cargado exitosamente.\n")
cat("Ejecuta mostrar_instrucciones() para ver las instrucciones completas.\n")
cat("Ejecuta ejecutar_prueba_completa() para probar con datos de ejemplo.\n\n")

# Verificar si la API key está configurada
if (Sys.getenv("OPENAI_API_KEY") == "") {
  cat("⚠️  ADVERTENCIA: API key de OpenAI no configurada.\n")
  cat("   Configúrala con: Sys.setenv(OPENAI_API_KEY = 'tu-clave')\n\n")
} else {
  cat("✅ API key de OpenAI configurada correctamente.\n\n")
}


mi_df <- crear_dataframe_ejemplo_nombres()

resultado <- procesar_nombres_con_respaldo(df = mi_df, 
  columna_nombres = 'nombre_completo',  
  nombre_archivo_jsonl = 'mi_separacion2.jsonl')


mi_df <- nom_mas_freq_candidatos[2700,]

resultado <- procesar_nombres_con_respaldo(df = mi_df, 
  columna_nombres = 'nombre_original',  
  nombre_archivo_jsonl = 'mi_separacion4.jsonl')


mi_df <- tabla_nom_ape_simp

df_nom_ape_simp <- procesar_nombres_con_respaldo(df = mi_df, 
  columna_nombres = 'nombre_norm',  
  nombre_archivo_jsonl = 'tabla_nom_ape_simp.jsonl')

df_nom_ape_simp |> write.csv2("tabla.csv", row.names = FALSE, fileEncoding = "UTF-8")
