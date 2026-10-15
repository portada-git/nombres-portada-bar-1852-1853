# Script para extraer nombres de capitán/patrón desde DataFrame existente
# Procesamiento fila por fila con seguimiento en tiempo real

library(httr)
library(jsonlite)
library(dplyr)

# =============================================================================
# CONFIGURACIÓN INICIAL
# =============================================================================

# Tu API key de OpenAI (REEMPLAZAR con tu clave real)
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# =============================================================================
# FUNCIÓN PRINCIPAL DE EXTRACCIÓN
# =============================================================================

extraer_nombre_capitan <- function(texto, api_key) {
  
  # Prompt optimizado basado en ejemplos reales
  prompt <- paste0("Extrae únicamente el nombre del capitán o patrón del siguiente texto histórico de navegación española del siglo XIX. ",
                   "REGLAS IMPORTANTES:\n",
                   "- Devuelve SOLO el nombre tal como aparece en el texto, sin explicaciones adicionales\n",
                   "- Busca después de las abreviaciones: 'p.' (patrón), 'c.' (capitán), 'cap.' (capitán)\n",
                   "- El nombre puede aparecer como: inicial y apellido, nombre completo y apellido, o solo apellido\n",
                   "- Mantén EXACTAMENTE el formato original (puntos, espacios, mayúsculas)\n",
                   "- Si hay múltiples nombres, extrae solo el del capitán/patrón/piloto\n",
                   "- Si no encuentras un nombre claro después de p./c./piloto, devuelve 'NO ENCONTRADO'\n\n",
                   "EJEMPLOS del formato esperado:\n",
                   "Texto: 'laud Merced, de 23 t., p. V. Obiol, con 2300 arrobas' → 'V. Obiol'\n",
                   "Texto: 'polacra goleta Jóven Mercedita, de 102 t., c. D. B. Crovetto, con 400 almireces' → 'D. B. Crovetto'\n",
                   "Texto: 'laud Pastor, de 39 t., p. J. Agustin Roca, con 1200 fanegas' → 'J. Agustin Roca'\n\n",
                   "Texto a analizar: '", texto, "'")
  
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
    max_tokens = 100,
    temperature = 0.0
  )
  
  # Realizar petición con manejo de errores
  tryCatch({
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) == 200) {
      content <- content(response, "parsed")
      resultado <- content$choices[[1]]$message$content
      return(trimws(resultado))
    } else {
      return(paste("Error API:", status_code(response)))
    }
  }, error = function(e) {
    return(paste("Error conexión:", e$message))
  })
}

# =============================================================================
# FUNCIÓN PARA PROCESAR DATAFRAME EXISTENTE
# =============================================================================

procesar_dataframe_existente <- function(df, 
                                       columna_texto, 
                                       nombre_columna_resultado = "nombre_capitan",
                                       api_key = OPENAI_API_KEY,
                                       pausa_segundos = 1.5,
                                       fila_inicio = 1,
                                       fila_fin = NULL) {
  
  # Validaciones iniciales
  if (!columna_texto %in% names(df)) {
    stop(paste("Error: La columna '", columna_texto, "' no existe en el dataframe"))
  }
  
  if (is.null(fila_fin)) {
    fila_fin <- nrow(df)
  }
  
  # Crear columna resultado si no existe
  if (!nombre_columna_resultado %in% names(df)) {
    df[[nombre_columna_resultado]] <- NA_character_
    cat("✓ Columna '", nombre_columna_resultado, "' creada en el dataframe\n")
  }
  
  # Información del procesamiento
  total_filas <- fila_fin - fila_inicio + 1
  cat("=== INICIANDO PROCESAMIENTO ===\n")
  cat("Dataframe:", deparse(substitute(df)), "\n")
  cat("Columna de texto:", columna_texto, "\n")
  cat("Columna resultado:", nombre_columna_resultado, "\n")
  cat("Filas a procesar:", fila_inicio, "a", fila_fin, "(", total_filas, "filas)\n")
  cat("Pausa entre peticiones:", pausa_segundos, "segundos\n")
  cat("=====================================\n\n")
  
  # Contador de resultados
  exitos <- 0
  errores <- 0
  no_encontrados <- 0
  textos_vacios <- 0
  
  # Procesar fila por fila
  for (i in fila_inicio:fila_fin) {
    fila_actual <- i - fila_inicio + 1
    
    cat("[", sprintf("%3d/%3d", fila_actual, total_filas), "] ")
    cat("Procesando fila", i, "...")
    
    # Obtener el texto de la fila actual
    texto <- df[[columna_texto]][i]
    
    # Verificar si el texto está vacío o es NA
    if (is.na(texto) || nchar(trimws(texto)) == 0) {
      df[[nombre_columna_resultado]][i] <- "Texto vacío"
      textos_vacios <- textos_vacios + 1
      cat(" TEXTO VACÍO\n")
      next
    }
    
    # Extraer el nombre del capitán
    resultado <- extraer_nombre_capitan(texto, api_key)
    
    # Guardar resultado en el dataframe
    df[[nombre_columna_resultado]][i] <- resultado
    
    # Clasificar resultado para estadísticas
    if (grepl("^Error", resultado)) {
      errores <- errores + 1
      cat(" ERROR:", resultado, "\n")
    } else if (resultado == "No encontrado") {
      no_encontrados <- no_encontrados + 1
      cat(" NO ENCONTRADO\n")
    } else {
      exitos <- exitos + 1
      cat(" ÉXITO:", resultado, "\n")
    }
    
    # Mostrar progreso cada 10 filas
    if (fila_actual %% 10 == 0) {
      porcentaje <- round((fila_actual / total_filas) * 100, 1)
      cat("\n--- PROGRESO:", porcentaje, "% ---\n")
      cat("Éxitos:", exitos, "| No encontrados:", no_encontrados, 
          "| Errores:", errores, "| Textos vacíos:", textos_vacios, "\n\n")
    }
    
    # Pausa para evitar rate limits
    if (i < fila_fin) {  # No hacer pausa en la última iteración
      Sys.sleep(pausa_segundos)
    }
  }
  
  # Resumen final
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  cat("Total procesado:", total_filas, "filas\n")
  cat("✓ Éxitos:", exitos, "\n")
  cat("⚠ No encontrados:", no_encontrados, "\n")
  cat("✗ Errores:", errores, "\n")
  cat("○ Textos vacíos:", textos_vacios, "\n")
  cat("Tasa de éxito:", round((exitos / total_filas) * 100, 1), "%\n")
  cat("==================================\n")
  
  return(df)
}

# =============================================================================
# FUNCIÓN DE VERIFICACIÓN DE RESULTADOS
# =============================================================================

verificar_extraccion <- function(df, columna_original, columna_resultado, mostrar_ejemplos = 5) {
  cat("\n=== VERIFICACIÓN DE RESULTADOS ===\n")
  
  # Estadísticas generales
  total <- nrow(df)
  exitos <- sum(!is.na(df[[columna_resultado]]) & 
                !grepl("^Error|No encontrado|Texto vacío", df[[columna_resultado]]))
  
  cat("Total de registros:", total, "\n")
  cat("Nombres extraídos:", exitos, "\n")
  cat("Tasa de éxito:", round((exitos / total) * 100, 1), "%\n\n")
  
  # Mostrar ejemplos exitosos
  if (mostrar_ejemplos > 0) {
    cat("=== EJEMPLOS EXITOSOS ===\n")
    ejemplos_exitosos <- df[!is.na(df[[columna_resultado]]) & 
                           !grepl("^Error|No encontrado|Texto vacío", df[[columna_resultado]]), ]
    
    n_mostrar <- min(mostrar_ejemplos, nrow(ejemplos_exitosos))
    
    for (i in 1:n_mostrar) {
      cat("\nEjemplo", i, ":\n")
      cat("Texto original: ", substr(ejemplos_exitosos[[columna_original]][i], 1, 80), "...\n")
      cat("Nombre extraído: '", ejemplos_exitosos[[columna_resultado]][i], "'\n")
    }
  }
}

# =============================================================================
# FUNCIÓN PARA GUARDAR RESULTADOS
# =============================================================================

guardar_resultados <- function(df, nombre_archivo = NULL) {
  if (is.null(nombre_archivo)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    nombre_archivo <- paste0("dataframe_con_capitanes_", timestamp, ".csv")
  }
  
  write.csv(df, nombre_archivo, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✓ Dataframe guardado como:", nombre_archivo, "\n")
}

# =============================================================================
# EJEMPLO DE USO
# =============================================================================

# PASO 1: Configurar tu API key
# OPENAI_API_KEY <- "tu-clave-api-real-aqui"

# PASO 2: Cargar tu dataframe existente (ejemplo)
# mi_dataframe <- read.csv("mi_archivo.csv", stringsAsFactors = FALSE)

# PASO 3: Procesar el dataframe
# mi_dataframe_procesado <- procesar_dataframe_existente(
#   df = mi_dataframe,
#   columna_texto = "nombre_de_tu_columna_texto",  # Cambiar por el nombre real
#   nombre_columna_resultado = "nombre_capitan",   # Nombre de la nueva columna
#   api_key = OPENAI_API_KEY,
#   pausa_segundos = 1.5,    # Ajustar según tu plan de OpenAI
#   fila_inicio = 1,         # Opcional: fila de inicio
#   fila_fin = NULL          # Opcional: fila final (NULL = hasta el final)
# )

# PASO 4: Verificar resultados
# verificar_extraccion(mi_dataframe_procesado, "nombre_de_tu_columna_texto", "nombre_capitan")

# PASO 5: Guardar resultados
# guardar_resultados(mi_dataframe_procesado, "mi_dataframe_con_capitanes.csv")

# =============================================================================
# DATAFRAME DE EJEMPLO PARA PRUEBAS
# =============================================================================

crear_dataframe_ejemplo <- function() {
  df_ejemplo <- data.frame(
    id = 1:5,
    texto_historico = c(
      "De id en 2 d. laud Merced, de 23 t., p. V. Obiol, con 2300 arrobas algarrobas.",
      "polacra goleta Jóven Mercedita, de 102 t., c. D. B. Crovetto, con 400 almireces.",
      "laud Pastor, de 39 t., p. J. Agustin Roca, con 1200 fanegas de trigo.",
      "bergantín San José, de 89 t., c. José María Sánchez, procedente de Valencia.",
      "goleta Carmen sin información de capitán disponible."
    ),
    puerto_origen = c("Valencia", "Barcelona", "Palma", "Alicante", "Tarragona"),
    stringsAsFactors = FALSE
  )
  
  return(df_ejemplo)
}

# Para probar el script con datos de ejemplo:
# df_prueba <- crear_dataframe_ejemplo()
df_resultado_2 <- procesar_dataframe_existente(df_prueba, "texto_historico")
# verificar_extraccion(df_resultado, "texto_historico", "nombre_capitan")