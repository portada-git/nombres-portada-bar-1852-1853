library(parallel)
library(stringdist)
library(dplyr)
library(readr)

# Ponderaciones x columna
ponderaciones <- c(
  master_forename = 3.5,    # Nombre del capitán
  master_surname = 4.0,     # Apellido del capitán
  nom_barrco = 3.5,         # Nombre del barco
  tipo_bar = 2.0,           # Tipo de barco
  bandera = 2.5,            # Bandera
  tn = 2.5,                 # Tonelaje
  rol = 0.5                 # Rol
)

# Función para calcular similitud fonética
calcular_similitud_fonetica <- function(texto1, texto2) {
  if (is.na(texto1) || is.na(texto2) || texto1 == "" || texto2 == "") {
    return(0)
  }
  
  # Usar soundex
  soundex1 <- stringdist::phonetic(texto1, method = "soundex")
  soundex2 <- stringdist::phonetic(texto2, method = "soundex")
  
  # Similitud fonética
  sim_soundex <- ifelse(soundex1 == soundex2, 1, 0)
  
  return(sim_soundex)
}

# Función para calcular similitud combinada (morfológica y fonética)
calcular_similitud_combinada <- function(texto1, texto2) {
  if (is.na(texto1) || is.na(texto2) || texto1 == "" || texto2 == "") {
    return(0)
  }
  
  # Calcular similitud morfológica (Jaro-Winkler)
  sim_morfologica <- 1 - stringdist(texto1, texto2, method = "jw")
  
  # Calcular similitud fonética
  sim_fonetica <- calcular_similitud_fonetica(texto1, texto2)
  
  # Promedio ponderado (más peso a morfológica)
  return((sim_morfologica * 0.7) + (sim_fonetica * 0.3))
}

# Función para calcular similitud numérica (tonelaje)
calcular_similitud_numerica <- function(val1, val2) {
  if (is.na(val1) || is.na(val2)) {
    return(0)
  }
  
  # Convertir a numérico
  num1 <- as.numeric(val1)
  num2 <- as.numeric(val2)
  
  if (is.na(num1) || is.na(num2) || max(num1, num2) == 0) {
    return(0)
  }
  
  # Calcular similitud basada en diferencia relativa
  similitud <- 1 - abs(num1 - num2) / max(num1, num2)
  return(max(0, similitud))
}

# Función principal para calcular similitud entre dos registros
calcular_similitud_registros <- function(fila1, fila2, ponderaciones) {
  similitud_total <- 0
  suma_ponderaciones <- 0
  
  # Similitud de nombre del capitán
  if (!is.na(fila1$master_forename) && !is.na(fila2$master_forename)) {
    similitud <- calcular_similitud_combinada(fila1$master_forename, fila2$master_forename)
    similitud_total <- similitud_total + (similitud * ponderaciones["master_forename"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["master_forename"]
  }
  
  # Similitud de apellido del capitán
  if (!is.na(fila1$master_surname) && !is.na(fila2$master_surname)) {
    similitud <- calcular_similitud_combinada(fila1$master_surname, fila2$master_surname)
    similitud_total <- similitud_total + (similitud * ponderaciones["master_surname"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["master_surname"]
  }
  
  # Similitud de nombre del barco
  if (!is.na(fila1$nom_barrco) && !is.na(fila2$nom_barrco)) {
    similitud <- calcular_similitud_combinada(fila1$nom_barrco, fila2$nom_barrco)
    similitud_total <- similitud_total + (similitud * ponderaciones["nom_barrco"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["nom_barrco"]
  }
  
  # Similitud de tipo de barco
  if (!is.na(fila1$tipo_bar) && !is.na(fila2$tipo_bar)) {
    similitud <- calcular_similitud_combinada(fila1$tipo_bar, fila2$tipo_bar)
    similitud_total <- similitud_total + (similitud * ponderaciones["tipo_bar"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["tipo_bar"]
  }
  
  # Similitud de bandera
  if (!is.na(fila1$bandera) && !is.na(fila2$bandera)) {
    similitud <- calcular_similitud_combinada(fila1$bandera, fila2$bandera)
    similitud_total <- similitud_total + (similitud * ponderaciones["bandera"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["bandera"]
  }
  
  # Similitud de tonelaje
  if (!is.na(fila1$tn) && !is.na(fila2$tn)) {
    similitud <- calcular_similitud_numerica(fila1$tn, fila2$tn)
    similitud_total <- similitud_total + (similitud * ponderaciones["tn"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["tn"]
  }
  
  # Similitud de rol
  if (!is.na(fila1$rol) && !is.na(fila2$rol)) {
    similitud <- calcular_similitud_combinada(fila1$rol, fila2$rol)
    similitud_total <- similitud_total + (similitud * ponderaciones["rol"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["rol"]
  }
  
  # Calcular similitud final
  if (suma_ponderaciones > 0) {
    resultado <- (similitud_total / suma_ponderaciones) * 100
    return(ifelse(is.na(resultado), 0, resultado))
  } else {
    return(0)
  }
}

# Función para encontrar todas las similitudes por encima del umbral
encontrar_similitudes <- function(df, umbral = 90) {
  n_filas <- nrow(df)
  similitudes <- list()
  
  cat("Calculando similitudes para", n_filas, "registros...\n")
  
  for (i in 1:(n_filas - 1)) {
    if (i %% 100 == 0) {
      cat("Procesando fila", i, "de", n_filas, "\n")
    }
    
    for (j in (i + 1):n_filas) {
      similitud <- calcular_similitud_registros(df[i, ], df[j, ], ponderaciones)
      
      if (similitud >= umbral) {
        similitudes <- append(similitudes, list(data.frame(
          id1 = i,
          id2 = j,
          similitud = similitud,
          master_forename1 = df$master_forename[i],
          master_forename2 = df$master_forename[j],
          master_surname1 = df$master_surname[i],
          master_surname2 = df$master_surname[j],
          nom_barrco1 = df$nom_barrco[i],
          nom_barrco2 = df$nom_barrco[j],
          stringsAsFactors = FALSE
        )))
      }
    }
  }
  
  if (length(similitudes) > 0) {
    return(bind_rows(similitudes))
  } else {
    return(data.frame())
  }
}

# Función para crear clusters a partir de las similitudes
crear_clusters <- function(df_similitudes, df_original) {
  if (nrow(df_similitudes) == 0) {
    # Si no hay similitudes, cada registro es su propio cluster
    df_resultado <- df_original
    df_resultado$cluster <- paste0("MATCH_", sprintf("%02d", 1:nrow(df_original)))
    return(df_resultado)
  }
  
  # Crear grafo de similitudes
  nodos <- unique(c(df_similitudes$id1, df_similitudes$id2))
  
  # Inicializar clusters
  clusters <- list()
  cluster_id <- 1
  visitados <- numeric(0)
  
  # Función para encontrar todos los nodos conectados (DFS)
  encontrar_componente <- function(nodo, df_similitudes, visitados) {
    componente <- c(nodo)
    visitados <- c(visitados, nodo)
    
    # Buscar conexiones
    conexiones_1 <- df_similitudes$id2[df_similitudes$id1 == nodo & !df_similitudes$id2 %in% visitados]
    conexiones_2 <- df_similitudes$id1[df_similitudes$id2 == nodo & !df_similitudes$id1 %in% visitados]
    conexiones <- unique(c(conexiones_1, conexiones_2))
    
    for (conexion in conexiones) {
      if (!conexion %in% visitados) {
        resultado_rec <- encontrar_componente(conexion, df_similitudes, visitados)
        componente <- c(componente, resultado_rec$componente)
        visitados <- resultado_rec$visitados
      }
    }
    
    return(list(componente = unique(componente), visitados = visitados))
  }
  
  # Encontrar componentes conectados
  for (nodo in nodos) {
    if (!nodo %in% visitados) {
      resultado <- encontrar_componente(nodo, df_similitudes, visitados)
      clusters[[cluster_id]] <- resultado$componente
      visitados <- resultado$visitados
      cluster_id <- cluster_id + 1
    }
  }
  
  # Asignar etiquetas de cluster
  df_resultado <- df_original
  df_resultado$cluster <- NA
  
  for (i in 1:length(clusters)) {
    cluster_label <- paste0("MATCH_", sprintf("%02d", i))
    for (indice in clusters[[i]]) {
      df_resultado$cluster[indice] <- cluster_label
    }
  }
  
  # Asignar clusters individuales para registros sin similitudes
  sin_cluster <- which(is.na(df_resultado$cluster))
  if (length(sin_cluster) > 0) {
    inicio_individual <- length(clusters) + 1
    for (i in 1:length(sin_cluster)) {
      cluster_label <- paste0("MATCH_", sprintf("%02d", inicio_individual + i - 1))
      df_resultado$cluster[sin_cluster[i]] <- cluster_label
    }
  }
  
  return(df_resultado)
}

# Función principal de clustering
clustering_capitanes <- function(df, umbral = 90) {
  cat("=== INICIANDO CLUSTERING DE CAPITANES ===\n")
  cat("Umbral de similitud:", umbral, "%\n")
  cat("Total de registros:", nrow(df), "\n\n")
  
  # Verificar columnas requeridas
  columnas_requeridas <- c("master_forename", "master_surname", "tipo_bar", 
                          "nom_barrco", "tn", "bandera", "rol")
  
  columnas_faltantes <- columnas_requeridas[!columnas_requeridas %in% names(df)]
  if (length(columnas_faltantes) > 0) {
    stop("Faltan las siguientes columnas en el dataframe: ", paste(columnas_faltantes, collapse = ", "))
  }
  
  # Encontrar similitudes
  cat("Paso 1: Calculando similitudes...\n")
  df_similitudes <- encontrar_similitudes(df, umbral)
  
  cat("Similitudes encontradas:", nrow(df_similitudes), "\n\n")
  
  # Crear clusters
  cat("Paso 2: Creando clusters...\n")
  df_resultado <- crear_clusters(df_similitudes, df)
  
  # Estadísticas
  n_clusters <- length(unique(df_resultado$cluster))
  cluster_sizes <- table(df_resultado$cluster)
  clusters_multiples <- sum(cluster_sizes > 1)
  
  cat("=== RESULTADOS ===\n")
  cat("Total de clusters creados:", n_clusters, "\n")
  cat("Clusters con múltiples registros:", clusters_multiples, "\n")
  cat("Registros en clusters múltiples:", sum(cluster_sizes[cluster_sizes > 1]), "\n\n")
  
  # Mostrar algunos clusters grandes
  cat("Clusters más grandes:\n")
  clusters_ordenados <- sort(cluster_sizes, decreasing = TRUE)
  for (i in 1:min(5, length(clusters_ordenados))) {
    if (clusters_ordenados[i] > 1) {
      cat("", names(clusters_ordenados)[i], ":", clusters_ordenados[i], "registros\n")
    }
  }
  
  return(list(
    df_resultado = df_resultado,
    df_similitudes = df_similitudes,
    estadisticas = list(
      total_clusters = n_clusters,
      clusters_multiples = clusters_multiples,
      registros_agrupados = sum(cluster_sizes[cluster_sizes > 1])
    )
  ))
}

# Función para analizar un cluster específico
analizar_cluster <- function(df_resultado, cluster_id) {
  registros_cluster <- df_resultado[df_resultado$cluster == cluster_id, ]
  
  cat("=== ANÁLISIS DEL CLUSTER", cluster_id, "===\n")
  cat("Número de registros:", nrow(registros_cluster), "\n\n")
  
  if (nrow(registros_cluster) > 1) {
    cat("Capitanes en el cluster:\n")
    for (i in 1:nrow(registros_cluster)) {
      cat("", i, ":", registros_cluster$master_forename[i], 
          registros_cluster$master_surname[i], 
          "- Barco:", registros_cluster$nom_barrco[i], "\n")
    }
  }
  
  return(registros_cluster)
}

# Ejemplo de uso:
# mi_dataframe <- maristany_select  # Reemplaza con tu dataframe real
# Asumiendo que tu dataframe se llama 'mi_dataframe'
# resultado <- clustering_capitanes(mi_dataframe, umbral = 90)
# df_con_clusters <- resultado$df_resultado
# 
# # Para analizar un cluster específico:
# analizar_cluster(df_con_clusters, "MATCH_01")
# 
# # Para guardar resultados:
# write_csv(df_con_clusters, "capitanes_con_clusters.csv")

cat("Script de clustering listo para usar.\n")
cat("Uso: resultado <- clustering_capitanes(tu_dataframe, umbral = 90)\n")