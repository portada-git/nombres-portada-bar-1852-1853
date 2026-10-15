# Recalcula los candidatos Jaro-Winkler desde los diccionarios del INE y del Idescat.
# Se usa solo con la variable de entorno RECALCULAR_FUZZY=1.
#
# Reproduce exactamente el procedimiento usado en 2025: los diccionarios se construyen igual que
# entonces y la similitud se calcula con RecordLinkage::jarowinkler, de modo que los candidatos
# coinciden con los guardados en data/intermedio/. Si RecordLinkage no está instalado se usa
# stringdist (método "jw", p = 0,1), que difiere levemente.

diccionario_apellidos <- function() {
  d <- ruta("data", "diccionarios", "apellidos")
  cat_ <- read_delim(file.path(d, "idescat_apellidos_cataluna_2024.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE, skip = 8, show_col_types = FALSE)
  ap <- c(
    str_to_upper(cat_$Cognom),
    str_to_upper(read_csv(file.path(d, "ine_marcboquet_apellidos_frec_20_a_99.csv"), show_col_types = FALSE)$apellido),
    str_to_upper(read_csv(file.path(d, "ine_marcboquet_apellidos_frec_100_o_mas.csv"), show_col_types = FALSE)$apellido),
    str_to_upper(read_csv(file.path(d, "ine_padron2015_apellidos_frec_20_a_99.csv"), show_col_types = FALSE)$surnames),
    str_to_upper(read_csv(file.path(d, "ine_padron2015_apellidos_frec_100_o_mas.csv"), show_col_types = FALSE)$surname)
  )
  ap <- sort(unique(ap))
  ap <- ap[ap != ""]
  tok <- tibble(value = ap) |>
    separate(value, into = paste0("apellido", 1:5), sep = " ", fill = "right", extra = "merge") |>
    pivot_longer(starts_with("apellido"), values_to = "apellido", values_drop_na = TRUE) |>
    distinct(apellido)
  stri_trans_general(tok$apellido, "Latin-ASCII")
}

diccionario_nombres <- function() {
  d <- ruta("data", "diccionarios", "nombres")
  h <- read_csv(file.path(d, "ine_marcboquet_nombres_hombres.csv"), show_col_types = FALSE)
  names(h) <- c("name", "frequency", "mean_age")
  nm <- rbind(read_csv(file.path(d, "ine_padron2015_nombres_hombres.csv"), show_col_types = FALSE), h) |> distinct(name, .keep_all = TRUE)
  nm$name <- stri_trans_general(nm$name, "Latin-ASCII")
  tok <- nm |>
    separate(name, into = paste0("nombre", 1:3), sep = " ", fill = "right", extra = "merge") |>
    pivot_longer(starts_with("nombre"), values_to = "nombre", values_drop_na = TRUE) |>
    distinct(nombre)
  tok$nombre
}

# Mejor coincidencia de cada término (sin umbral; los umbrales se aplican en 01_construccion_registros.R).
candidatos_jw <- function(terminos, diccionario) {
  dic_min <- trimws(tolower(diccionario))
  if (requireNamespace("RecordLinkage", quietly = TRUE)) {
    res <- lapply(trimws(tolower(terminos)), function(t) {
      s <- RecordLinkage::jarowinkler(t, dic_min)
      i <- which.max(s)
      c(i, s[i])
    })
    idx <- vapply(res, `[`, numeric(1), 1)
    sim <- vapply(res, `[`, numeric(1), 2)
  } else {
    message("RecordLinkage no está instalado: se usa stringdist (resultados levemente distintos)")
    sim_m <- lapply(split(terminos, ceiling(seq_along(terminos) / 200)), function(x)
      1 - stringdistmatrix(tolower(x), dic_min, method = "jw", p = 0.1))
    sim_m <- do.call(rbind, sim_m)
    idx <- max.col(sim_m, ties.method = "first")
    sim <- sim_m[cbind(seq_along(terminos), idx)]
  }
  # Como en fuzzy.R: por debajo de 0,75 se conserva el término original.
  tibble(nombre_original = terminos, nombre_corregido = if_else(sim >= 0.75, diccionario[idx], terminos),
         similitud_jaro_winkler = round(sim, 3))
}
