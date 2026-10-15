# Etapa 2: tablas descriptivas del artículo (tablas 1-6 y 10) y cifras del texto.
source(here::here("R", "00_setup.R"))
reg <- readRDS(ruta("data", "procesado", "registros.rds"))
N <- nrow(reg)

# ---- Tabla 1: entradas por rol y año -----------------------------------------
roles <- c(capitan = "Capitán", patron = "Patrón", piloto = "Piloto", "ilegible o desconocido" = "Ilegible o desconocido")
t1 <- reg |>
  mutate(rol = factor(roles[rol], levels = roles)) |>
  count(rol, anio) |>
  pivot_wider(names_from = anio, values_from = n, values_fill = 0) |>
  mutate(TOTAL = `1852` + `1853`)
t1 <- bind_rows(t1 |> mutate(rol = as.character(rol)), summarise(t1, rol = "TOTAL", across(-rol, sum)))
guardar_tabla(t1, "tabla01_entradas_rol_anio")
cifra("3", "Entradas analizadas", N)
cifra("8", "Porcentaje de entradas de patrones", pct(sum(reg$rol == "patron"), N))

# ---- Nombres distintos por año (sección 3) -----------------------------------
norm_basica <- function(x) {
  x <- str_squish(str_to_upper(x))
  x <- str_replace_all(x, "[.,]", "")
  x <- str_replace_all(x, "[-\u2019']", " ")
  stri_trans_general(x, "Latin-ASCII")
}
unicos <- reg |>
  mutate(nombre_basico = norm_basica(nombre_fuente)) |>
  group_by(anio) |>
  summarise(sin_tratamiento = n_distinct(nombre_fuente), normalizacion_basica = n_distinct(nombre_basico))
guardar_tabla(unicos, "anexo_nombres_distintos_por_anio")
for (i in seq_len(nrow(unicos))) {
  cifra("3", paste("Nombres distintos sin tratamiento", unicos$anio[i]), unicos$sin_tratamiento[i])
  cifra("3", paste("Nombres distintos con normalización básica", unicos$anio[i]), unicos$normalizacion_basica[i])
}

# ---- Tabla 2: número de elementos del nombre en la fuente ---------------------
t2 <- reg |>
  count(elementos = n_elementos_fuente) |>
  mutate(pct = round(100 * n / sum(n), 2))
t2 <- bind_rows(t2 |> mutate(elementos = paste("Con", elementos, if_else(elementos == 1, "elemento", "elementos"))),
                tibble(elementos = "Total", n = sum(t2$n), pct = 100))
guardar_tabla(t2, "tabla02_elementos_nombre")
cifra("5", "Nombres con uno o dos elementos (%)", pct(sum(reg$n_elementos_fuente <= 2), N))
cifra("5", "Nombres con tres o más elementos (%)", pct(sum(reg$n_elementos_fuente >= 3), N))

# ---- Tabla 3: primeras separaciones del LLM -----------------------------------
t3 <- stream_in(file(ruta("data", "raw", "llm_separacion_nombre_apellido_log.jsonl")), verbose = FALSE) |>
  filter(tipo == "resultado") |>
  arrange(fila) |>
  transmute(ID = fila, MASTER_NAME = nombre_original, MASTER_FORENAME, MASTER_SURNAME) |>
  head(10)
guardar_tabla(t3, "tabla03_separacion_llm_ejemplo")
sep_log <- stream_in(file(ruta("data", "raw", "llm_separacion_nombre_apellido_log.jsonl")), verbose = FALSE) |> filter(tipo == "resultado")
cifra("5", "Valores únicos enviados al LLM", nrow(sep_log))
cifra("5", "Minutos de proceso del LLM (suma de tiempos por consulta)", round(sum(sep_log$tiempo_procesamiento_seg) / 60, 1))

# ---- Tabla 4: normalización término a término --------------------------------
cand <- readRDS(ruta("data", "procesado", "candidatos_jw.rds")) |>
  transmute(NOMBRE_ORIG = nombre_original, NOMBRE_DICC = nombre_corregido, JARO_WINKLER = similitud_jaro_winkler, CHR_DIF = chr_dif) |>
  arrange(NOMBRE_ORIG)
guardar_tabla(cand, "tabla04_candidatos_jw_completa")
extracto_t4 <- c("BEIR", "BELAGUER", "BELENGUER", "BELLIDO", "BELTRAN", "BENANCIO", "BENASA", "BENASAR", "BENASCO",
                 "BENDRICH", "BENESAR", "BENNASAR", "BENNESAR", "BERENGUER", "BERG")
guardar_tabla(filter(cand, NOMBRE_ORIG %in% extracto_t4), "tabla04_candidatos_jw_extracto")

# ---- Tabla 5: variantes de Bennasar ------------------------------------------
t5 <- reg |>
  filter(apellido_manual == "Bennasar") |>
  transmute(MASTER_FORENAME = nombre_manual, MASTER_SURNAME = apellido_fuente, MASTER_SURNAME_REV = apellido_manual, buque)
guardar_tabla(t5, "tabla05_variantes_bennasar")

# ---- Apellidos distintos antes y después de la normalización (sección 6) ------
cifra("6", "Apellidos distintos sin tratamiento", n_distinct(reg$apellido_fuente, na.rm = TRUE))
cifra("6", "Entradas por apellido sin tratamiento", formatC(N / n_distinct(reg$apellido_fuente, na.rm = TRUE), format = "f", digits = 2, decimal.mark = ","))
cifra("6", "Apellidos distintos tras la normalización manual", n_distinct(reg$apellido_manual))
cifra("6", "Registros por apellido tras la normalización", formatC(N / n_distinct(reg$apellido_manual), format = "f", digits = 2, decimal.mark = ","))

# ---- Diccionarios de referencia (sección 6) ------------------------------------
source(ruta("R", "opcional_recalculo_fuzzy.R"))
dic <- ruta("data", "diccionarios", "apellidos")
idescat <- read_delim(file.path(dic, "idescat_apellidos_cataluna_2024.csv"), delim = ";", skip = 8, show_col_types = FALSE)
ine <- c(read_csv(file.path(dic, "ine_padron2015_apellidos_frec_100_o_mas.csv"), show_col_types = FALSE)$surname,
         read_csv(file.path(dic, "ine_padron2015_apellidos_frec_20_a_99.csv"), show_col_types = FALSE)$surnames,
         read_csv(file.path(dic, "ine_marcboquet_apellidos_frec_100_o_mas.csv"), show_col_types = FALSE)$apellido,
         read_csv(file.path(dic, "ine_marcboquet_apellidos_frec_20_a_99.csv"), show_col_types = FALSE)$apellido)
frec_idescat <- suppressWarnings(as.numeric(gsub("\\.", "", idescat[[3]])))
cifra("6", "Apellidos distintos en los listados del INE (frecuencia >= 20)", n_distinct(str_to_upper(ine)))
cifra("6", "Apellidos distintos en el listado del Idescat 2024", n_distinct(idescat$Cognom))
cifra("6", "Frecuencia mínima en el listado del Idescat", min(frec_idescat, na.rm = TRUE))
cifra("6", "Elementos de apellido distintos en el diccionario unificado INE + Idescat", length(diccionario_apellidos()))
cifra("6", "Elementos de nombre distintos en el diccionario de nombres de varón", length(diccionario_nombres()))

# ---- Tabla 6: indicador de concentración de los 20 apellidos más frecuentes ---
conc <- reg |>
  group_by(Apellido = apellido_manual) |>
  summarise(N_manual = n(), Individuos = n_distinct(id_manual, na.rm = TRUE), sin_id = sum(is.na(id_manual)), .groups = "drop") |>
  mutate(Indicador = round((N_manual - Individuos) / (N_manual - 1), 3))
top20 <- conc |> slice_max(N_manual, n = 20, with_ties = FALSE)
stopifnot(all(top20$sin_id == 0))
top20$N_automatico <- vapply(norm_txt(top20$Apellido), function(a) sum(reg$apellido_auto_n == a), integer(1))
t6 <- top20 |> select(Apellido, N_manual, N_automatico, Individuos, Indicador) |> arrange(Indicador)
guardar_tabla(t6, "tabla06_concentracion_top20")
saveRDS(t6, ruta("data", "procesado", "concentracion_top20.rds"))
cifra("7", "Apellidos con identificador de individuo en la revisión manual", n_distinct(reg$apellido_manual[!is.na(reg$id_manual)]))
cifra("7", "Individuos identificados en la revisión manual", n_distinct(reg$id_manual, na.rm = TRUE))

# ---- Tabla 10: formas nominales de Sorolla por buque -------------------------
t10 <- reg |>
  filter(apellido_manual == "Sorolla") |>
  count(Nombres = nombre_manual, buque) |>
  pivot_wider(names_from = buque, values_from = n, values_fill = 0) |>
  mutate(Total = rowSums(across(-Nombres)))
t10 <- bind_rows(t10, summarise(t10, Nombres = "Total", across(-Nombres, sum)))
guardar_tabla(t10, "tabla10_sorolla_formas_por_buque")

# ---- Casos del texto: Medinas y Sorolla --------------------------------------
med <- reg |> filter(apellido_manual == "Medinas")
guardar_tabla(count(med, id_manual, nombre_manual, buque, tipo_buque), "anexo_caso_medinas")
med1 <- med |> filter(id_manual == "MEDINAS0001") |> arrange(fecha_llegada)
cifra("7", "Medinas (Gabriel): llegadas a Barcelona", nrow(med1))
cifra("7", "Medinas (Gabriel): días entre llegadas (mediana)", median(as.integer(diff(med1$fecha_llegada))))
sor <- reg |> filter(apellido_manual == "Sorolla")
guardar_tabla(sor |> group_by(id_manual, buque, tipo_buque) |>
  summarise(entradas = n(), tonelajes = paste(sort(unique(tonelaje)), collapse = "/"),
            puertos = paste(names(sort(table(puerto_salida), decreasing = TRUE))[1:2], collapse = ", "), .groups = "drop"),
  "anexo_caso_sorolla")

# ---- Duraciones en horas registradas como días (corrección de la etapa 1) ------
horas <- reg |> filter(horas_leidas_como_dias)
guardar_tabla(horas |> transmute(ID, apellido_manual, buque, tipo_buque, puerto_salida, fecha_salida_original,
                                 fecha_salida_corregida = fecha_salida, fecha_llegada, horas_texto, texto = str_sub(texto, 1, 120)),
              "anexo_correccion_duraciones_en_horas")
cifra("3", "Noticias con la duración expresada en horas", sum(!is.na(reg$horas_texto)))
cifra("3", "Duraciones en horas registradas como días (corregidas)", nrow(horas))

# ---- Proyección histórica (conclusiones) -------------------------------------
vap <- reg |> filter(str_detect(tipo_buque, "vapor"))
top10_vap <- vap |> count(apellido_manual, sort = TRUE) |> head(10)
guardar_tabla(vap |> count(apellido_manual, sort = TRUE), "anexo_vapores_entradas_por_apellido")
cifra("8", "Entradas de vapores", nrow(vap))
cifra("8", "Entradas de vapores (% del total)", pct(nrow(vap), N))
cifra("8", "Entradas de vapores de los 10 apellidos más frecuentes (%)", pct(sum(top10_vap$n), nrow(vap)))

guardar_cifras("02_tablas_descriptivas")
