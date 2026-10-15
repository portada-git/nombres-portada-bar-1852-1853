# Etapa 1: construye la base de registros con la vía manual y la vía automática.
#
# Entradas
#   data/raw/entradas_1852_1853_revision_manual.xlsx            registros 1852-1853 con la revisión manual (CognomA REV, CognomID)
#   data/raw/entradas_1852_1853_nombre_llm.rds   mismos registros + nombre extraído por LLM (columna nombre_capitan)
#   data/raw/llm_separacion_nombre_apellido_log.jsonl  separación nombre/apellido hecha con gpt-4o-mini (agosto de 2025)
#   data/intermedio/candidatos_jw_*_2025.rds   candidatos Jaro-Winkler guardados en 2025 (ver README)
# Salida
#   data/procesado/registros.rds   una fila por entrada de buque (6.902)
source(here::here("R", "00_setup.R"))

RECALCULAR_FUZZY <- identical(Sys.getenv("RECALCULAR_FUZZY"), "1")

# ---- Vía manual --------------------------------------------------------------
manual <- read_excel(ruta("data", "raw", "entradas_1852_1853_revision_manual.xlsx"), sheet = "Full1") |>
  limpiar_nombres() |>
  transmute(
    ID = as.integer(ID),
    fecha_pub = as.Date(Data_publicacio),
    anio = as.integer(format(fecha_pub, "%Y")),
    fecha_salida = as.Date(Data_sortida),
    fecha_llegada = as.Date(Data_arribada),
    puerto_salida = Port_sortida_REV,
    tipo_buque = Tipus_embarcacio_REV,
    buque = Nom_embarcacio_REV,
    tonelaje = suppressWarnings(as.integer(str_remove_all(Tonatge_embarcacio, "[^0-9]"))),
    rol = Carrec_del_responsnable_REV,
    nombre_fuente = Nom_del_responsable,
    nombre_manual = Nom_desagregat_REV,
    apellido_fuente = CognomA_desagregat,
    apellido_manual = CognomA_REV,
    id_manual = CognomID,
    texto = Text_cru
  )
stopifnot(nrow(manual) == 6902, !anyDuplicated(manual$ID))

# Corrección de duraciones: algunas noticias expresan el viaje en horas ("De Palma en 18 horas",
# a menudo leído por el OCR como "boras") y la extracción lo registró como días. Solo se corrige
# cuando los días registrados coinciden con las horas del texto; la fecha original se conserva.
patron_horas <- regex("\\ben\\s+(\\d+)\\s*(horas?|boras?|hores|heras|horns|h\\.)", ignore_case = TRUE)
manual <- manual |>
  mutate(
    horas_texto = suppressWarnings(as.integer(str_match(texto, patron_horas)[, 2])),
    dias_registrados = as.integer(fecha_llegada - fecha_salida),
    horas_leidas_como_dias = (dias_registrados == horas_texto) %in% TRUE,
    fecha_salida_original = fecha_salida,
    fecha_salida = if_else(horas_leidas_como_dias, fecha_llegada - ceiling(horas_texto / 24), fecha_salida)
  ) |>
  select(-dias_registrados)

# ---- Vía automática ----------------------------------------------------------
# 1. Nombre extraído por el LLM y preprocesamiento básico.
auto <- readRDS(ruta("data", "raw", "entradas_1852_1853_nombre_llm.rds")) |>
  limpiar_nombres() |>
  transmute(ID = as.integer(ID), nombre_capitan)
x <- str_replace_all(auto$nombre_capitan, "[.,-]", " ")
auto$nombre_prepro <- str_to_upper(str_squish(stri_trans_general(x, "Latin-ASCII")))

# 2. Segmentación en términos y normalización término a término.
nombres_unicos <- auto |> count(nombre_prepro, sort = TRUE) |> mutate(id_master_name = row_number())
terminos <- nombres_unicos |>
  mutate(termino = str_split(nombre_prepro, " ")) |>
  unnest(termino) |>
  group_by(id_master_name) |>
  mutate(posicion = row_number()) |>
  ungroup()

if (RECALCULAR_FUZZY) {
  source(ruta("R", "opcional_recalculo_fuzzy.R"))
  cand_ap <- candidatos_jw(unique(terminos$termino), diccionario_apellidos())
  cand_no <- candidatos_jw(unique(terminos$termino), diccionario_nombres())
} else {
  cand_ap <- readRDS(ruta("data", "intermedio", "candidatos_jw_apellidos_2025.rds"))
  cand_no <- readRDS(ruta("data", "intermedio", "candidatos_jw_nombres_2025.rds"))
}

# Mejor candidato entre apellidos y nombres; umbrales que combinan similitud y diferencia de longitud.
candidatos <- bind_rows(cand_ap, cand_no) |>
  distinct() |>
  mutate(chr_dif = abs(nchar(nombre_original) - nchar(nombre_corregido))) |>
  group_by(nombre_original) |>
  slice_max(similitud_jaro_winkler, n = 1, with_ties = FALSE) |>
  ungroup()
correcciones <- candidatos |>
  filter(
    (similitud_jaro_winkler >= 0.95 & chr_dif == 1 & nchar(nombre_original) > 8) |
      (similitud_jaro_winkler >= 0.96 & chr_dif == 1 & nchar(nombre_original) > 6) |
      (similitud_jaro_winkler >= 0.97 & chr_dif == 1 & nchar(nombre_original) > 4) |
      (similitud_jaro_winkler >= 0.85 & chr_dif == 0)
  ) |>
  select(termino = nombre_original, termino_norm = nombre_corregido)
saveRDS(candidatos, ruta("data", "procesado", "candidatos_jw.rds"))

nombres_unicos <- terminos |>
  left_join(correcciones, by = "termino") |>
  mutate(termino_norm = coalesce(termino_norm, termino)) |>
  arrange(id_master_name, posicion) |>
  group_by(id_master_name) |>
  summarise(nombre_norm = paste(termino_norm, collapse = " "), .groups = "drop") |>
  right_join(nombres_unicos, by = "id_master_name")

# 3. Separación nombre/apellido con el LLM (resultado guardado; no se vuelve a consultar la API).
separacion <- stream_in(file(ruta("data", "raw", "llm_separacion_nombre_apellido_log.jsonl")), verbose = FALSE) |>
  filter(tipo == "resultado") |>
  select(nombre_norm = nombre_original, MASTER_FORENAME, MASTER_SURNAME, estado_llm = estado)
stopifnot(!anyDuplicated(separacion$nombre_norm))

auto <- auto |>
  left_join(nombres_unicos |> select(nombre_prepro, nombre_norm), by = "nombre_prepro") |>
  left_join(separacion, by = "nombre_norm")
sin_llm <- is.na(auto$MASTER_SURNAME)
if (RECALCULAR_FUZZY) {
  # Nombres normalizados de forma distinta a 2025: el LLM nunca los vio. Se separan con la
  # regla simple (primer elemento = nombre, último = apellido) y se marcan en estado_llm.
  auto <- auto |>
    mutate(
      estado_llm = if_else(sin_llm, "sin_llm_regla", estado_llm),
      MASTER_FORENAME = if_else(sin_llm & str_count(nombre_norm, "\\S+") > 1, word(nombre_norm, 1), MASTER_FORENAME),
      MASTER_SURNAME = if_else(sin_llm, word(nombre_norm, -1), MASTER_SURNAME)
    )
  message("Entradas sin separación del LLM (resueltas con la regla): ", sum(sin_llm))
} else {
  stopifnot(!any(sin_llm))
}
auto$apellido_llm_bruto <- auto$MASTER_SURNAME

# 4. Correcciones posteriores a la salida del modelo (mismas reglas que en 2025).
vacio <- function(x) is.na(x) | x == "" | x == "<empty>"
auto <- auto |>
  mutate(
    sv = vacio(MASTER_SURNAME),
    MASTER_SURNAME = if_else(sv, word(MASTER_FORENAME, -1), MASTER_SURNAME),
    MASTER_FORENAME = if_else(sv, str_trim(str_remove(MASTER_FORENAME, "\\s+\\S+$")), MASTER_FORENAME),
    e1 = str_starts(MASTER_SURNAME, "1\\s") %in% TRUE,
    MASTER_FORENAME = if_else(e1, paste(MASTER_FORENAME, "1"), MASTER_FORENAME),
    MASTER_SURNAME = if_else(e1, str_trim(str_remove(MASTER_SURNAME, "^1\\s+")), MASTER_SURNAME),
    mv = vacio(MASTER_FORENAME) & str_count(MASTER_SURNAME, "\\S+") >= 2,
    MASTER_FORENAME = if_else(mv, word(MASTER_SURNAME, 1), MASTER_FORENAME),
    MASTER_SURNAME = if_else(mv, str_trim(word(MASTER_SURNAME, 2, -1)), MASTER_SURNAME)
  ) |>
  select(-sv, -e1, -mv)

registros <- manual |>
  left_join(auto, by = "ID") |>
  mutate(
    apellido_manual_n = norm_txt(apellido_manual),
    apellido_auto_n = norm_txt(MASTER_SURNAME),
    n_elementos_fuente = str_count(str_squish(nombre_fuente), "\\S+"),
    n_elementos_norm = str_count(nombre_norm, "\\S+")
  )
stopifnot(nrow(registros) == 6902)
saveRDS(registros, ruta("data", "procesado", "registros.rds"))
message("registros.rds: ", nrow(registros), " filas; fuzzy ", if (RECALCULAR_FUZZY) "recalculado" else "desde candidatos guardados")
