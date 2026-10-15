# Etapa 4: desambiguación automática de Maristany (similitud ponderada + componentes conexas)
# y comparación con la revisión manual (tablas 7, 8 y 9).
source(here::here("R", "00_setup.R"))
reg <- readRDS(ruta("data", "procesado", "registros.rds"))

# Algoritmo de 2025 sin cambios: ponderaciones, similitud Jaro-Winkler + Soundex, umbral del 90%.
invisible(capture.output(source(ruta("R", "clustering_similitud.R"))))

# Orden de filas del análisis original (alfabético por nombre preprocesado, estable respecto del .rds);
# solo afecta a la numeración de las etiquetas MATCH_xx.
orden_rds <- readRDS(ruta("data", "raw", "entradas_1852_1853_nombre_llm.rds"))$ID
m <- reg |>
  filter(str_detect(nombre_prepro, "MARISTAN"), !str_detect(nombre_prepro, "PAGES")) |>
  mutate(orden = match(ID, orden_rds)) |>
  arrange(nombre_prepro, orden)

# Limpieza aplicada en 2025 antes de comparar (iniciales, "1" leído por "I", tratamiento "D").
m <- m |>
  mutate(
    master_forename = str_squish(str_replace(MASTER_FORENAME, "\\.", "")),
    master_surname = str_squish(str_replace(MASTER_SURNAME, "\\.", "")),
    master_forename = str_replace(master_forename, "1", "I"),
    master_surname = str_replace(master_surname, "1", "I"),
    master_forename = str_remove(master_forename, "^D\\s"),
    nom_barrco = if_else(buque == "Virgen", "Virgen del Mar", buque),
    tipo_bar = tipo_buque,
    tn = tonelaje,
    bandera = "espanola"
  )
for (p in c("EDRO", "GUSTIN", "OMAS", "SIDRO", "ERARDO", "SE", "IME")) m$master_forename <- str_remove(m$master_forename, p)
stopifnot(nrow(m) == 67)

res <- clustering_capitanes(as.data.frame(m), umbral = 90)$df_resultado |> as_tibble()
saveRDS(res, ruta("data", "procesado", "maristany_clusters.rds"))
guardar_tabla(res |> select(ID, nombre_fuente, master_forename, master_surname, buque, tipo_buque, tonelaje, rol,
                            fecha_salida, fecha_llegada, id_manual, cluster), "anexo_maristany_registros_y_clusters")

# ---- Tabla 7: variables y ponderaciones --------------------------------------
t7 <- tibble(
  Campo = c("MASTER_FORENAME", "MASTER_SURNAME", "SHIP_NAME", "SHIP_TYPE", "SHIP_FLAG", "SHIP_TONNAGE", "MASTER_ROLE"),
  Descripcion = c("Nombre del capitán", "Apellido del capitán", "Nombre del barco", "Tipo de barco", "Bandera", "Tonelaje", "Rol"),
  Ponderacion = unname(ponderaciones[c("master_forename", "master_surname", "nom_barrco", "tipo_bar", "bandera", "tn", "rol")])
)
guardar_tabla(t7, "tabla07_ponderaciones_clustering")

# ---- Tablas 8 y 9: manual frente a automático --------------------------------
cruce <- res |> filter(!is.na(id_manual)) |> count(Manual = id_manual, Automatico = cluster, name = "N")
guardar_tabla(cruce, "tabla08_maristany_cruce_completo")
guardar_tabla(cruce |> filter(N >= 4) |> arrange(desc(N)), "tabla08_maristany_cruce_extracto")
divididos <- cruce |> group_by(Manual) |> filter(n_distinct(Automatico) > 1) |> ungroup()
t9 <- divididos |> left_join(res |> distinct(cluster, master_forename, master_surname), by = c("Automatico" = "cluster")) |>
  transmute(Manual, Automatico, Nombre = paste(master_forename, master_surname))
guardar_tabla(t9, "tabla09_maristany_discrepancia")

# ---- Métricas de concordancia sobre los registros comunes ---------------------
ids_manual <- reg$ID[reg$apellido_manual %in% "Maristany"]
comun <- res |> filter(ID %in% ids_manual, !is.na(id_manual))
pares <- combn(nrow(comun), 2)
mismo_m <- comun$id_manual[pares[1, ]] == comun$id_manual[pares[2, ]]
mismo_a <- comun$cluster[pares[1, ]] == comun$cluster[pares[2, ]]
tp <- sum(mismo_m & mismo_a); fp <- sum(!mismo_m & mismo_a); fn <- sum(mismo_m & !mismo_a)
ct <- table(comun$id_manual, comun$cluster)
c2 <- function(v) sum(choose(v, 2))
a <- c2(rowSums(ct)); b <- c2(colSums(ct)); esperado <- a * b / choose(sum(ct), 2)
ari <- (c2(ct) - esperado) / ((a + b) / 2 - esperado)
exactos <- sum(vapply(unique(comun$id_manual), function(id) {
  cl <- unique(comun$cluster[comun$id_manual == id])
  length(cl) == 1 && all(comun$id_manual[comun$cluster == cl] == id)
}, logical(1)))
solo_auto <- res |> filter(!ID %in% ids_manual)
solo_manual <- reg |> filter(ID %in% setdiff(ids_manual, res$ID))
guardar_tabla(bind_rows(solo_auto |> transmute(ID, conjunto = "solo automático", nombre_fuente, apellido_manual, texto),
                        solo_manual |> transmute(ID, conjunto = "solo manual", nombre_fuente, apellido_manual, texto)),
              "anexo_maristany_registros_no_comunes")

metricas <- tibble(
  indicador = c("registros automáticos", "registros manuales", "registros comunes", "individuos manuales (total)",
                "clusters automáticos (total)", "individuos manuales (comunes)", "clusters automáticos (comunes)",
                "individuos reproducidos exactamente", "pares posibles", "pares unidos por ambos", "falsos positivos",
                "falsos negativos", "precisión", "exhaustividad", "índice de Rand ajustado", "IC manual"),
  valor = c(nrow(res), length(ids_manual), nrow(comun), n_distinct(reg$id_manual[reg$ID %in% ids_manual]),
            n_distinct(res$cluster), n_distinct(comun$id_manual), n_distinct(comun$cluster), exactos, ncol(pares),
            tp, fp, fn, round(tp / (tp + fp), 2), round(tp / (tp + fn), 2), round(ari, 2),
            round((67 - n_distinct(reg$id_manual[reg$ID %in% ids_manual])) / 66, 3))
)
guardar_tabla(metricas, "anexo_maristany_metricas_concordancia")
for (i in seq_len(nrow(metricas))) cifra("7", paste("Maristany:", metricas$indicador[i]), metricas$valor[i])

# ---- Cifras citadas en el texto -----------------------------------------------
mar <- reg |> filter(apellido_manual == "Maristany")
inicial <- str_sub(str_replace(str_remove(mar$nombre_manual, "^D\\.?\\s*"), "^1", "I"), 1, 1)
cifra("7", "Maristany: registros con inicial J (revisión manual)", sum(inicial == "J", na.rm = TRUE))
cifra("7", "Maristany: registros con inicial I (revisión manual, «1.» leído como I)", sum(inicial == "I", na.rm = TRUE))
borigua <- mar |> filter(buque == "Borigua", fecha_llegada == as.Date("1852-10-26"))
carmen <- mar |> filter(buque == "Carmen", fecha_llegada == as.Date("1852-09-19"))
cifra("7", "Borigua: días de viaje desde Santander (llegada 26/10/1852)", as.integer(borigua$fecha_llegada - borigua$fecha_salida))
cifra("7", "Días entre la llegada del laúd Carmen (19/9/1852) y la salida de la Borigua", as.integer(borigua$fecha_salida - carmen$fecha_llegada))
astarve <- mar |> filter(buque == "Astarve"); industria <- mar |> filter(buque == "Industria") |> slice_min(fecha_llegada)
cifra("7", "Industria: días de viaje desde Nueva Orleans", as.integer(industria$fecha_llegada - industria$fecha_salida))
cifra("7", "Astarve: fecha de llegada", format(astarve$fecha_llegada, "%d/%m/%Y"))

guardar_cifras("04_desambiguacion_maristany")
