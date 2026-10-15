# Etapa 3: concordancia entre la vía automática (fuzzy + LLM) y la revisión manual en el apellido,
# y comparación del LLM con una regla simple de expresiones regulares.
source(here::here("R", "00_setup.R"))
reg <- readRDS(ruta("data", "procesado", "registros.rds"))
N <- nrow(reg)

ev <- reg |>
  mutate(
    llm = apellido_auto_n,
    regex = word(nombre_norm, -1),            # regla: último elemento = apellido
    manual = apellido_manual_n,
    grupo = case_when(n_elementos_norm <= 2 ~ "1-2 elementos", n_elementos_norm == 3 ~ "3 elementos", TRUE ~ "4 o más elementos"),
    literal = mapply(function(m, s) str_detect(s, fixed(m)), manual, nombre_norm)
  )

global <- tibble(
  metodo = c("Cadena automática con LLM", "Regla de expresiones regulares"),
  coincidencias = c(sum(ev$llm == ev$manual), sum(ev$regex == ev$manual)),
  entradas = N
) |> mutate(pct = round(100 * coincidencias / entradas, 2))
guardar_tabla(global, "anexo_evaluacion_apellido_global")

por_grupo <- ev |>
  group_by(grupo) |>
  summarise(entradas = n(), llm = sum(llm == manual), regex = sum(regex == manual), .groups = "drop") |>
  mutate(pct_llm = round(100 * llm / entradas, 1), pct_regex = round(100 * regex / entradas, 1))
guardar_tabla(por_grupo, "anexo_evaluacion_apellido_por_elementos")

top20 <- readRDS(ruta("data", "procesado", "concentracion_top20.rds"))$Apellido |> norm_txt()
ev20 <- ev |> filter(manual %in% top20)

# Error típico del modelo: incorpora al apellido la inicial que sigue al tratamiento "D".
err_d <- ev |> filter(n_elementos_norm == 3, llm != manual, regex == manual, str_detect(nombre_norm, "^D "),
                      word(llm, -1) == manual, str_count(llm, "\\S+") == 2)
guardar_tabla(ev |> filter(llm != manual) |> select(ID, nombre_fuente, nombre_capitan, nombre_norm, manual, llm, regex),
              "anexo_evaluacion_apellido_discrepancias")

cifra("5", "Coincidencia exacta del apellido (entradas)", global$coincidencias[1])
cifra("5", "Coincidencia exacta del apellido (%)", pct(global$coincidencias[1], N))
cifra("5", "Coincidencia con regla de expresiones regulares (%)", pct(global$coincidencias[2], N))
g4 <- por_grupo |> filter(grupo == "4 o más elementos")
cifra("5", "Nombres de 4 o más elementos: acierto del LLM (%)", pct(g4$llm, g4$entradas))
cifra("5", "Nombres de 4 o más elementos: acierto de la regla (%)", pct(g4$regex, g4$entradas))
cifra("5", "Errores del LLM por la inicial tras 'D' (3 elementos, la regla acierta)", nrow(err_d))
cifra("5", "Apellido manual ausente literalmente de la cadena procesada (%)", pct(sum(!ev$literal), N))
cifra("8", "Coincidencia en los 20 apellidos más frecuentes", paste0(sum(ev20$llm == ev20$manual), " de ", nrow(ev20)))
cifra("8", "Coincidencia en los 20 apellidos más frecuentes (%)", pct(sum(ev20$llm == ev20$manual), nrow(ev20)))
cifra("6", "Apellidos distintos según la vía automática", n_distinct(ev$llm))

guardar_cifras("03_evaluacion_apellidos")
