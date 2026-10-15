# Ejecuta todo el flujo de análisis y regenera tablas, figuras y cifras del artículo.
# Uso (desde la raíz del proyecto):  Rscript run_all.R
# Para recalcular el fuzzy matching desde los diccionarios: RECALCULAR_FUZZY=1 Rscript run_all.R
etapas <- c("01_construccion_registros.R", "02_tablas_descriptivas.R", "03_evaluacion_apellidos.R", "04_desambiguacion_maristany.R", "05_figuras.R")
for (e in etapas) {
  message("== ", e)
  local(source(here::here("R", e), encoding = "UTF-8", local = TRUE))
}

suppressPackageStartupMessages({ library(dplyr); library(readr) })
cifras <- list.files(here::here("outputs", "cifras"), pattern = "^0.*\\.csv$", full.names = TRUE) |>
  lapply(read_delim, delim = ";", col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) |>
  bind_rows() |>
  arrange(as.integer(seccion))
write_excel_csv2(cifras, here::here("outputs", "cifras_articulo.csv"))
message("Listo: ", nrow(cifras), " cifras en outputs/cifras_articulo.csv")
