# Configuración común: paquetes, rutas y funciones auxiliares.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(readxl)
  library(jsonlite)
  library(stringdist)
  library(ggplot2)
  library(ggrepel)
  library(here)
})

ruta <- function(...) here::here(...)

dir.create(ruta("data", "procesado"), showWarnings = FALSE, recursive = TRUE)
dir.create(ruta("outputs", "tablas"), showWarnings = FALSE, recursive = TRUE)
dir.create(ruta("outputs", "figuras"), showWarnings = FALSE, recursive = TRUE)

# Normalización usada para comparar apellidos manuales y automáticos.
norm_txt <- function(x) str_squish(str_to_upper(stri_trans_general(x, "Latin-ASCII")))

# Nombres de columna sin acentos ni espacios (los originales están en catalán).
limpiar_nombres <- function(df) {
  names(df) <- make.names(iconv(names(df), "UTF-8", "ASCII//TRANSLIT"))
  names(df) <- gsub("\\.", "_", names(df))
  df
}

guardar_tabla <- function(df, nombre) {
  write_excel_csv2(df, ruta("outputs", "tablas", paste0(nombre, ".csv")))
  invisible(df)
}

# Registro de las cifras citadas en el artículo: cada etapa guarda las suyas en outputs/cifras/.
.cifras <- new.env()
.cifras$tabla <- tibble(seccion = character(), indicador = character(), valor = character())
cifra <- function(seccion, indicador, valor) {
  if (is.numeric(valor)) valor <- format(valor, decimal.mark = ",", big.mark = "", scientific = FALSE, trim = TRUE)
  .cifras$tabla <- bind_rows(.cifras$tabla, tibble(seccion = seccion, indicador = indicador, valor = as.character(valor)))
  invisible(valor)
}
guardar_cifras <- function(etapa) {
  dir.create(ruta("outputs", "cifras"), showWarnings = FALSE)
  write_excel_csv2(.cifras$tabla, ruta("outputs", "cifras", paste0(etapa, ".csv")))
}

pct <- function(x, n, d = 1) formatC(100 * x / n, format = "f", digits = d, decimal.mark = ",")

# Tipografía de todas las figuras: Arial 11 (Liberation Sans si Arial no está instalada).
fuente <- if ("Arial" %in% systemfonts::system_fonts()$family) "Arial" else "Liberation Sans"
gris_texto <- "grey25"

tema_articulo <- theme_minimal(base_size = 11, base_family = fuente) +
  theme(
    text = element_text(colour = gris_texto),
    axis.text = element_text(size = 9, colour = "grey35"),
    axis.title = element_text(size = 10, colour = gris_texto),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    plot.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 9),
    plot.margin = margin(4, 8, 4, 4)
  )
