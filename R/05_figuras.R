# Etapa 5: figuras 1 a 7 del artículo (Arial; ancho de 16 cm, apto para página A4).
source(here::here("R", "00_setup.R"))
reg <- readRDS(ruta("data", "procesado", "registros.rds"))
t6 <- readRDS(ruta("data", "procesado", "concentracion_top20.rds"))

guardar_fig <- function(p, nombre, ancho = 16, alto = 9) {
  ggsave(ruta("outputs", "figuras", paste0(nombre, ".png")), p, device = ragg::agg_png,
         width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
}
pt <- function(x) x / .pt   # tamaño de texto en puntos para geom_text
azul <- "#2F5C8A"
# Paleta sobria y distinguible (también para daltonismo); un color por buque.
paleta_buques <- c("#2F5C8A", "#7FA7CF", "#B5534A", "#D9A441", "#5E8C61", "#8A6FA5", "#8C8C8C")

# ---- Figura 1: flujo de preprocesamiento, normalización y desambiguación ------
# Diagrama conceptual. Coordenadas en milímetros sobre un lienzo de 160 x 72.
campos <- c("Cadena original", "Cadena preprocesada", "Nombre de pila", "Apellido", "Apellido normalizado", "ID de individuo")
cajas <- tibble(
  n = c(1, 2, 4, 5, 6),
  xmin = c(0, 0, 0, 63, 126), xmax = c(34, 34, 34, 97, 160),
  ymax = c(70, 48, 23, 27, 31)
) |>
  mutate(ymin = ymax - (6 + 3.7 * n), fill = c("#F2F5F9", "#E9EFF6", "#DFE8F2", "#D5E1EE", "#CAD9EA"))
lineas <- cajas |>
  rowwise() |>
  reframe(n = n, xmin = xmin, k = seq_len(n), y = ymax - 7.6 - 3.7 * (seq_len(n) - 1), texto = campos[seq_len(n)])
flechas <- tibble(
  x = c(17, 17, 34, 97), xend = c(17, 17, 63, 126),
  y = c(cajas$ymin[1], cajas$ymin[2], 11.5, 11.5), yend = c(48, 23, 11.5, 11.5)
)
etiquetas <- tibble(
  x = c(20, 20, 48.5, 111.5), y = c((cajas$ymin[1] + 48) / 2, (cajas$ymin[2] + 23) / 2, 15, 15),
  hjust = c(0, 0, 0.5, 0.5), texto = c("Preprocesamiento", "Separación", "Normalización", "Desambiguación")
)
dicc <- tibble(xmin = 63, xmax = 97, ymin = 44, ymax = 58)
borde <- "#8EA6BF"

p1 <- ggplot() +
  geom_rect(data = cajas, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(fill)), colour = borde, linewidth = 0.3) +
  geom_text(data = cajas, aes(x = xmin + 2.5, y = ymax - 3.4, label = paste0("Campos (", n, ")")),
            hjust = 0, family = fuente, fontface = "bold", size = pt(9), colour = gris_texto) +
  geom_text(data = lineas, aes(x = xmin + 2.5, y = y, label = texto), hjust = 0, family = fuente, size = pt(8), colour = gris_texto) +
  geom_rect(data = dicc, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#FBF3E2", colour = "#D2AE6E", linewidth = 0.3) +
  annotate("text", x = 80, y = 51, label = "Diccionarios\nINE + Idescat", family = fuente, size = pt(9), lineheight = 0.95, colour = gris_texto) +
  geom_segment(data = flechas, aes(x = x, xend = xend, y = y, yend = yend), colour = "grey45", linewidth = 0.4,
               arrow = arrow(length = unit(1.6, "mm"), type = "closed")) +
  annotate("segment", x = 80, xend = 80, y = 27, yend = 44, colour = "#C49A52", linewidth = 0.4,
           arrow = arrow(length = unit(1.6, "mm"), type = "closed", ends = "both")) +
  annotate("text", x = 82, y = 35.5, label = "Jaro-Winkler", hjust = 0, family = fuente, fontface = "italic", size = pt(8), colour = "#9A7535") +
  geom_text(data = etiquetas, aes(x = x, y = y, label = texto, hjust = hjust), family = fuente, fontface = "italic", size = pt(9), colour = gris_texto) +
  coord_fixed(xlim = c(0, 160), ylim = c(min(cajas$ymin), 70), expand = FALSE, clip = "off") +
  theme_void()
guardar_fig(p1, "fig01_flujo_trabajo", ancho = 16.4, alto = 7.6)

# ---- Figura 2: entradas según la frecuencia de su apellido --------------------
f2 <- reg |>
  count(apellido_manual, name = "apariciones") |>
  count(apariciones, name = "apellidos") |>
  mutate(entradas = apariciones * apellidos)
guardar_tabla(f2, "fig02_frecuencia_apellidos_datos")
p2 <- ggplot(f2, aes(apariciones, entradas)) +
  geom_col(fill = azul, width = 0.7) +
  scale_x_continuous(breaks = c(1, seq(10, 90, 10)), expand = expansion(add = 1)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Apariciones del apellido", y = "Entradas") +
  tema_articulo +
  theme(panel.grid.major.x = element_blank(), axis.line.x = element_line(colour = "grey60", linewidth = 0.3))
guardar_fig(p2, "fig02_frecuencia_apellidos", alto = 7)

# ---- Figura 3: concentración de los 20 apellidos más frecuentes ---------------
p3 <- ggplot(t6, aes(N_manual, Individuos)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, fullrange = TRUE, colour = "grey70", linewidth = 0.4, linetype = "dashed") +
  geom_point(colour = azul, size = 1.8) +
  geom_text_repel(aes(label = Apellido), colour = gris_texto, size = pt(8), family = fuente, seed = 1,
                  min.segment.length = 0.4, segment.colour = "grey70", segment.size = 0.25, box.padding = 0.3) +
  scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 10)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Apariciones", y = "Individuos") +
  tema_articulo
guardar_fig(p3, "fig03_concentracion_apellidos", alto = 8.5)

# ---- Figuras 4-7: líneas de tiempo (salida-llegada) por individuo -------------
meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
etiqueta_mes <- function(x) {
  m <- as.integer(format(x, "%m"))
  if_else(m == 1, paste0(meses[m], "\n", format(x, "%Y")), meses[m])
}

linea_tiempo <- function(datos, orden_ids, nombre) {
  d <- datos |>
    filter(id_manual %in% orden_ids) |>
    mutate(
      fila = match(id_manual, rev(orden_ids)),
      inicio = coalesce(fecha_salida, fecha_llegada),
      fin = pmax(fecha_llegada, inicio + 3),   # ancho mínimo visible: 3 días
      buque = factor(buque, levels = unique(buque[order(fecha_llegada)]))
    )
  guardar_tabla(d |> select(id_manual, nombre_fuente, buque, tipo_buque, puerto_salida, fecha_salida, fecha_llegada) |> arrange(id_manual, fecha_llegada),
                paste0(nombre, "_datos"))
  inicio_eje <- if (min(d$inicio) < as.Date("1852-01-01")) as.Date("1851-12-01") else as.Date("1852-01-01")
  n <- length(orden_ids)
  p <- ggplot(d) +
    geom_hline(yintercept = seq_len(n), colour = "grey93", linewidth = 0.3) +
    geom_vline(xintercept = as.Date("1853-01-01"), colour = "grey75", linewidth = 0.3, linetype = "dashed") +
    geom_rect(aes(xmin = inicio, xmax = fin, ymin = fila - 0.16, ymax = fila + 0.16, fill = buque), colour = NA) +
    scale_x_date(limits = c(inicio_eje, as.Date("1854-01-01")), breaks = seq(as.Date("1852-01-01"), as.Date("1853-10-01"), by = "3 months"),
                 labels = etiqueta_mes, expand = c(0, 0)) +
    scale_y_continuous(breaks = seq_len(n), labels = rev(orden_ids), limits = c(0.5, n + 0.5), expand = c(0, 0)) +
    scale_fill_manual(values = paleta_buques) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(colour = NA))) +
    labs(x = NULL, y = NULL, fill = NULL) +
    tema_articulo +
    theme(
      panel.grid = element_blank(),
      axis.ticks.x = element_line(colour = "grey70", linewidth = 0.3),
      axis.ticks.length.x = unit(1.2, "mm"),
      axis.line.x = element_line(colour = "grey70", linewidth = 0.3),
      axis.text.y = element_text(size = 8.5, colour = gris_texto),
      legend.position = "top", legend.justification = "left", legend.location = "plot",
      legend.key.size = unit(2.6, "mm"), legend.key.spacing.x = unit(3, "mm"),
      legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(1.5, "mm")
    )
  guardar_fig(p, nombre, alto = 2.3 + 0.62 * n)
}

linea_tiempo(reg |> filter(apellido_manual == "Medinas"), c("MEDINAS0001", "MEDINAS0002"), "fig04_linea_tiempo_medinas")
linea_tiempo(reg |> filter(apellido_manual == "Sorolla"), c("Sorolla0002", "Sorolla0003", "Sorolla0001"), "fig05_linea_tiempo_sorolla")
linea_tiempo(reg |> filter(apellido_manual == "Maristany"),
             sprintf("MARISTANY%04d", c(18, 1, 20, 2, 19, 21, 22)), "fig06_linea_tiempo_g_maristany")
linea_tiempo(reg |> filter(apellido_manual == "Maristany"),
             sprintf("MARISTANY%04d", c(12, 4, 10)), "fig07_linea_tiempo_a_maristany")
