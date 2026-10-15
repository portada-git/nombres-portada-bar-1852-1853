# Capitanes y patrones en las entradas de buques en Barcelona (1852-1853): datos y código

Datos y código para reproducir los ejercicios computacionales del artículo:

> Ibarz, J. y Nieto, A. «¿Quién está al mando? Datos de capitanes y patrones en las noticias de entradas de buques en la Barcelona de la segunda mitad del siglo XIX. Notas metodológicas». *Historia Regional* (en prensa).

El trabajo forma parte del proyecto [PortADa. Port Arrivals Data](https://cordis.europa.eu/project/id/101129889) (Marie Skłodowska-Curie Actions, 101129889).

El flujo parte de 6.902 noticias de entradas de buques del *Diario de Barcelona* (1852-1853) y compara dos vías para normalizar y desambiguar los nombres de capitanes y patrones: una revisión manual y una vía computacional (extracción y separación de nombres con un modelo de lenguaje, normalización con Jaro-Winkler contra diccionarios del INE y del Idescat, y agrupamiento por similitud ponderada).

## Cómo reproducir

Requisitos: R ≥ 4.5 con `dplyr`, `tidyr`, `stringr`, `stringi`, `readr`, `readxl`, `jsonlite`, `stringdist`, `ggplot2`, `ggrepel`, `scales`, `ragg`, `systemfonts` y `here`.

```sh
Rscript run_all.R
```

En unos 15 segundos se generan en `outputs/` las tablas y figuras del artículo y el archivo `outputs/cifras_articulo.csv`, con cada cifra citada en el texto y la sección en la que aparece.

Para recalcular el *fuzzy matching* desde los diccionarios en lugar de usar los candidatos guardados (unos 2 minutos más; requiere además `RecordLinkage`):

```sh
RECALCULAR_FUZZY=1 Rscript run_all.R
```

Con `RecordLinkage` 0.4-12.6 el recálculo reproduce exactamente los candidatos de 2025 y todas las tablas y cifras salen idénticas.

## Contenido

```
data/
  raw/
    entradas_1852_1853_revision_manual.xlsx      6.902 entradas con la revisión manual
    entradas_1852_1853_nombre_llm.rds            las mismas entradas con el nombre extraído por el LLM
    llm_separacion_nombre_apellido_log.jsonl     separación nombre/apellido de los 3.976 nombres únicos (gpt-4o-mini)
  intermedio/                                    candidatos Jaro-Winkler calculados en 2025
  diccionarios/                                  apellidos y nombres de referencia (INE, Idescat)
R/
  00_setup.R                       paquetes, rutas y funciones comunes
  01_construccion_registros.R      base de registros: vía manual y vía automática
  02_tablas_descriptivas.R         tablas 1-6 y 10
  03_evaluacion_apellidos.R        concordancia manual/automática del apellido
  04_desambiguacion_maristany.R    agrupamiento de Maristany y métricas (tablas 7-9)
  05_figuras.R                     figuras 1-7
  clustering_similitud.R           algoritmo de agrupamiento (similitud ponderada + componentes conexas)
  opcional_recalculo_fuzzy.R       recálculo Jaro-Winkler desde los diccionarios
  llm/                             consultas a gpt-4o-mini, con los prompts completos
run_all.R
```

En `outputs/tablas/`, los archivos `tablaNN_*` corresponden a las tablas del artículo, `figNN_*_datos` a los datos de cada figura y `anexo_*` a material de apoyo (CSV con separador «;» y coma decimal).

## Qué se reproduce y qué no

- **Todo lo que se calcula sobre los datos** (normalización, evaluación, agrupamiento, tablas, figuras y cifras) se regenera de forma determinista.
- **Las consultas al modelo de lenguaje no se repiten**: sus salidas están guardadas en `data/raw/`. Los scripts de `R/llm/` permiten repetirlas con una clave de OpenAI (`OPENAI_API_KEY`), aunque el modelo puede dar resultados levemente distintos.

## Fuentes de los datos

| Archivo | Origen |
|---|---|
| `data/raw/` | Noticias de entradas de buques del *Diario de Barcelona* (1852-1853), extraídas en la prueba piloto previa al proyecto PortADa, con la revisión manual de los autores. |
| `idescat_apellidos_cataluna_2024.csv` | Institut d'Estadística de Catalunya, [apellidos de la población](https://www.idescat.cat/cognoms/), 2024 (frecuencia ≥ 4). |
| `ine_padron2015_*.csv` | Instituto Nacional de Estadística, apellidos y nombres más frecuentes (padrón a 1/1/2015), vía [jvalhondo/spanish-names-surnames](https://github.com/jvalhondo/spanish-names-surnames). |
| `ine_marcboquet_*.csv` | Instituto Nacional de Estadística, edición anterior de los mismos listados, vía [marcboquet/spanish-names](https://github.com/marcboquet/spanish-names). |

## Decisiones de procesamiento

- **Duraciones en horas.** En 294 noticias la duración del viaje está en horas («De Palma en 18 horas», que el OCR suele leer «boras»). En 17 la extracción la registró como días. `01_construccion_registros.R` las corrige solo cuando los días registrados coinciden con las horas del texto y conserva la fecha original en `fecha_salida_original` (listado en `anexo_correccion_duraciones_en_horas.csv`).
- **Orden de los registros en el agrupamiento.** Las etiquetas `MATCH_xx` dependen del orden de los registros; `04_desambiguacion_maristany.R` reproduce el orden original para que coincidan con las tablas 8 y 9 del artículo.

## Licencia

El código se publica bajo licencia MIT. Los diccionarios conservan las condiciones de uso de sus fuentes (INE e Idescat), que permiten la reutilización citando el origen.
