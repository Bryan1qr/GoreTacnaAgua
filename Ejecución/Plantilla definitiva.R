# Test para wqi -----------------------------------------------------------
## Generación de datos:

source("scripts/automatización/dataset_agua.R")
source("scripts/automatización/espacial_agua.R")
source("scripts/automatización/icarhs_tabla.R")


# Carga de fuente de datos
tabla1 <- dataset_agua2(ruta = "data", coordenadas = "spatial_input")

# Generación del ICARHS:
icarhss <- icarhs_tab(tabla_base = tabla1$tabla_larga,
                      tabla_icarhs = "tabla_icarhs.xlsx",
                      fecha_inicio = "2020-01-01", fecha_fin = "2024-12-31")


# Guardado en Google drive ------------------------------------------------

library(googlesheets4)
library(googledrive)

#### Creación de archivo por primera vez:

#gs4_create("nombre de tabla", sheets = list("Hoja1" = tabla a guardar))
# gs4_create("dataset_icarhs", sheets = list("Hoja1" = tabla_icarhs))

#### Actualización de hojas de datos de google sheets:

# Actualizar el google sheet: ---------------------------------------------
# De la tabla ancha:
sheet_id <- drive_find(pattern = "dataset_agua_ancho_v1") %>% pull(id)
sheet_id <- "1lyesoSUlYOveVP77g0ZOyj_ttRyZcj70O3Hqw_q5Bzk"
db_mod <- tabla1$tabla_ancha %>% 
  mutate(fecha_larga = format(fecha_larga, "%Y-%m-%d %H:%M"))

range_write(sheet_id, range = "Hoja1", 
            data = db_mod, reformat = TRUE)

# De la tabla espacial:
sheet_id2 <- drive_find(pattern = "dataset_espacial_v3") %>% pull(id)

range_write(sheet_id2, range = "Hoja1", data = tabla1$data_espacial, reformat = TRUE)

# De la tabla icarhs:
sheet_id3 <- drive_find(pattern = "dataset_icarhs") %>% pull(id)

range_write(sheet_id3, range = "Hoja1", data = icarhss, reformat = TRUE)