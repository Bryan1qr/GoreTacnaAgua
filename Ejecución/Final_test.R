library(tidyverse)
library(openxlsx)
library(sf)
source("scripts/automatización/dataset_agua.R")
source("scripts/automatización/espacial_agua.R")


df1 <- dataset_agua2(
  ruta = "data", 
  coordenadas = "spatial_input")

actividad <- df1 %>% mutate(estado = if_else(fecha_larga >= "2023-01-01", "activo", "inactivo")) %>%
  filter(estado == "activo") %>% select(cuenca, codigo) %>% distinct()

df3 <- df1 %>% left_join(actividad %>% select(codigo, cuenca) %>% mutate(estado = "activo"),
                 by = c("codigo", "cuenca")) %>%
  mutate(estado = replace_na(estado, "inactivo"),
         categoria = case_when(
           categoria == "C4" ~ "C4E2",
           categoria == "C3" ~ "C3D1",
           TRUE ~ categoria
         ))


########### ESPACIAL #################################
df2 <- df3 %>% select(codigo, cuenca, descripcion, zona, este, norte, categoria, tipo, cuerpo_agua) %>% 
  distinct() %>% 
  st_as_sf(coords = c("este", "norte"), crs = 32719) %>%
  st_transform(crs = 4326) %>%            
  mutate(
    lon = st_coordinates(.)[, 1],  # Extrae la longitud (X)
    lat = st_coordinates(.)[, 2]   # Extrae la latitud (Y)
  ) %>% st_drop_geometry() %>% 
  as.data.frame() %>% mutate(coordenadas = paste0(lat, ", ", lon)) %>% 
  select(-lon, -lat)

df4 <- df2 %>% 
  left_join(actividad %>% select(codigo, cuenca) %>% mutate(estado = "activo"),
            by = c("codigo", "cuenca")) %>%
  mutate(estado = replace_na(estado, "inactivo"))

library(googlesheets4)
library(googledrive)


gs4_create("dataset_agua_v2", sheets = list("Hoja1" = df3))
gs4_create("dataset_espacial_v3", sheets = list("Hoja1" = df4))

df2 <- distinct(df1)


xd1 <- df3 %>% select(-c(descripcion, zona, este, norte, categoria)) %>% 
  mutate(id = paste(codigo, cuenca,fecha_larga, sep = "_")) %>% filter(id == "QCari1_USHUSUMA_2024-10-30 10:30:00")

duplicados <- df3 %>%
  select(codigo, cuenca, fecha_larga, PARAMETROS) %>%  # Selecciona las columnas relevantes
  mutate(id = paste(codigo, cuenca, fecha_larga, sep = "_")) %>%  # Crea el id
  group_by(id, PARAMETROS) %>%  # Agrupa por id y parámetros
  count() %>%  # Cuenta cuántos registros existen para cada grupo
  filter(n > 1)

# Actualizar el google sheet: ---------------------------------------------
sheet_id <- drive_find(pattern = "dataset_agua_v2") %>% pull(id)

range_write(sheet_id, range = "Hoja1", data = df3, reformat = TRUE)



# Actualizar el google sheet2: ---------------------------------------------
sheet_id2 <- drive_find(pattern = "dataset_espacial_v3") %>% pull(id)

range_write(sheet_id2, range = "Hoja1", data = df4, reformat = TRUE)

# Tablas largas -----------------------------------------------------------

dataset_largo_v2 <- df3 %>% mutate(id = paste(cuenca, codigo, fecha_larga, sep = "-")) %>% 
  pivot_wider(names_from = PARAMETROS, values_from = valor, 
              id_cols = c(cuenca, codigo, fecha_larga, tipo, cuerpo_agua, estado, categoria, zona, este, norte, descripcion)) %>% 
  arrange(cuenca, codigo, fecha_larga)

gs4_create("dataset_agua_largo_v1", sheets = list("Hoja1" = dataset_largo_v2))

# Actualizar el google sheet: ---------------------------------------------
sheet_id3 <- drive_find(pattern = "dataset_agua_largo_v1") %>% pull(id)

range_write(sheet_id3, range = "Hoja1", data = dataset_largo_v2, reformat = TRUE)


dfx <- df3 %>% mutate(id = paste(cuenca, codigo, fecha_larga, sep = "-"))

dfx %>% dplyr::summarise(n = dplyr::n(), .by = c(id, PARAMETROS)) |>
  dplyr::filter(n > 1L) %>% View()


openxlsx::write.xlsx(dataset_largo_v2, "tabla_larga_agua_tacna.xlsx")
