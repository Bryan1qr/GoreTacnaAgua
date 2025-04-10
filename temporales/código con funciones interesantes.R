a <- read.delim("clipboard")
library(tidyverse)

a2 <- a %>% distinct() %>% arrange(PARÁMETROS.DE.CAMPO) %>% 
  mutate(across(everything(), ~ if_else(str_detect(.x, "^[A-Z]+$"), NA_character_, .x))) %>% 
  filter(!is.na(PARÁMETROS.DE.CAMPO))


a3 <- data.frame(PARÁMETROS.DE.CAMPO = unique(df1$PARAMETROS))


# Creación del nuevo dataset de agua --------------------------------------
dataset_agua2 <- function(ruta, coordenadas){
  # Creamos una funciónn para la selección de los valores excel deseados:
  agua <- function(origen_df){
    a <- readxl::read_xls(origen_df, skip = 19) %>%
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>% 
      select(where(~ any(!is.na(.)))) %>% select(-starts_with("Cat")) %>% mutate(periodo = origen_df) %>% 
      pivot_longer(cols = -c(PARAMETROS, UNIDAD, periodo), names_to = "codigo", values_to = "valor") %>% 
      mutate(cuenca = str_extract(periodo, "(?<=/)(.*?)(?=_)"),
             año = str_extract(periodo, "(?<=_)(.*?)(?=_)"),
             periodo = str_extract(periodo, "(?<=_)([^_\\.]+)(?=\\.)")) %>%
      filter(valor != "----") %>%
      select(codigo, cuenca, año, periodo, PARAMETROS, UNIDAD, valor) %>% 
      mutate(UNIDAD = zoo::na.locf(UNIDAD, na.rm = FALSE), 
             UNIDAD = if_else(UNIDAD == "mg/L P", "mg/L", UNIDAD))
    a
  }
  archivos_xls <- list.files(path = ruta, pattern = "\\.xls$", full.names = TRUE)
  listita <- map(archivos_xls, agua)
  bind_rows(listita)
  }


library(tidyverse)
library(openxlsx)
library(sf)
source("scripts/automatización/dataset_agua.R")
source("scripts/automatización/espacial_agua.R")
a4 <- dataset_agua2(ruta = "data", coordenadas = "spatial_input")


a5 <- a4 %>% select(PARAMETROS, UNIDAD) %>% distinct()
