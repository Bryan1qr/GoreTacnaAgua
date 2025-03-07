# uniformización ----------------------------------------------------------
# Este es un scrip que preparó una tabla a partir de los datos uniformizados
# de los puntos de monitoreo de tacna
library(tidyverse)
library(sf)
read.csv("insumos/datos_de_uniformización.csv") %>%
  mutate(este = as.numeric(gsub(" ", "", este)),
         norte = as.numeric(gsub(" ", "", norte)),
         altitud = as.numeric(gsub(" ","", altitud))) %>% 
  mutate(nombre_rh = paste(tipo, nombre_rh)) %>% 
  st_as_sf(coords = c("este", "norte"), crs = 32719) %>% 
  st_transform(crs = 4326) %>% 
  mutate(
    ycoord = st_coordinates(.)[, 2],
    xcoord = st_coordinates(.)[, 1],
    coordenadas = paste(ycoord, xcoord, sep = ","),
    categoria = case_when(
      categoria == "Cat. 1 A2" ~ "C1A2",
      categoria == "Cat. 4" ~ "C4E2",
      categoria == "Cat. 3" ~ "C3D1",
      TRUE ~ NA
    )) %>% 
  as_tibble() %>% 
  select(-c(tipo, geometry, ycoord, xcoord)) %>% 
  write.csv("insumos/tabla_coordenadas_para_modelo.csv", row.names = F)

read.csv("insumos/tabla_coordenadas_para_modelo.csv") %>% as_tibble() %>% head()
