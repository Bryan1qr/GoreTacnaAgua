library(tidyverse)

source("scripts/automatización/dataset_agua.R")

dm <- dataset_agua2(ruta = "data", tabla_agua = "insumos/tabla_coordenadas_para_modelo.csv")

dm <- dm %>% openxlsx::write.xlsx("dataset_agua_2025_v2.xlsx")

dfm1 <- dm %>% pivot_longer(names_to = "parametros", values_to = "valor", cols = 11:length(dm))
install.packages("googlesheets4")


library(googlesheets4)
library(googledrive)

gs4_auth()

gs4_create("dataset_agua_largo", sheets = list("Hoja1" = dm))

# Actualizar el google sheet: ---------------------------------------------
sheet_id <- drive_find(pattern = "dataset_agua_largo") %>% pull(id)

range_write(sheet_id, range = "Hoja1", data = dm, reformat = TRUE)
