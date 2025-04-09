library(tidyverse)
library(openxlsx)

source("scripts/automatización/dataset_agua.R")
source("scripts/automatización/espacial_agua.R")


df1 <- dataset_agua2(
  ruta = "data", 
  coordenadas = "spatial_input")


# Oficio 