# generación de tabla para icarhs -----------------------------------------

icarhs_tab <- function(tabla_base, tabla_icarhs, fecha_inicio, fecha_fin){
  tabla_icarhs1 <- read.xlsx(tabla_icarhs)
  lotico <- tabla_base %>% filter(fecha_larga >= fecha_inicio & tipo == "lótico" & fecha_larga <= fecha_fin)
  estructura <- lotico %>% select(-c(valor, PARAMETROS, fecha_larga)) %>% distinct()
  tab1 <- left_join(x = lotico, y = tabla_icarhs1, by = c("PARAMETROS", "categoria")) %>%
    filter(!is.na(TIPO)) %>% 
    rename(c("Variable" = "PARAMETROS", "Value" = "valor")) %>% 
    mutate(Date = as.Date(fecha_larga), DetectionLimit = NA) %>% 
    mutate(DetectionLimit = as.numeric(DetectionLimit))
  
  algoritmo1 <- function(tabla){
    tabla %>% 
    group_by(codigo) %>%
      group_modify(~ {
        df_temp <- .x
        wqi_value <- tryCatch({
          calc_wqi(df_temp)
        }, error = function(e) {
          NA
        })
        tibble(WQI = wqi_value)
      }) %>%
      ungroup()
  }
  
  mo <- tab1 %>%  filter(TIPO == "MO") %>% algoritmo1()
  fqm <- tab1 %>% filter(TIPO == "FQM") %>% algoritmo1()
  xd <- list(mo, fqm)
  tabb1 <- data.frame(xd[1]) %>% mutate(WQI1 = round(WQI$WQI)) %>% select(codigo, WQI1)
  tabb2 <- data.frame(xd[2]) %>% mutate(WQI2 = round(WQI$WQI)) %>% select(codigo, WQI2)
  combi <- full_join(tabb1, tabb2) %>% mutate(WQI_index = pmin(WQI1, WQI2, na.rm = F)) %>% filter(!is.na(WQI_index))
  left_join(combi, estructura, by = "codigo") %>% 
    mutate(calidad = case_when(
      WQI_index >= 95 & WQI_index <=100 ~ "Excelente",
      WQI_index >= 80 & WQI_index < 95 ~ "Bueno",
      WQI_index >= 65 & WQI_index < 80 ~ "Regular",
      WQI_index >= 45 & WQI_index < 65 ~ "Malo",
      WQI_index >= 0 & WQI_index < 45 ~ "Pésimo",
      TRUE ~ NA
    )) %>% 
    st_as_sf(coords = c("este", "norte"), crs = 32719, remove = F) %>%
    st_transform(crs = 4326) %>%            
    mutate(
      lon = st_coordinates(.)[, 1],  # Extrae la longitud (X)
      lat = st_coordinates(.)[, 2]   # Extrae la latitud (Y)
    ) %>% st_drop_geometry() %>% 
    as.data.frame() %>% mutate(coordenadas = paste0(lat, ", ", lon)) %>% 
    select(-lon, -lat)
}

