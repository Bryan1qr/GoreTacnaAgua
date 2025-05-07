# Creación del nuevo dataset de agua --------------------------------------
dataset_agua2 <- function(ruta, coordenadas){
  
  paquetes <- c("tidyverse", "openxlsx", "readxl", "zoo", "wqindex", "sf")
  for (pkg in paquetes) {
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("El paquete", pkg, "es necesario pero no está instalado."))}}
  
  suppressMessages(suppressWarnings({
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
      
      b <- readxl::read_xls(origen_df, skip = 15, col_names = TRUE) %>% 
        filter(`Fecha monitoreo` %in% c("Hora Monitoreo", "PARAMETROS")) %>% 
        select(where(~ any(!is.na(.)))) %>% 
        select(-starts_with(c( "DD", "..."))) %>% 
        pivot_longer(cols = -`Fecha monitoreo`, names_to = "fecha", values_to = "hora") %>% 
        mutate(
          hora = ifelse(is.na(hora), "12:00", hora),
          hora = ifelse(
            grepl("^\\d*\\.\\d+$", hora),
            sapply(hora, function(x) {
              decimal_time <- as.numeric(x)
              hours <- floor(decimal_time * 24)
              minutes <- round((decimal_time * 24 - hours) * 60)
              sprintf("%02d:%02d:%02d", hours, minutes, 0)
            }),
            hora
          )
        ) %>% fill(fecha, hora, .direction = "downup") %>% 
        pivot_wider(id_cols = fecha, names_from = `Fecha monitoreo`, values_from = hora) %>% 
        mutate(fecha = str_replace(fecha, "\\.\\.\\.\\d+$", ""),
               fecha_larga = as.POSIXct(paste(fecha, `Hora Monitoreo`), format = "%d/%m/%Y %H:%M")) %>%
        select(fecha_larga, PARAMETROS) %>% rename("codigo" = "PARAMETROS")
      
      a %>% left_join(b, by = "codigo")}
    
    archivos_xls <- list.files(path = ruta, pattern = "\\.xls$", full.names = TRUE)
    listita <- map(archivos_xls, agua)
    
    df_combinado <- bind_rows(listita)%>% 
      mutate(valor = if_else(grepl("^Ausencia", valor), 0, parse_number(valor,locale = locale(decimal_mark = ","))),
             codigo = case_when(
               cuenca == "SAMA" & codigo == "RChac2" ~ "RTara1",
               cuenca == "SAMA" & codigo == "RIrab" ~ "RTica2",
               cuenca == "USHUSUMA" & codigo == "RpPauc" ~ "QCari2",
               cuenca == "LOCUMBA" & codigo == "RLocu2" ~ "RSala2",
               TRUE ~ codigo)) %>% 
      mutate(PARAMETROS = paste0(PARAMETROS, " (", UNIDAD, ")")) %>% 
      select(-UNIDAD, -año, -periodo) %>% distinct()
    
    espacial <- spatial1(coordenadas)
    c <- merge(df_combinado, espacial,  by = c("codigo", "cuenca"), all.x = TRUE) %>% 
      mutate(tipo = case_when(
        grepl("^E", codigo) ~ "léntico",
        grepl("^L", codigo) ~ "léntico",
        TRUE ~ "lótico"),
        cuerpo_agua = str_extract(descripcion, "^\\S+\\s+\\S+"),
        cuerpo_agua = str_replace_all(cuerpo_agua, ",", ""),
        cuerpo_agua = str_replace(cuerpo_agua, "^Rio\\b", "Río"),
        cuerpo_agua = str_squish(cuerpo_agua))
    
    d <- c %>% 
      filter((PARAMETROS == "Nitratos (NO3-) (mg/L)" | PARAMETROS == "Nitratos-N (mg/L)") & categoria == "C4E2") %>% 
      pivot_wider(names_from = "PARAMETROS", values_from = "valor") %>% 
      mutate(`Nitratos (NO3-) (mg/L)` = if_else(is.na(`Nitratos (NO3-) (mg/L)`), `Nitratos-N (mg/L)` * 4.43, NA)) %>%
      select(-`Nitratos-N (mg/L)`) %>% na.omit() %>% 
      pivot_longer(cols = 11, names_to = "PARAMETROS", values_to = "valor")
    
    tabla1 <- bind_rows(c, d)  %>% arrange(fecha_larga)
    actividad <- tabla1 %>% 
      mutate(estado = if_else(fecha_larga >= "2023-01-01", "activo", "inactivo")) %>%
      filter(estado == "activo") %>% select(cuenca, codigo) %>% distinct()
    
    tabla_larga <- tabla1 %>% 
      left_join(actividad %>% 
                  select(codigo, cuenca) %>%
                  mutate(estado = "activo"),
                by = c("codigo", "cuenca")) %>%
      mutate(estado = replace_na(estado, "inactivo")) %>% 
      st_as_sf(coords = c("este", "norte"), crs = 32719, remove = FALSE) %>%
      st_transform(crs = 4326) %>%            
      mutate(
        lon = st_coordinates(.)[, 1],  # Extrae la longitud (X)
        lat = st_coordinates(.)[, 2]   # Extrae la latitud (Y)
      ) %>% st_drop_geometry() %>% 
      as.data.frame() %>% mutate(coordenadas = paste0(lat, ", ", lon)) %>% 
      select(-lon, -lat)
    
    tabla_ancha <- tabla_larga %>% pivot_wider(
      names_from = PARAMETROS, values_from = valor,
      id_cols = c(cuenca, codigo, fecha_larga, 
                  tipo, cuerpo_agua, estado, categoria, zona,
                  descripcion,este, norte, coordenadas))
    
    data_espacial <- tabla_larga %>% 
      select(codigo, cuenca, descripcion,
             zona, este, norte, coordenadas,
             categoria, tipo, cuerpo_agua, estado) %>% 
      distinct()
    
    lst(tabla_larga, tabla_ancha, data_espacial) 
  }))
}

