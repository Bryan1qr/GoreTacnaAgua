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
  merge(df_combinado, espacial,  by = c("codigo", "cuenca"), all.x = TRUE) %>% 
    mutate(tipo = case_when(
      grepl("^E", codigo) ~ "léntico",
      grepl("^L", codigo) ~ "léntico",
      TRUE ~ "lótico"),
      cuerpo_agua = str_extract(descripcion, "^\\S+\\s+\\S+"),
      cuerpo_agua = str_replace_all(cuerpo_agua, ",", ""),
      cuerpo_agua = str_replace(cuerpo_agua, "^Rio\\b", "Río"),
      cuerpo_agua = str_squish(cuerpo_agua))
}

