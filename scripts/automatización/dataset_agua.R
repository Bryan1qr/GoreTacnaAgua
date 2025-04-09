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
      mutate(UNIDAD = zoo::na.locf(UNIDAD, na.rm = FALSE))
    
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
    mutate(valor = if_else(grepl("^Ausencia", valor), 0, parse_number(valor)),
           codigo = case_when(
             cuenca == "SAMA" & codigo == "RChac2" ~ "RTara1",
             cuenca == "SAMA" & codigo == "RIrab" ~ "RTica2",
             cuenca == "USHUSUMA" & codigo == "RpPauc" ~ "QCari2",
             cuenca == "LOCUMBA" & codigo == "RLocu2" ~ "RSala2",
             TRUE ~ codigo)) %>% 
    mutate(PARAMETROS = paste0(PARAMETROS, " (", UNIDAD, ")")) %>% 
    select(-UNIDAD, -año, -periodo) %>% distinct()
  
  espacial <- spatial1(coordenadas)
  merge(df_combinado, espacial,  by = c("codigo", "cuenca"), all.x = TRUE)
}

# codigo = case_when(
#   cuenca == "LOCUMBA" & codigo == "LSuch4S" ~ "LSuch4S",
#   cuenca == "LOCUMBA" & codigo == "QCara1" ~ "QCara1",
#   cuenca == "LOCUMBA" & codigo == "QAchi1" ~ "QAchi1",
#   cuenca == "LOCUMBA" & codigo == "RSala1" ~ "RJaru1",
#   cuenca == "LOCUMBA" & codigo == "RCall1" ~ "RCall1",
#   cuenca == "LOCUMBA" & codigo == "RCall2" ~ "RCall2",
#   cuenca == "LOCUMBA" & codigo == "RCall3" ~ "RCall3",
#   cuenca == "LOCUMBA" & codigo == "RCuri1" ~ "RCuri1",
#   cuenca == "LOCUMBA" & codigo == "RCuri2" ~ "RCuri2",
#   cuenca == "LOCUMBA" & codigo == "RIlab1" ~ "RIlab1",
#   cuenca == "LOCUMBA" & codigo == "RIlab2" ~ "RIlab2",
#   cuenca == "LOCUMBA" & codigo == "RSala2" ~ "RSala1",
#   cuenca == "LOCUMBA" & codigo == "RSala3" ~ "RSala2",
#   cuenca == "LOCUMBA" & codigo == "RLocu2" ~ "RSala2",
#   cuenca == "LOCUMBA" & codigo == "RLocu3" ~ "RLocu3",
#   cuenca == "LOCUMBA" & codigo == "RLocu4" ~ "RLocu4",
#   cuenca == "LOCUMBA" & codigo == "RLocu5" ~ "RLocu5",
#   cuenca == "LOCUMBA" & codigo == "QHond1" ~ "QHond1",
#   cuenca == "LOCUMBA" & codigo == "RColo1" ~ "RCalu1",
#   cuenca == "LOCUMBA" & codigo == "RCint1" ~ "RCint1",
#   cuenca == "LOCUMBA" & codigo == "QBoro1" ~ "QBoro1",
#   cuenca == "LOCUMBA" & codigo == "RCali1" ~ "RCali1",
#   cuenca == "LOCUMBA" & codigo == "RCall0" ~ "RCall0",
#   cuenca == "LOCUMBA" & codigo == "RLocu6" ~ "RLocu6",
#   cuenca == "LOCUMBA" & codigo == "LSuch1S" ~ "LSuch1S",
#   cuenca == "LOCUMBA" & codigo == "LSuch1F" ~ "LSuch1F",
#   cuenca == "LOCUMBA" & codigo == "LSuch2S" ~ "LSuch2S",
#   cuenca == "LOCUMBA" & codigo == "LSuch2F" ~ "LSuch2F",
#   cuenca == "LOCUMBA" & codigo == "LSuch3S" ~ "LSuch3S",
#   cuenca == "LOCUMBA" & codigo == "LSuch3F" ~ "LSuch3F",
#   cuenca == "LOCUMBA" & codigo == "RHuay1" ~ "RHuay1",
#   cuenca == "LOCUMBA" & codigo == "LAric1S" ~ "LAric1S",
#   cuenca == "LOCUMBA" & codigo == "LAric1F" ~ "LAric1F",
#   cuenca == "LOCUMBA" & codigo == "LAric2S" ~ "LAric2S",
#   cuenca == "LOCUMBA" & codigo == "LAric2F" ~ "LAric2F",
#   cuenca == "LOCUMBA" & codigo == "LAric3S" ~ "LAric3S",
#   cuenca == "LOCUMBA" & codigo == "LAric3F" ~ "LAric3F",
#   cuenca == "LOCUMBA" & codigo == "LAric4S" ~ "LAric4S",
#   cuenca == "LOCUMBA" & codigo == "LAric4F" ~ "LAric4F",
#   cuenca == "SAMA" & codigo == "QCach1" ~ "QCach1",
#   cuenca == "SAMA" & codigo == "RPist1" ~ "RPist1",
#   cuenca == "SAMA" & codigo == "REsti1" ~ "REsti1",
#   cuenca == "SAMA" & codigo == "RArum1" ~ "RArum1",
#   cuenca == "SAMA" & codigo == "RTala1" ~ "RTala1",
#   cuenca == "SAMA" & codigo == "RSala1" ~ "RSala1",
#   cuenca == "SAMA" & codigo == "RSala2" ~ "RSala2",
#   cuenca == "SAMA" & codigo == "RSama1" ~ "RSama1",
#   cuenca == "SAMA" & codigo == "RSama2" ~ "RSama2",
#   cuenca == "SAMA" & codigo == "RSama3" ~ "RSama3",
#   cuenca == "SAMA" & codigo == "RSama4" ~ "RSama4",
#   cuenca == "SAMA" & codigo == "RSama5" ~ "RSama5",
#   cuenca == "SAMA" & codigo == "RSama6" ~ "RSama6",
#   cuenca == "SAMA" & codigo == "QKovi1" ~ "QKovi1",
#   cuenca == "SAMA" & codigo == "RIrab" ~ "RTica2",
#   cuenca == "SAMA" & codigo == "RTica2" ~ "RTica2",
#   cuenca == "SAMA" & codigo == "QCapa1" ~ "QCapa1",
#   cuenca == "SAMA" & codigo == "RChac2" ~ "RTara1",
#   cuenca == "SAMA" & codigo == "RTara1" ~ "RTara1",
#   cuenca == "SAMA" & codigo == "RSala3" ~ "RSala3",
#   cuenca == "SAMA" & codigo == "RTica1" ~ "RTica1",
#   cuenca == "SAMA" & codigo == "EJaru1S" ~ "EJaru1S",
#   cuenca == "SAMA" & codigo == "EJaru1F" ~ "EJaru1F",
#   cuenca == "SAMA" & codigo == "EJaru2S" ~ "EJaru2S",
#   cuenca == "SAMA" & codigo == "EJaru2F" ~ "EJaru2F",
#   cuenca == "SAMA" & codigo == "RJaru1" ~ "RJaru1",
#   cuenca == "SAMA" & codigo == "RMama1" ~ "RMama1",
#   cuenca == "USHUSUMA" & codigo == "RUshu1" ~ "RUshu1",
#   cuenca == "USHUSUMA" & codigo == "RUshu2" ~ "RUshu2",
#   cuenca == "USHUSUMA" & codigo == "RpPauc" ~ "QCari1",
#   cuenca == "MAURI" & codigo == "RQuil1" ~ "RQuiv1",
#   TRUE ~ codigo)
# 