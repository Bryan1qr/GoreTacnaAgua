# Función para integrar el componente espacial ----------------------------

spatial1 <- function(x){
  archivos <- list.files(x, pattern = "\\.xlsx$", full.names = TRUE)
  
  extraccion <- function(y){
    read.xlsx(y, startRow = 11, colNames = FALSE) %>% 
      mutate(
        cuenca = case_when(
          str_detect(X3, regex("^Cuenca", ignore_case = TRUE)) ~ X3 %>%
            str_split(" ") %>%
            map_chr(~ str_c(.[-1], collapse = "-")),
          
          str_detect(X3, regex("Intercuenca", ignore_case = TRUE)) ~ X3 %>%
            str_split(" ") %>%
            map_chr(~ str_c(., collapse = "")),
          
          TRUE ~ NA_character_)) %>%
      mutate(cuenca = str_to_upper(cuenca),
             categoria = map_chr(str_extract_all(X16, "\\b\\w"),
                                 ~ paste0(.x, collapse = ""))) %>% 
      select(X7, X8, cuenca, X12, X13, X14, categoria) %>% 
      rename_with(~ c("codigo", "descripcion", "cuenca", "zona", "este", "norte", "categoria"),
                  everything())
  }
  
  lista <- map(archivos, extraccion)
  bind_rows(lista)
}
