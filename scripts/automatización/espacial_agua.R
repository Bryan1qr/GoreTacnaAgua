# Función para integrar el componente espacial ----------------------------

spatial1 <- function(x){
  archivos <- list.files(x, pattern = "\\.xlsx$", full.names = TRUE)
  
  extraccion <- function(y){
    read.xlsx(y, startRow = 11, colNames = FALSE) %>% 
      mutate(
        cuenca = str_extract(y, "(?<=/)[^/]+(?=\\.xlsx)"),
        categoria = map_chr(str_extract_all(X16, "\\b\\w"),
                                 ~ paste0(.x, collapse = "")),
             categoria = case_when(
               categoria == "C1A" ~ "C1A2",
               categoria == "C3" ~ "C3D1",
               categoria == "C4" ~ "C4E2")) %>% 
      select(X7, X8, cuenca, X12, X13, X14, categoria) %>% 
      rename_with(~ c("codigo", "descripcion", "cuenca", "zona", "este", "norte", "categoria"),
                  everything())
  }
  
  lista <- map(archivos, extraccion)
  bind_rows(lista)
}
