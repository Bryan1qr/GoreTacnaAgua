source("scripts/creaciones/filtro.R")



# CAtegoría 4E2
c1 <- read.delim("clipboard")
c2 <- c1 %>% filtro()

c3 <- a5 %>% full_join(c2, by = "PARAMETROS", keep = F, relationship = "many-to-many") %>% 
  select(-UNIDADES) %>% relocate(UNIDAD, .after = 1)
# Categoría 3D1

d1 <- read.delim("clipboard")
d2 <- d1 %>% filtro()

d3 <- a5 %>% full_join(d2, by = "PARAMETROS", keep = F, relationship = "many-to-many") %>% 
  select(-UNIDADES) %>% relocate(UNIDAD, .after = 1)

# Categoría 1A2

e1 <- read.delim("clipboard")
e2 <- e1 %>% filtro()

e3 <- a5 %>% full_join(e2, by = "PARAMETROS", keep = F, relationship = "many-to-many") %>% 
  select(-UNIDADES) %>% relocate(UNIDAD, .after = 1)


# cuencas

caplina <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                      RCapl1 = e3$RCapl1, RCapl3 = e3$RCapl3)


locumba <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                      RCall1 = c3$RCall1, RCall3 = c3$RCall3, 
                      RSala1 = c3$RSala1, RCuri2 = d3$RCuri2, 
                      RIlab2 = d3$RIlab2, RSala3 = d3$RSala3, 
                      RLocu3 = d3$RLocu3, RLocu5 = d3$RLocu5)

intercuenca <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                          QVila1 = e3$QVila1,QVila3 = e3$QVila3, RUchu1 = e3$RUchu1)

sama <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                   QKovi1 = c3$QKovi1, RTica2 = c3$RTica2, RPist1 = d3$RPist1,
                   RTala1 = d3$RTala1, RSala2 = d3$RSala2, RSama2 = d3$RSama2,
                   RSama3 = d3$RSama3, RSama5 = d3$RSama5)

ushusuma <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                       QCari1 = c3$QCari1, RUshu2 = c3$RUshu2)

mauri <- data.frame(PARAMETROS = c3$PARAMETROS, UNIDAD = c3$UNIDAD,
                    RMaur3 = c3$RMaur3, RMaur4 = c3$RMaur4)


write.xlsx(caplina, "temporales/caplina_tab1.xlsx")
write.xlsx(locumba, "temporales/locumba_tab1.xlsx")
write.xlsx(intercuenca, "temporales/intercuenca_tab1.xlsx")
write.xlsx(sama, "temporales/sama_tab1.xlsx")
write.xlsx(ushusuma, "temporales/ushusuma_tab1.xlsx")
write.xlsx(mauri, "temporales/mauri_tab1.xlsx")