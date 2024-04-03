#Activa el package

library(dplyr)
library(tidyr)
library(sknifedatar)
library(lubridate)
library(DBI)
library(stringr)
library(openxlsx)
library(sf)
library(ggplot2)

#usethis::edit_r_environ(scope= c("project"))

#Me conecto al sql
con <- DBI::dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = "10.0.55.50",
                      database = "bi-test",
                      uid = Sys.getenv("uid"),
                      pwd = Sys.getenv("pwd"),
                      encoding= "windows-1252")

#armo la query para traer los datos de provincias y paises

data <- dbGetQuery(con,"select * from SCI_DASHBOARD_EJECUTIVO_RANKING")

dbDisconnect(con)

#Guardo el archivo para tenerlo disponible

save(data, file= "ranking.RData")
load("ranking.RData")



#Actualizo los UT de Naranja considerando Naranja SDK
  
  load("UT_Naranja_All.RData")
  
data <- data %>%
    left_join(user_transaccionales %>%
                mutate(proyecto= tolower(proyecto))%>%
                select(proyecto, provincia,UT),
              by= c("Project_Name"= "proyecto", "Provincia"= "provincia"))%>%
                mutate(UT= coalesce(UT.y,UT.x))%>%
      select(-UT.x,-UT.y)

#Selecciono las regiones argentina, chile y mexico y vendemas de andina


data <- data %>%
  filter(!Project_Name %in% c('Uala-co','medianet','datafast')) %>%
  filter(Region_Normalizada %in% c('Mexico','Andina','Chile',"AR, UY, PY","CARCAM")) %>%
  group_by(Region_Normalizada,Provincia)%>%
  summarise(across(c(QTx,Amount_USD,UT),sum)) %>%
  mutate(Pais = case_when(Region_Normalizada == 'Andina' ~ 'Peru',
                          Region_Normalizada == "AR, UY, PY" ~ 'Argentina',
                          Region_Normalizada == "CARCAM" ~ 'Republica Dominicana',
  TRUE ~ Region_Normalizada))

View(data)

#Cargo el archivo con los códigos geograficos

codigos <- read.xlsx("codigos geograficos.xlsx")

View(codigos)
#Agrego el codigo geografico

View(data)

data <- data %>%
  mutate(Provincia= case_when(Provincia== 'Ciudad Autónoma de Buenos Aire' ~ 'Ciudad Autónoma de Buenos Aires',
                              Provincia== 'Aysén del G. Carlos Ibáñez del' ~ 'Aysén',
                              Provincia== "Libertador General Bernardo O'" ~ "Libertador General B. O'Higgins",
                              Provincia== 'Magallanes y de la Antártica C' ~ "Región de Magallanes y Antártica Chilena",
                              Provincia== 'Veracruz de Ignacio de la Llav' ~ "Veracruz de Ignacio de la Llave",
                              Provincia== 'Veracruz de Ignacio de la Llav' ~ "Veracruz de Ignacio de la Llave",
                              TRUE ~ Provincia)) %>%
  left_join(codigos, by= c("Pais","Provincia")) %>%
  as.data.frame()

View(data)
#Cargo las poblaciones

poblaciones <- read.csv2("poblacion.csv")

#Agrego las poblaciones y separo en 3 el archivo
View(data)

data <- data %>%
  left_join(poblaciones, by= c("Pais","Codigo"))%>%
  mutate(Tasa= round(UT*100000/Poblacion,2)) %>%
  group_split(Pais)

View(data[[5]])
#Armo la rampa de colores de GeoPagos

GeoPaleta <- colorRampPalette(c("#FF8EDD","#9D00FF"))(5)



#Agrego los objetos geograficos a Argentina

#Cargo los datos

argentina <- st_read(dsn= "C:/Users/hernan.hernandez/Documents/Documents/Cartografia/pais_prov",
                     layer = "pxpciadatosok")


argentina <- st_make_valid(argentina)


argentina  <- st_transform(argentina,crs = 4326)

#Selecciono sin Antartida

argentina  <- st_crop(argentina ,xmin= -74,ymin= -55, xmax= -53, ymax= -21)



argentina <- data[[1]] %>%
    mutate(Codigo= case_when(Codigo < 10 ~ paste0(0,Codigo),
                             TRUE ~ as.character(Codigo))) %>%
    inner_join(.,argentina %>% select(link,geometry), by= c("Codigo"="link"))%>%
  st_as_sf()


#Creo esta función para editar las etiquetas

labeler <- function(labels){
  #Guardo el nivel
  levels <- levels(labels)
  
  #Edito el nivel
  
  levels <- stringr::str_remove(string = levels, fixed("("))
  levels <- stringr::str_remove(string = levels, fixed("]"))
  levels <- stringr::str_remove(string = levels, fixed("["))
  levels <- stringr::str_replace(string = levels,pattern = ",",replacement = "-")

  
  #Edito las etiquetas
  labels <- stringr::str_remove(string = labels, fixed("("))
  labels <- stringr::str_remove(string = labels, fixed("]"))
  labels <- stringr::str_remove(string = labels, fixed("["))
  labels <- stringr::str_replace(string = labels,pattern = ",",replacement = "-")
 
  return(levels(factor(labels,levels = levels)))
}




#Calculo los quintiles

#Armo los quantiles


argentina$UT_q <- cut(argentina$UT,quantile(argentina$UT, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)

argentina$Proporcion_q  <- cut(argentina$Tasa,quantile(argentina$Tasa, probs= c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 4)


#Guardo los mapas de Argentina en una lista

map_argentina <- list()

#Distribución UT Absoluta

map_argentina[[1]] <- ggplot(argentina)+
  geom_sf(aes(fill= UT_q), colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                 labels= labeler(argentina$UT_q))+
  labs(fill= "Usuarios Transaccionales") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Distribución UT Proporcional

map_argentina[[2]] <- ggplot(argentina)+
  geom_sf(aes(fill= Proporcion_q),colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(argentina$Proporcion_q))+
  labs(fill= paste0("Proporcion cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


############################################################
#                                                          #
#          Agrego los objetos geográficos a Chile          #
#                                                          #
############################################################


#Cargo los datos

chile <- st_read(dsn= "C:/Users/hernan.hernandez/Documents/Documents/Cartografia/Chile/Regiones",layer = "Regional")


chile <- st_make_valid(chile)


chile <- st_transform(chile,crs = 4326)



chile <- data[[2]] %>%
  inner_join(.,chile %>% select(codregion,geometry), by= c("Codigo"="codregion"))%>%
  st_as_sf()


#Calculo los quintiles

#Armo los quantiles

chile$UT_q <- cut(chile$UT,quantile(chile$UT, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)

chile$Proporcion_q  <- cut(chile$Tasa,quantile(chile$Tasa, probs= c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 4)


#Guardo los mapas de Argentina en una lista

map_chile <- list()

#Distribución UT Absoluta

map_chile[[1]] <- ggplot(chile)+
  geom_sf(aes(fill= UT_q), colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(chile$UT_q))+
  labs(fill= "Usuarios Transaccionales") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Distribución UT Proporcional

map_chile[[2]] <- ggplot(chile)+
  geom_sf(aes(fill= Proporcion_q),colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(chile$Proporcion_q))+
  labs(fill= paste0("Proporcion cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

############################################################
#                                                          #
#          Agrego los datos geográficos a Mexico           #
#                                                          #
############################################################


#Cargo los datos

mexico <- st_read(dsn= "C:/Users/hernan.hernandez/Documents/Documents/Cartografia/México",
                  layer = "gdb_ref_esta_ine_2009",
                  options = "ENCODING=UTF-8")


mexico <- st_make_valid(mexico)

mexico <- st_transform(mexico,crs = 4326)




mexico <- data[[3]] %>%
  mutate(Codigo= case_when(Codigo < 10 ~ paste0(0,Codigo),
                           TRUE ~ as.character(Codigo)))%>%
  inner_join(.,mexico %>% select(cve_ent,geometry), by= c("Codigo"="cve_ent"))%>%
  st_as_sf()


#Calculo los quintiles

#Armo los quantiles

mexico$UT_q <- cut(mexico$UT,quantile(mexico$UT, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)

mexico$Proporcion_q  <- cut(mexico$Tasa,quantile(mexico$Tasa, probs= c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 4)


#Guardo los mapas de Argentina en una lista

map_mexico <- list()

#Distribución UT Absoluta

map_mexico[[1]] <- ggplot(mexico)+
  geom_sf(aes(fill= UT_q), colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(mexico$UT_q))+
  labs(fill= "Usuarios Transaccionales") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Distribución UT Proporcional

map_mexico[[2]] <- ggplot(mexico)+
  geom_sf(aes(fill= Proporcion_q),colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(mexico$Proporcion_q))+
  labs(fill= paste0("Proporcion cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

############################################################
#                                                          #
#          Agrego los datos geográficos a Peru             #
#                                                          #
############################################################


#Cargo los datos

peru <- st_read(dsn= "C:/Users/hernan.hernandez/Documents/Documents/Cartografia/Peru",
                  layer = "DEPARTAMENTOS",
                  options = "ENCODING=UTF-8")


peru <- st_make_valid(peru)

peru <- st_transform(peru,crs = 4326)




peru <- data[[4]] %>%
  mutate(Codigo= case_when(Codigo < 10 ~ paste0(0,Codigo),
                           TRUE ~ as.character(Codigo)))%>%
  inner_join(.,peru %>% select(IDDPTO,geometry), by= c("Codigo"="IDDPTO"))%>%
  st_as_sf()


#Calculo los quintiles

#Armo los quantiles

peru$UT_q <- cut(peru$UT,quantile(peru$UT, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)

peru$Proporcion_q  <- cut(peru$Tasa,quantile(peru$Tasa, probs= c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)


#Guardo los mapas de Argentina en una lista

map_peru <- list()

#Distribución UT Absoluta

map_peru[[1]] <- ggplot(peru)+
  geom_sf(aes(fill= UT_q), colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(peru$UT_q))+
  labs(fill= "Usuarios Transaccionales") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Distribución UT Proporcional

map_peru[[2]] <- ggplot(peru)+
  geom_sf(aes(fill= Proporcion_q),colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(peru$Proporcion_q))+
  labs(fill= paste0("Proporcion cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


############################################################
#                                                          #
#   Agrego los datos geográficos a Rep Dominicana          #
#                                                          #
############################################################


#Cargo los datos

rd <- st_read(dsn= "C:/Users/hernan.hernandez/Documents/Documents/Cartografia/Dominicana",
              layer = "PROVCenso2010")

rd <- st_make_valid(rd)

rd <- st_transform(rd,crs = 4326)




rd <- data[[5]] %>%
  mutate(Codigo= case_when(Codigo < 10 ~ paste0(0,Codigo),
                           TRUE ~ as.character(Codigo)))%>%
  inner_join(.,rd %>% select(PROV,geometry), by= c("Codigo"="PROV"))%>%
  st_as_sf()

#Calculo los quintiles

#Armo los quantiles

rd$UT_q <- cut(rd$UT,quantile(rd$UT, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)

rd$Proporcion_q  <- cut(rd$Tasa,quantile(rd$Tasa, probs= c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)


#Guardo los mapas de Argentina en una lista

map_rd <- list()

#Distribución UT Absoluta

map_rd[[1]] <- ggplot(rd)+
  geom_sf(aes(fill= UT_q), colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(rd$UT_q))+
  labs(fill= "Usuarios Transaccionales") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Distribución UT Proporcional

map_rd[[2]] <- ggplot(rd)+
  geom_sf(aes(fill= Proporcion_q),colour= "#E6E6E6")+
  scale_fill_manual(values = GeoPaleta,
                    labels= labeler(rd$Proporcion_q))+
  labs(fill= paste0("Proporcion cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Exporto los mapas a un ppt para editarlos

library(export)

graph2ppt(map_argentina[[1]],file="mapas.pptx",width=12, height=10)
graph2ppt(map_argentina[[2]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_chile[[1]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_chile[[2]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_mexico[[1]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_mexico[[2]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_peru[[1]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_peru[[2]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_rd[[1]],file="mapas.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_rd[[2]],file="mapas.pptx",width=12, height=10,append= TRUE)



#Guardo la data de los paises

ranking <- do.call(rbind,data)
View(ranking)

save(ranking, file= "Input Reporte/ranking.RData")

#Guardo un dataset con los valores

names(mexico)

df.final <- argentina %>% st_drop_geometry() %>%
  select(Pais,Provincia,UT,Tasa,Proporcion_q) %>%
  bind_rows(chile %>% st_drop_geometry() %>%
              select(Pais,Provincia,UT,Tasa,Proporcion_q))%>%
  bind_rows(mexico %>% st_drop_geometry() %>%
              select(Pais,Provincia,UT,Tasa,Proporcion_q)) %>%
  bind_rows(chile %>% st_drop_geometry() %>%
              select(Pais,Provincia,UT,Tasa,Proporcion_q)) %>%
  bind_rows(peru %>% st_drop_geometry() %>%
              select(Pais,Provincia,UT,Tasa,Proporcion_q)) %>%
  bind_rows(rd %>% st_drop_geometry() %>%
              select(Pais,Provincia,UT,Tasa,Proporcion_q))

openxlsx::write.xlsx(df.final,"datosUTPaisesDic2022.xlsx")
############################################################
#                                                          #
#             Armo la información de contexto              #
#                                                          #
############################################################

#Cargo los datos
library(openxlsx)

inc_finan <- read.xlsx("Input Reporte/Inclusion financiera/Data Inclusion Financiera.xlsx")

#Separo el archivo en 3

inc_finan <- inc_finan %>%
group_split(Pais)

#Uno la cartografia
#Argentina

if_argentina <- inc_finan[[1]] %>%
  mutate(Codigos= case_when(Codigos < 10 ~ paste0(0,Codigos),
                           TRUE ~ as.character(Codigos)))%>%
  inner_join(.,argentina %>% select(link,geometry), by= c("Codigos"="link"))%>%
  st_as_sf()


#Chile

if_chile <- inc_finan[[2]] %>%
inner_join(.,chile %>% select(codregion,geometry), by= c("Codigos"="codregion"))%>%
  st_as_sf()


#Mexico

if_mexico <-inc_finan[[3]] %>%
  mutate(Codigos= case_when(Codigos < 10 ~ paste0(0,Codigos),
                           TRUE ~ as.character(Codigos)))%>%
  inner_join(.,mexico %>% select(cve_ent,geometry), by= c("Codigos"="cve_ent"))%>%
  st_as_sf()

#Armo los mapas

map_if <- list()

#Armo los quintiles

if_argentina$Prop_q  <- cut(if_argentina$Prop,quantile(if_argentina$Prop, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)
if_chile$Prop_q <- cut(if_chile$Prop,quantile(if_chile$Prop, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)
if_mexico$Prop_q <- cut(if_mexico$Prop,quantile(if_mexico$Prop, probs = c(0,.2,.4,.6,.8,1),na.rm = T,names = F),include.lowest = TRUE, dig.lab = 5)



#Armo la rampa de colores de GeoPagos

GeoPaleta <- colorRampPalette(c("#FF8EDD","#9D00FF"))(5)
GeoPaleta2 <- colorRampPalette(c("#56D26E","#B7AEFF","#FF8EDD"))(5)
GeoPaleta3 <- colorRampPalette(c("#B7AEFF","#615c89"))(5)



#Inclusion financiera

map_if[[1]] <- ggplot(if_argentina)+
  geom_sf(aes(fill= Prop_q), colour= "white", size= 0.01)+
  scale_fill_manual(values = GeoPaleta3,
                labels= labeler(if_argentina$Prop_q))+
  labs(fill= paste0("PDA cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


map_if[[2]] <- ggplot(if_chile)+
  geom_sf(aes(fill= Prop_q), colour= "white", size= 0.01)+
  scale_fill_manual(values = GeoPaleta3,
                    labels= labeler(if_chile$Prop_q))+
  labs(fill= paste0("PDA cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


map_if[[3]] <- ggplot(if_mexico)+
  geom_sf(aes(fill= Prop_q), colour= "white")+
  scale_fill_manual(values = GeoPaleta3,
                    labels= labeler(if_mexico$Prop_q))+
  labs(fill= paste0("PDA cada","\n","100.000 hab.")) +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#Exporto los mapas a un ppt para editarlos


library(export)

graph2ppt(map_if[[1]],file="contexto.pptx",width=12, height=10)
graph2ppt(map_if[[2]],file="contexto.pptx",width=12, height=10,append= TRUE)
graph2ppt(map_if[[3]],file="contexto.pptx",width=12, height=10,append= TRUE)


#Subo la data de contexto


data %>%
  filter(!Project_Name %in% c('Uala-co','medianet','datafast')) %>%
  filter(Region_Normalizada %in% c('Mexico','Andina','Chile',"AR, UY, PY","CARCAM")) %>%
  group_by(Region_Normalizada,Mes,Provincia)%>%
  summarise(across(c(QTx,Amount_USD,UT),sum)) %>%
  mutate(Pais = case_when(Region_Normalizada == 'Andina' ~ 'Peru',
                          Region_Normalizada == "AR, UY, PY" ~ 'Argentina',
                          Region_Normalizada == "CARCAM" ~ 'Republica Dominicana',
                          TRUE ~ Region_Normalizada)) %>%
  group_by(Pais,Mes)%>%
  mutate(prop= UT/sum(UT))%>%
  pivot_wider(id_cols = c(Pais,Provincia), names_from = Mes,values_from = prop)

View(data)

