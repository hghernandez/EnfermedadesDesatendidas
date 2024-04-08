library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)
library(leaflet)

load(file="tasas/tasas_chagas_suav.RData")

##%######################################################%##
#                                                          #
####                Estimo la I de Moran                ####
#                                                          #
##%######################################################%##

#Cargo la matriz de vecindad

matriz <- read.gal("matriz de vecindad/matriz")

tasas_chagas_df[is.na(tasas_chagas_df)] <- 0

sexo = unique(tasas_chagas_df$sexo)

for(sexo in sexo){

  tasas_chagas_df[tasas_chagas_df$sexo== sexo,c(10:14)] <- cbind(attr(localmoran(tasas_chagas_df$suavizadas[tasas_chagas_df$sexo== sexo[1]],nb2listw(matriz,style = "B")),"quadr"),
                                                  localmoran(tasas_chagas_df$suavizadas[tasas_chagas_df$sexo== sexo[1]],nb2listw(matriz,style = "B"))[,c(1,5)]
                                                  )

    
    
  print(sexo)
}

#Armo los cluster

tasas_chagas_df <- tasas_chagas_df %>%
  mutate(cluster= case_when(`Pr(z != E(Ii))` > 0.05 ~ 'no significativo',
                            TRUE ~ mean))

#Cargo la cartografia de los departamentos

deptos <- st_read(dsn= "cartografia/deptos",
                  layer = "pxdptodatosok")

#Arreglo las geometrias
deptos <- st_make_valid(deptos)

#Cargo la cartografia de las provincias
pcias <- st_read(dsn= "cartografia/provincias",
                 layer = "pxpciadatosok") 
#Arreglo las geometrias
pcias <- st_make_valid(pcias)

#Selecciono caba como provincia
caba <- pcias %>% filter(link == '02') 


#Excluyo antartida y caba
deptos <- deptos %>% filter(!link %in% c(94028,94021) & codpcia != '02')

#Uno los departamentos y caba
deptos <- deptos %>%
  bind_rows(caba)

#Arreglo los datos

deptos <- deptos %>%
  mutate(link= case_when(link== '02' ~ '02000', TRUE ~ link),
         codpcia= case_when(link== '02000' ~ '02',TRUE ~ codpcia),
         departamen= case_when(link== '02000' ~ 'CABA',TRUE ~ departamen))

#Agrego los datos de ambos sexos


deptos <- deptos %>%
  left_join(tasas_chagas_df %>% filter(grepl("3.",sexo)) %>% select(link,RME,suavizadas,mean,cluster))

#Calculo los quintiles 

deptos$suavizadas_agrup <- cut(deptos$suavizadas,quantile(deptos$suavizadas,probs= c(0,0.2,0.4,0.6,0.8,1)),
                               dig.lab = 4)

#Formateo los puntos de corte

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


#Armo los mapas

colores_personalizados <- colorFactor(
  palette = c("red", "blue", "lightpink", "skyblue2", "white"),
  domain = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo")
)



mapa_chagas <-leaflet(deptos) %>%
  addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
           tileOptions(tms = TRUE,maxZoom = 14),attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>') %>%
  addPolygons(fill = colores_personalizados(deptos$mean),
              fillColor = colores_personalizados(deptos$mean),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 1,
              color = "grey",
              popup = paste0(deptos$departamen,"\n",deptos$mean),
              group= "Cluster") %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            colors = c("red", "blue", "lightpink", "skyblue2", "white"), 
            labels = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo"), 
            title = "Cluster",
            group = "Cluster")


pal <- RColorBrewer::brewer.pal(5,"Blues")

mapa_chagas <- mapa_chagas %>%
  addPolygons(fill= deptos$suavizadas_agrup,
              color= pal,
              fillOpacity = 0.7,
              stroke = FALSE,
              weight = 1,
              popup = paste0(deptos$departamen,"\n",deptos$suavizadas),
              group= "Suavizadas") %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            colors = pal, 
            labels = labeler(deptos$suavizadas_agrup), 
            title = "RME suavizadas",
            group = "Suavizadas")

mapa_chagas <- mapa_chagas %>%
  addPolygons(fill = colores_personalizados(deptos$cluster),
              fillColor = colores_personalizados(deptos$cluster),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 1,
              color = "grey",
              popup = paste0(deptos$departamen,"\n",deptos$cluster),
              group= "Cluster Test") %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            colors = c("red", "blue", "lightpink", "skyblue2", "white"), 
            labels = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo"), 
            title = "Cluster Test",
            group = "Cluster Test")


mapa_chagas %>%
  addLayersControl(
    overlayGroups = c("Suavizadas","Cluster","Cluster Test"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = F)
)

map