library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)
library(leaflet)

files <- list.files("tasas")

files <- files[grepl(pattern = "suav",files)]

load(paste0("tasas/",files[1]))



##%######################################################%##
#                                                          #
####                Estimo la I de Moran                ####
#                                                          #
##%######################################################%##

#Cargo la matriz de vecindad

matriz <- read.gal("matriz de vecindad/matriz")

tasas_enf_df[is.na(tasas_enf_df)] <- 0

sexo = unique(tasas_enf_df$sexo)

for(sexo in sexo){

  tasas_enf_df[tasas_enf_df$sexo== sexo,c(10:14)] <- cbind(attr(localmoran(tasas_enf_df$suavizadas[tasas_enf_df$sexo== sexo[1]],nb2listw(matriz,style = "B")),"quadr"),
                                                  localmoran(tasas_enf_df$suavizadas[tasas_enf_df$sexo== sexo[1]],nb2listw(matriz,style = "B"))[,c(1,5)]
                                                  )

    
    
  print(sexo)
}

#Armo los cluster

tasas_enf_df <- tasas_enf_df %>%
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


tasas_enf_df <- deptos %>%
  left_join(tasas_enf_df %>% filter(grepl("3.",sexo)) %>% select(link,RME,suavizadas,mean,cluster))

#Calculo los quintiles 

tasas_enf_df$suavizadas_agrup <- cut(tasas_enf_df$suavizadas,
                               c(round(unique(quantile(tasas_enf_df$suavizadas, probs = c(0, 0.2, 0.4, 0.6, 0.8),names = FALSE)), 2),Inf),
                               dig.lab = 6, include.lowest = TRUE,right = TRUE)

#Guardo el archivo

save(tasas_enf_df,file= paste0("Reporte/mapa_",gsub("\\.RData$","",files[1]),".RData"))

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

#Armo el popup
depto_popup <- paste0("<strong>Departamento: </strong>", 
                      deptos$departamen, 
                      "<br><strong>RME Suavizadas: </strong>", 
                      round(deptos$suavizadas,2))

#Armo los mapas

colores_personalizados <- colorFactor(
  palette = c("red", "blue", "lightpink", "skyblue2", "white"),
  domain = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo")
)



mapa_enf <-leaflet(deptos) %>%
  addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
           tileOptions(tms = TRUE,maxZoom = 14),attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>') %>%
  addPolygons(fill = colores_personalizados(deptos$mean),
              fillColor = colores_personalizados(deptos$mean),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 1,
              color = "#BDBDC3",
              popup = paste0(deptos$departamen,"\n",deptos$mean),
              group= "Cluster",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE)) %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            colors = c("red", "blue", "lightpink", "skyblue2", "white"), 
            labels = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo"), 
            title = "Cluster",
            group = "Cluster")

n <- length(levels(deptos$suavizadas_agrup))

pal <- colorFactor("BuPu",domain = deptos$suavizadas_agrup)

mapa_enf <- mapa_enf %>%
  addPolygons(fill= deptos$suavizadas_agrup,
              color= ~pal(deptos$suavizadas_agrup),
              fillOpacity = 0.7,
              stroke = FALSE,
              weight = 1,
              popup = depto_popup,
              group= "Suavizadas",
              highlight = highlightOptions(color = "white",
                                           weight = 2,
                                           bringToFront = TRUE)) %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            pal = pal, 
            values = deptos$suavizadas_agrup,
            labels = labeler(deptos$suavizadas_agrup),
            title = "RME suavizadas",
            group = "Suavizadas")

mapa_enf <- mapa_enf %>%
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


mapa_enf %>%
  addLayersControl(
    overlayGroups = c("Suavizadas","Cluster","Cluster Test"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = F)
)

map