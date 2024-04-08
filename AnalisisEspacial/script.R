library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)

load(file="tasas/tasas_ntd_suav.RData")

##%######################################################%##
#                                                          #
####                Estimo la I de Moran                ####
#                                                          #
##%######################################################%##

#Cargo la matriz de vecindad

matriz <- read.gal("matriz de vecindad/matriz")

tasas_ntd_df[is.na(tasas_ntd_df)] <- 0

sexo = unique(tasas_ntd_df$sexo)

for(sexo in sexo){

  tasas_ntd_df[tasas_ntd_df$sexo== sexo,c(10:14)] <- cbind(attr(localmoran(tasas_ntd_df$suavizadas[tasas_ntd_df$sexo== sexo[1]],nb2listw(matriz,style = "B")),"quadr"),
                                                  localmoran(tasas_ntd_df$suavizadas[tasas_ntd_df$sexo== sexo[1]],nb2listw(matriz,style = "B"))[,c(1,5)]
                                                  )

    
    
  print(sexo)
}

#Armo los cluster

tasas_ntd_df <- tasas_ntd_df %>%
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
  left_join(tasas_ntd_df %>% filter(grepl("3.",sexo)) %>% select(link,RME,suavizadas,cluster))

#Armo los mapas

colores_personalizados <- colorFactor(
  palette = c("red", "blue", "lightpink", "skyblue2", "white"),
  domain = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo")
)



leaflet(deptos) %>%
  addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
           tileOptions(tms = TRUE,maxZoom = 14),attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>') %>%
  addPolygons(fill = colores_personalizados(deptos$cluster),
              fillColor = colores_personalizados(deptos$cluster),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 2,
              popup = paste0(deptos$departamen,"\n",deptos$cluster)) %>%
  addLegend(position = "bottomright", # Posición de la leyenda
            colors = c("red", "blue", "lightpink", "skyblue2", "white"), 
            labels = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo"), 
            title = "Cluster")


