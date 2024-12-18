library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)
library(leaflet)

files <- list.files("tasas")

files <- files[grepl(pattern = "suav",files)]

load(paste0("tasas/",files[5]))



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
# depto_popup <- paste0("<strong>Departamento: </strong>", 
#                       tasas_enf_df$departamen, 
#                       "<br><strong>RME Suavizadas: </strong>", 
#                       round(tasas_enf_df$suavizadas,2))
# 
# #Armo los mapas
# 
# colores_personalizados <- colorFactor(
#   palette = c("red", "blue", "lightpink", "skyblue2", "white"),
#   domain = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo")
# )
# 
# 
# 
# mapa_enf <-leaflet(tasas_enf_df) %>%
#   addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
#            tileOptions(tms = TRUE,maxZoom = 20),attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>') %>%
#   addPolygons(fill = colores_personalizados(tasas_enf_df$mean),
#               fillColor = colores_personalizados(tasas_enf_df$mean),
#               fillOpacity = 0.7,
#               stroke = TRUE,
#               weight = 1,
#               color = "#BDBDC3",
#               popup = paste0(tasas_enf_df$departamen,"\n",tasas_enf_df$mean),
#               group= "Cluster",
#               highlight = highlightOptions(color = "white",
#                                            weight = 2,
#                                            bringToFront = TRUE)) %>%
#   addLegend(position = "bottomright", # Posición de la leyenda
#             pal= colores_personalizados,
#             values= ~tasas_enf_df$mean,
#             title = "Cluster",
#             group = "Cluster")
# 
# n <- length(levels(tasas_enf_df$suavizadas_agrup))
# 
# pal <- colorFactor("BuPu",domain = tasas_enf_df$suavizadas_agrup)
# 
# mapa_enf <- mapa_enf %>%
#   addPolygons(fill= tasas_enf_df$suavizadas_agrup,
#               color= ~pal(tasas_enf_df$suavizadas_agrup),
#               fillOpacity = 0.7,
#               stroke = FALSE,
#               weight = 1,
#               popup = depto_popup,
#               group= "Suavizadas",
#               highlight = highlightOptions(color = "white",
#                                            weight = 2,
#                                            bringToFront = TRUE)) %>%
#   addLegend(position = "bottomright", # Posición de la leyenda
#             pal = pal, 
#             values = tasas_enf_df$suavizadas_agrup,
#             labels = labeler(tasas_enf_df$suavizadas_agrup),
#             title = "RME suavizadas",
#             group = "Suavizadas")
# 
# mapa_enf <- mapa_enf %>%
#   addPolygons(fill = colores_personalizados(tasas_enf_df$cluster),
#               fillColor = colores_personalizados(tasas_enf_df$cluster),
#               fillOpacity = 0.7,
#               stroke = TRUE,
#               weight = 1,
#               color = "grey",
#               popup = paste0(tasas_enf_df$departamen,"\n",tasas_enf_df$cluster),
#               group= "Cluster Test") %>%
#   addLegend(position = "bottomright", # Posición de la leyenda
#             pal= colores_personalizados, 
#             values = ~tasas_enf_df$cluster, 
#             title = "Cluster Test",
#             group = "Cluster Test")
# 
# 
# mapa_enf <- mapa_enf %>%
#   addLayersControl(
#     overlayGroups = c("Suavizadas","Cluster","Cluster Test"),
#     position = "bottomleft",
#     options = layersControlOptions(collapsed = F))


#Guardo el mapa

#save(tasas_enf_df,file= paste0("Reporte/mapa_",gsub("\\.RData$","",files[1]),".RData"))



n <- length(levels(tasas_enf_df$suavizadas_agrup))

pal <- RColorBrewer::brewer.pal(n,"BuPu")
  
tasas <- ggplot(tasas_enf_df)+
  geom_sf(aes(fill= suavizadas_agrup), colour= "#E6E6E6")+
  scale_fill_manual(values = pal,
                    labels= labeler(tasas_enf_df$suavizadas_agrup))+
  coord_sf()+
  labs(fill= "RME suavizadas") +
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


palette = c("red", "blue", "lightpink", "skyblue2", "white")
domain = c("High-High", "Low-Low", "High-Low", "Low-High", "no significativo")
  
cluster <- ggplot(tasas_enf_df)+
  geom_sf(aes(fill= mean), colour= "#E6E6E6")+
  scale_fill_manual(values = palette,
                    breaks = domain)+
  labs(fill= "Cluster") +
  coord_sf()+
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

cluster_test <- ggplot(tasas_enf_df)+
  geom_sf(aes(fill= cluster), colour= "#E6E6E6")+
  scale_fill_manual(values = palette,
                    breaks = domain)+
  labs(fill= "Cluster Test") +
  coord_sf()+
  theme(legend.title = element_text(size= 10,face = "bold"),
        legend.text = element_text(size= 8,face = "bold"),
        legend.background = element_rect(colour= "black",fill= "white"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

library(gridExtra)
library(grid)

tot_ntd <- grid.arrange(tasas,cluster,cluster_test,
             ncol = 3, widths = c(2, 2, 2),
             top= "Total Enfermedades Desatendidas (2004-2018)")

library(R.utils)

saveObject(tot_ntd,paste0("Reporte/mapa_tot_ntd.Rbin"))

