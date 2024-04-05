library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)



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

#Armo mapa de matriz de vecindad#

coord <- st_coordinates(deptos)

#Genero la matriz de contiguidad#

neighbors <- poly2nb(deptos)

plot(st_geometry(deptos), border = "lightgrey")
plot.nb(neighbors, st_geometry(deptos), add = TRUE)


#Guardar matriz de vecindad#

write.nb.gal(neighbors,"matriz de vecindad/matriz")
