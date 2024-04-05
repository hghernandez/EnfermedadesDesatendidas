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


for(sexo in sexo){
  
  tasas_ntd_df[tasas_ntd_df$sexo== sexo,c(15)] <- tasas_ntd_df$Ii[tasas_ntd_df$sexo== sexo]-mean(tasas_ntd_df$Ii[tasas_ntd_df$sexo== sexo])
  tasas_ntd_df[tasas_ntd_df$sexo== sexo,c(16)] <- tasas_ntd_df$suavizadas[tasas_ntd_df$sexo== sexo]-mean(tasas_ntd_df$suavizadas[tasas_ntd_df$sexo== sexo])
  

  
  
  
  print(sexo)
}

colnames()
