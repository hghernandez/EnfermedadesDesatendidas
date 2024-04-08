library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)




load("datosMort/data.RData")

##%######################################################%##
#                                                          #
####             Genero un dataset para NTD             ####
#                                                          #
##%######################################################%##


chagas <- casos %>%
  filter(grepl("chagas",enf_desa_sub))


##%######################################################%##
#                                                          #
####               Poblaciones estimadas                ####
#                                                          #
##%######################################################%##


pob_deptos <- loadObject("PoblacionesEstimadas/pobdeptos0125.Rbin")


#Armo la poblacion para todo CABA

pob_caba <- pob_deptos %>%
  filter(codprov== 2) %>%
  group_by(ano,codprov,sexo,gredad) %>%
  summarise(poblacion= sum(poblacion))%>%
  mutate(coddep= 000,
         nomdep= 'Total CABA') %>%
  as.data.frame()

str(pob_caba)
#Armo la poblacion para todo el pais excepto caba

pob_sin_caba <- pob_deptos %>%
  filter(codprov!= 2) %>%
  group_by(ano,codprov,coddep,nomdep,sexo,gredad) %>%
  summarise(poblacion= sum(poblacion)) %>%
  as.data.frame()

str(pob_sin_caba)
#Unifico todo


pob_deptos <- pob_caba %>%
  bind_rows(pob_sin_caba)


#Armo una funcion para fixear los codigos

fix_cod <- function(x){
  
  x <- ifelse(stringr::str_length(x)== 1,paste0('00',x),
              ifelse(stringr::str_length(x)== 2,paste0('0',x),as.character(x))) 
  return(x)
}

str(pob_deptos)

#Fixeo coddep

pob_deptos <- pob_deptos %>%
  mutate(coddep= fix_cod(coddep),
         codprov= ifelse(codprov < 10,paste0('0',codprov),codprov))


#Uno los casos y la poblacion estimada
names(casos)
names(pob_deptos)

chagas <- pob_deptos %>%
  filter(ano >= 2004 & ano <= 2018) %>%
  left_join(chagas, by= c("ano","codprov"="provres","coddep"="depres","sexo","gredad"="edadquinq")) %>%
  mutate(link= paste0(codprov,coddep)) %>%
  fill(enf_desa_sub,.direction = "downup") %>%
  fill(enf_desa_tot,.direction = "downup")


#Seteo los valroes NULL a 0
chagas[is.na(chagas)] <- 0


##%######################################################%##
#                                                          #
####                  Calculo las RME                   ####
#                                                          #
##%######################################################%##


#Standar de casos

std =chagas %>%
  filter(gredad != '18.Total')%>%
  group_by(enf_desa_sub,sexo,gredad) %>%
  summarise(casos=sum(casos),
            poblacion=sum(poblacion)) %>%
  mutate(order= as.numeric(substring(gredad,1,2))) %>%
  arrange(sexo,order)

#casos


chagas_casos <- chagas %>%
  filter(gredad != '18.Total')%>%
  group_by(link,enf_desa_sub,sexo,gredad) %>%
  summarise(casos=sum(casos),
            poblacion=sum(poblacion)) %>%
  mutate(order= as.numeric(substring(gredad,1,2))) %>%
  arrange(link,sexo,order)



length(unique(ntd$link))

#Departamentos = 512
#Sexo= 3
# Lista de 1536 objetos (512*3)


tasas_chagas <- list()

for(i in 1:1536){
  for(j in 1:3){
    
    tasas_chagas[[i]] <- ageadjust.indirect(count = chagas_casos[(1+17*(i-1)):(17+17*(i-1)),5],
                                         pop= chagas_casos[(1+17*(i-1)):(17+17*(i-1)),6], 
                                         stdcount = std[(1+17*(j-1)):(17+17*(j-1)),4], 
                                         stdpop= std[(1+17*(j-1)):(17+17*(j-1)),5],
                                         stdrate = std[(1+17*(j-1)):(17+17*(j-1)),4]/std[(1+17*(j-1)):(17+17*(j-1)),5])
    
  }
  print(i)
}


#Guardo las tasas en un data frame

tasas_chagas_df <- as.data.frame(1:1536)

tasas_chagas_df$observados <- 0
tasas_chagas_df$esperados <- 0
tasas_chagas_df$RME <- 0
tasas_chagas_df$ic_inf <- 0
tasas_chagas_df$ic_sup <- 0


for(i in 1:1536){
  
  tasas_chagas_df[i,]$observados <- tasas_chagas[[i]]$sir[1]
  tasas_chagas_df[i,]$esperados <- tasas_chagas[[i]]$sir[2]
  tasas_chagas_df[i,]$RME <- tasas_chagas[[i]]$sir[3]*100
  tasas_chagas_df[i,]$ic_inf <- tasas_chagas[[i]]$sir[4]*100
  tasas_chagas_df[i,]$ic_sup <- tasas_chagas[[i]]$sir[5]*100
  print(i) 
  
}

#Agrego el sexo

s <- unique(chagas_casos$sexo)

tasas_chagas_df$sexo <- rep(s,512)

#Agrego el link

l <- unique(chagas_casos$link)

tasas_chagas_df$link <- rep(l,each= 3)

#agrego el evento

tasas_chagas_df[,1] <- "Chagas"

colnames(tasas_chagas_df)[1] <- 'Evento'

##%######################################################%##
#                                                          #
####                  Guardo las tasas                  ####
#                                                          #
##%######################################################%##

save(tasas_chagas_df,file="tasas/tasas_chagas.RData")
load("tasas/tasas_chagas.RData")


##%######################################################%##
#                                                          #
####                 Suavizo las tasas                  ####
#                                                          #
##%######################################################%##


#Excluyo antartida y malvinas


tasas_chagas_df <- tasas_chagas_df %>%
  filter(!link %in% c(94028,94021))

#Cargo la matriz de vecindad

matriz <- read.gal("matriz de vecindad/matriz")


#Suavizo las tasas

sexo = unique(tasas_chagas_df$sexo)

for(sexo in sexo){

tasas_chagas_df[tasas_chagas_df$sexo== sexo,9] <- EBlocal(tasas_chagas_df$observados[tasas_chagas_df$sexo== sexo],
        tasas_chagas_df$esperados[tasas_chagas_df$sexo== sexo],
        matriz)$est*100
print(sexo)
}

colnames(tasas_chagas_df)[9] <- 'suavizadas'

#Guardo las tasas

save(tasas_chagas_df,file="tasas/tasas_chagas_suav.RData")
