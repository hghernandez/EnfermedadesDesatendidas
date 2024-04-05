library(tidyverse)
library(R.utils)
library(foreign)
library(sf)
library(epitools)
library(spdep)



##%######################################################%##
#                                                          #
####            cargo la mortalidad del 2004            ####
#                                                          #
##%######################################################%##

def04 <- read.dbf("datosMort/DEFT2004.dbf")
names(def04)

def04 <- def04 %>%
  select(ANO,CODMUER,EDAD,"UNIEDAD"=UNIEDA,SEXO,"PROVRES"=PROVRE,"DEPRES"=DEPRE)


def0518 <- loadObject("datosMort/deftot0518.Rbin")

#Uno toda la lista

def0518.df <- def0518[[1]] %>%
  select(ANO,CODMUER,EDAD,UNIEDAD,SEXO,PROVRES,DEPRES)


for(i in 2:length(def0518)){
  
  def0518.df <- rbind(def0518.df,def0518[[i]]  %>%
                        select(ANO,CODMUER,EDAD,UNIEDAD,SEXO,PROVRES,DEPRES))
  print(i)

}

#Unifico el dataset


def04$ANO <- as.numeric(as.character(def04$ANO))

dataset <- rbind(def04,def0518.df)

#LLevo las columnas a minusculas

names(dataset) <- tolower(names(dataset))


##%######################################################%##
#                                                          #
####                Genero Edad Agrupada                ####
#                                                          #
##%######################################################%##


dataset <- dataset %>%
  mutate(edadquinq = case_when(uniedad >= 2 & uniedad<= 4 ~ '01.0-4',
         uniedad== 1 & edad <= 4 ~ '01.0-4',
         uniedad == 1 & edad > 4 & edad <= 9 ~ '02.5-9',
         uniedad == 1 & edad > 9 & edad <= 14 ~ '03.10-14',
         uniedad == 1 & edad > 14 & edad <= 19 ~ '04.15-19',
         uniedad == 1 & edad > 19 & edad <= 24 ~ '05.20-24',
         uniedad == 1 & edad > 24 & edad <= 29 ~ '06.25-29',
         uniedad == 1 & edad > 29 & edad <= 34 ~ '07.30-34',
         uniedad == 1 & edad > 34 & edad <= 39 ~ '08.35-39',
         uniedad == 1 & edad > 39 & edad <= 44 ~ '09.40-44',
         uniedad == 1 & edad > 44 & edad <= 49 ~ '10.45-49',
         uniedad == 1 & edad > 49 & edad <= 54 ~ '11.49-54',
         uniedad == 1 & edad > 54 & edad <= 59 ~ '12.54-59',
         uniedad == 1 & edad > 59 & edad <= 64 ~ '13.60-64',
         uniedad == 1 & edad > 64 & edad <= 69 ~ '14.65-69',
         uniedad == 1 & edad > 69 & edad <= 74 ~ '15.70-74',
         uniedad == 1 & edad > 74 & edad <= 79 ~ '16.75-79',
         uniedad == 1 & edad > 79 ~ '17.80 y +', 
         uniedad >= 8 ~ '18.Sin especificar'))



##%######################################################%##
#                                                          #
####          Creo la variable enfdes Subtipo           ####
#                                                          #
##%######################################################%##


dataset <- dataset %>%
  mutate(enf_desa_sub= case_when(substring(codmuer,1,3)== 'B57' ~ '01.Enfermedad de chagas',
  substring(codmuer,1,3)== 'B55' ~ '02.Leishmaniasis',
  substring(codmuer,1,3)== 'B56' ~ '03.Tripanosomiasis gambiense',
  substring(codmuer,1,3)== 'B65' ~ '04.Esquistosomiasis',
  substring(codmuer,1,3)== 'B77' ~ '05.Ascariasis',
  substring(codmuer,1,3)== 'B76' ~ '06.Anquilostomiasis',
  substring(codmuer,1,3)== 'B79' ~ '07.Tricuriasis',
  substring(codmuer,1,3)== 'B73' ~ '08.Oncocercosis',
  substring(codmuer,1,3)== 'B68' ~ '09.Teniasis',
  substring(codmuer,1,3)== 'B69' ~ '10.Cisticercosis',
  substring(codmuer,1,3)== 'B67' ~ '11.Equinococosis',
  substring(codmuer,1,3)== 'B74' ~ '12.Filariasis',
  substring(codmuer,1,3)== 'B72' ~ '13.Dracontiasis',
  substring(codmuer,1,4)== 'B660' ~ '14.Opistorquiasis',
  substring(codmuer,1,4)== 'B663' ~ '16.Fascioliasis',
  substring(codmuer,1,4)== 'B664' ~ '17.Paragonimiasis',
  substring(codmuer,1,3)== 'A30' ~ '18.Lepra',
  substring(codmuer,1,3)== 'B92' ~ '18.Lepra',
  substring(codmuer,1,3)== 'A71' ~ '19.Tracoma',
  substring(codmuer,1,4)== 'A311' ~ '20.Infeccion cutanea por micobacterias (Úlcera de Buruli)',
  substring(codmuer,1,3)== 'A66' ~ '21.Frambesia',
  substring(codmuer,1,3)== 'A67' ~ '22.Pinta',
  substring(codmuer,1,3)== 'A65' ~ '23.Sifilis no venerea',
  substring(codmuer,1,3)== 'A82' ~ '24.Rabia',
  substring(codmuer,1,3)== 'A90' ~ '25.Dengue',
  substring(codmuer,1,3)== 'A91' ~ '26.Dengue hemoragico',
  substring(codmuer,1,4)== 'A920' ~ '27.Enfermedad por virus Chikungunya',
  substring(codmuer,1,4)== 'A923' ~ '28.Enfermedad viral del oeste del Nilo',
  substring(codmuer,1,3)== 'A95' ~ '29.Fiebre Amarilla'))


##%######################################################%##
#                                                          #
####            Creo la variable enfdes tot             ####
#                                                          #
##%######################################################%##


dataset <- dataset %>%
  mutate(enf_desa_tot= case_when(substring(codmuer,1,3)>= 'B55' & substring(codmuer,1,3) <= 'B57' ~ '01.Protozoarios',
                                 substring(codmuer,1,3)>= 'B65' & substring(codmuer,1,3) <= 'B79' ~ '02.Helmintos',
                                 substring(codmuer,1,3)>= 'A30' & substring(codmuer,1,3) <= 'A71' | substring(codmuer,1,3) == 'B92' ~ '03.Bacterias'))


##%######################################################%##
#                                                          #
####           Arreglo el formato de provres            ####
#                                                          #
##%######################################################%##



dataset <- dataset %>%
  mutate(provres = ifelse(provres < 10,paste0("0",provres),provres))

View(dataset %>%
  group_by(provres) %>%
  summarise(n= n()))


##%######################################################%##
#                                                          #
####             Arreglo la variable sexo               ####
#                                                          #
##%######################################################%##

dataset <- dataset %>%
  mutate(sexo= case_when(sexo== 1 ~ '1.Varones',
                         sexo== 2 ~ '2.Mujeres',
                         sexo== 3 ~ '9.Indeterminado',
                         sexo== 9 ~ '9.Mal definido'))


##%######################################################%##
#                                                          #
####              Creo el dataset de casos              ####
####           #sumo total edad y total sexo            ####
#                                                          #
##%######################################################%##

View(dataset %>%
  group_by(provres,depres) %>%
  summarise(n= n()))

casos_sin_caba <- dataset %>%
  filter(!is.na(enf_desa_sub) & provres != '02')%>%
  group_by(ano,provres,depres,enf_desa_sub,enf_desa_tot,edadquinq,sexo) %>%
  summarise(casos= n()) %>% #Agrego total edad
  bind_rows(group_by(.,ano,provres,depres,enf_desa_sub,enf_desa_tot,sexo)%>%
              summarise(casos=sum(casos))%>%
              mutate(edadquinq= '18.Total')) %>% #Agrego Ambos Sexos
  bind_rows(group_by(.,ano,provres,depres,enf_desa_sub,enf_desa_tot,edadquinq)%>%
              summarise(casos=sum(casos))%>%
              mutate(sexo= '3.Ambos sexos')) %>% #Genero un total NTD
  bind_rows(group_by(.,ano,provres,depres,edadquinq,sexo)%>%
              summarise(casos=sum(casos))%>%
              mutate(enf_desa_sub= '30.Total NTD',
                     enf_desa_tot='04.Total NTD')) %>%
  filter(substring(sexo,1,1) != '9' & edadquinq != "18.Sin especificar")


casos_caba <- dataset %>%
  filter(!is.na(enf_desa_sub) & provres == '02')%>%
  group_by(ano,provres,enf_desa_sub,enf_desa_tot,edadquinq,sexo) %>%
  summarise(casos= n()) %>% #Agrego total edad
  bind_rows(group_by(.,ano,provres,enf_desa_sub,enf_desa_tot,sexo)%>%
              summarise(casos=sum(casos))%>%
              mutate(edadquinq= '18.Total')) %>% #Agrego Ambos Sexos
  bind_rows(group_by(.,ano,provres,enf_desa_sub,enf_desa_tot,edadquinq)%>%
              summarise(casos=sum(casos))%>%
              mutate(sexo= '3.Ambos sexos')) %>% #Genero un total NTD
  bind_rows(group_by(.,ano,provres,edadquinq,sexo)%>%
              summarise(casos=sum(casos))%>%
              mutate(enf_desa_sub= '30.Total NTD',
                     enf_desa_tot='04.Total NTD')) %>%
  filter(substring(sexo,1,1) != '9' & edadquinq != "18.Sin especificar") %>%
  mutate(depres = 000)
  

#Unifico los casos

casos <- casos_caba %>%
  bind_rows(casos_sin_caba)

#Armo una funcion para fixear los codigos

fix_cod <- function(x){
  
  x <- ifelse(stringr::str_length(x)== 1,paste0('00',x),
              ifelse(stringr::str_length(x)== 2,paste0('0',x),as.character(x))) 
  return(x)
}

#Fixeo depres

casos <- casos %>%
  mutate(depres= fix_cod(depres))

#Guardo los casos

save(casos, file="datosMort/data.RData")
load("datosMort/data.RData")





