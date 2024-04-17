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
         uniedad == 1 & edad > 49 & edad <= 54 ~ '11.50-54',
         uniedad == 1 & edad > 54 & edad <= 59 ~ '12.55-59',
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

distr_edad <- casos %>%
  filter(sexo != '3.Ambos sexos' & edadquinq != '18.Total' & enf_desa_sub == '30.Total NTD') %>%
  group_by(edadquinq,sexo) %>%
  summarise(casos= sum(casos)) %>%
  arrange(sexo)

ggplot(distr_edad,aes(x= edadquinq, fill= sexo ,y= ifelse(sexo== '1.Varones',casos,-casos)))+
  geom_bar(stat= 'identity')+
  scale_y_continuous(
    labels = abs, 
    limits = max(distr_edad$casos) * c(-1,1)
  ) + 
  scale_fill_manual(values=c("#E6F5D0","#F1B6DA"),labels=c("Varones","Mujeres"))+
  scale_x_discrete(labels= substring(distr_edad$edadquinq,4,str_length(distr_edad$edadquinq)))+
  coord_flip() + 
  theme_minimal() +
  labs(
    x = "Edad Quinquenal", 
    y = "Casos", 
    fill = "Sexo", 
    title = "Distribución de los casos por edad"
  )
  
poblacion <- R.utils::loadObject("PoblacionesEstimadas/pobdeptos0125.Rbin")


fix_length <- function(x,y){
  
  x <- as.numeric(substring(x,1,2))
  
  x <- ifelse(x < 10,paste0('0',y),
              ifelse(x == 17,"17.80 y +",
                     ifelse(x== 11,"11.50-54",
                            ifelse(x== 12,"12.55-59",as.character(y)))))
  

  return(x)
  
  
}


pob_agrup <- poblacion %>%
  filter(ano >= 2004 & ano <= 2018)%>%
  mutate(gredad = fix_length(gredad,gredad)) %>%
  group_by(ano,gredad,sexo)%>%
  summarise(poblacion= sum(poblacion))

unique(poblacion$gredad)

tasas_esp <- casos %>%
  filter(enf_desa_sub == '30.Total NTD') %>%
  group_by(ano,edadquinq,sexo) %>%
  summarise(casos= sum(casos)) %>%
  left_join(pob_agrup,by= c("ano","edadquinq"="gredad","sexo")) %>%
  group_by(edadquinq,sexo)%>%
  summarise(casos= sum(casos),
            poblacion= sum(poblacion))%>%
  mutate(tasa= casos*1000000/poblacion) 



conf_interval <- function(d,p){
  
  li= d*((1 -(1/(9*d)) - (1.96/3)* sqrt(1/d)))^3/p
  ls= (d+1) * ((1 - (1/(9*(d+1))) + (1.96/3) * sqrt(1/(d+1))))^3 / p
  
  return(c(li,ls))

  
}


for(i in 1:nrow(tasas_esp)){
tasas_esp[i,6] <- conf_interval(tasas_esp$casos[i],tasas_esp$poblacion[i])[1]*1000000
tasas_esp[i,7] <- conf_interval(tasas_esp$casos[i],tasas_esp$poblacion[i])[2]*1000000 
}  

colnames(tasas_esp)[6] <- "IC_inf"
colnames(tasas_esp)[7] <- "IC_sup" 


ggplot(tasas_esp %>% filter(edadquinq != '18.Total'),
       aes(x= edadquinq, y= tasa, color= sexo))+
  geom_point()+
  geom_errorbar(aes(ymin= IC_inf, ymax= IC_sup))+
  scale_x_discrete(labels= substring(distr_edad$edadquinq,4,str_length(distr_edad$edadquinq)))+
  scale_color_manual(values=c("#7FBC41","#F1B6DA","#8E0152"),labels=c("Varones","Mujeres","Ambos sexos"))+
  labs(x= "Edad Agrupada", y= "Tasa de mortalidad \n (por 1 millon de hab.)",
       color= "Sexo")


ggplot(tasas_esp %>% filter(edadquinq == '18.Total'),
       aes(x= sexo, y= tasa, color= sexo))+
  geom_point()+
  geom_errorbar(aes(ymin= IC_inf, ymax= IC_sup))+
  scale_x_discrete(labels=c("Varones","Mujeres","Ambos sexos"))+
  scale_color_manual(values=c("#7FBC41","#F1B6DA","#8E0152"),labels=c("Varones","Mujeres","Ambos sexos"))+
  labs(x= "Edad Agrupada", y= "Tasa de mortalidad \n (por 1 millon de hab.)",
       color= "Sexo")


save(distr_edad,file="Reporte/piramide.RData")
save(tasas_esp,file="Reporte/tasas_esp.RData")

round(tasas_esp$casos[tasas_esp$sexo== '2.Mujeres' & tasas_esp$edadquinq == '18.Total']*100/tasas_esp$casos[tasas_esp$sexo== '3.Ambos sexos' & tasas_esp$edadquinq == '18.Total'])

scales::comma(tasas_esp$casos[tasas_esp$sexo== '3.Ambos sexos' & tasas_esp$edadquinq == '18.Total'],big.mark = ".",decimal.mark = ",")
