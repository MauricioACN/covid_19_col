
#### Library -----------------------------------------

library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)
source("functions.R")

#### Especific Data ----------------------------------------- 

datos <- read.csv("historico/14-04-2020.csv", header = T,sep = ",", stringsAsFactors = F, encoding = "UTF-8")
enfermedad = data.frame(enfermedad = c("Enfermedades Cardiovasculares","Cáncer","EPOC*","Diabetes","Hipertensión"),
                        riesgo = c(0.105,0.056,0.063,0.073,0.06))

#### preprocesing -----------------------------------------

names(datos)[c(1,2,3,8,9)] <- c("ID","Fecha","Ubicacion","Tipo","Pais_procedencia")
datos = datos %>% mutate(Fecha = as.Date(Fecha, "%d/%m/%Y"))
datos1 = datos %>% select(ID, Atención..) %>% mutate(n = 1) %>% spread(key = Atención..,value = n,fill = 0)
datos = left_join(datos,datos1, by= c("ID"))
grup_edades = (seq.int(from = 1,to = 100,by = 10))-1
datos$grup_edad = findInterval(datos$Edad, grup_edades) %>% as.factor()
etiquetas = c("<10","10_19","20_29","30_39","40_49","50_59","60_69","70_79","80_89","90_99")
levels(datos$grup_edad) <- etiquetas

#### Historical Information Preprocesing -----------------------------------------

#### Use a directory with the all dataframes that you need add
bases <- list.files(path = "historico",pattern = "*.csv",full.names = T)
bases <- sapply(bases, read_csv, simplify=FALSE)

validacion(bases) #Use to determinate if you need change a specific dataframe | only analyze if the dataframes has the same number of colums

#### Use when the colnames does't the same in all dataframes
bases[[16]] = bases[[16]][,1:9] 
bases[[16]]$Edad = as.numeric(ifelse(stringr::str_detect(bases[[16]]$Edad," meses| mes"),0,bases[[16]]$Edad))

#### Continue here if you dont need apply the above situation
ids_erroneos = readRDS("ids_erroneos.rds") # This ids had a diferent age values in the window time

base_casos = prepro_historico(datos = bases) # Apply the preprocesing function and create the finally dataframe
bases <- lapply(bases, mix_clean) 




