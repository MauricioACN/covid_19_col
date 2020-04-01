### librarias
library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)
###
RMSE = function(m, o){
  sqrt(mean((m - o)^2, na.rm = T))
}
###
datos <- read.csv("Casos_colombia.csv", header = T,sep = ",", stringsAsFactors = F, encoding = "UTF-8")
names(datos)[c(1,2,3,8,9)] <- c("ID","Fecha","Ubicacion","Tipo","Pais_procedencia")
#preprocesamiento
datos = datos %>% mutate(Fecha = as.Date(Fecha, "%d/%m/%Y"))
#datos agrgados
resumen = datos %>% group_by(Fecha) %>% summarize(cuenta = n())
#Plot casos por día
p <- ggplot(data=resumen, aes(x=Fecha, y=cuenta))+geom_line()+geom_point()
fig <- ggplotly(p)
## casos agregados por día
resumen$casos <- NA
for (s in 1:nrow(resumen)) {
  if (s==1) {
    resumen$casos[s] <- resumen$cuenta[s]
  }
  else{
    resumen$casos[s] <- resumen$casos[s-1]+resumen$cuenta[s]
  }
  print(s)
}
#Plot 
#Plot casos agregados por día
xaxis <- list(title = "Fecha de Suceso",
              showline = F,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2)

yaxis <- list(title = "Cantidad de Casos",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2)
fig <- plot_ly(resumen, x = ~Fecha) 
fig <- fig %>% add_trace(y = ~casos, type = 'scatter', mode = 'lines', line = list(color = 'rgba(67,67,67,1)', width = 2), name = "Casos Reales")
fig <- fig %>% layout(title = "Casos COVID-19 en Colombia", xaxis = xaxis, yaxis = yaxis)
## Creación del factor de crecimiento
resumen$factor_crec <- NA
for (s in 1:nrow(resumen)) {
  if (s==1) {
    resumen$factor_crec[s] <- NA
  }
  else{
    resumen$factor_crec[s] <- resumen$casos[s]/resumen$casos[s-1]
  }
  print(s)
}
### creación de promedios moviles
resumen = resumen %>% mutate(media_movil_2 = rollmean(factor_crec, k = 2 , fill = NA, align = "right"),
                             media_movil_3 = rollmean(factor_crec, k = 3 , fill = NA, align = "right"),
                             media_movil_4 = rollmean(factor_crec, k = 4 , fill = NA, align = "right"))
### Aplicación de medias moviles
resumen$casos_pron_2 <- NA
resumen$casos_pron_3 <- NA
resumen$casos_pron_4 <- NA
for (s in 1:nrow(resumen)) {
  if (s==1) {
    resumen$casos_pron_2[s] <- NA
    resumen$casos_pron_3[s] <- NA
    resumen$casos_pron_4[s] <- NA
  }
  else{
    resumen$casos_pron_2[s] <- round(resumen$casos[s-1]*resumen$media_movil_2[s],0)
    resumen$casos_pron_3[s] <- round(resumen$casos[s-1]*resumen$media_movil_3[s],0)
    resumen$casos_pron_4[s] <- round(resumen$casos[s-1]*resumen$media_movil_4[s],0)
  }
  print(s)
}
### Cálculo del RMSE
results_rmse = data.frame(media_movil_2 = RMSE(m = resumen$casos_pron_2, o = resumen$casos),
                          media_movil_3 = RMSE(m = resumen$casos_pron_3, o = resumen$casos),
                          media_movil_4 = RMSE(m = resumen$casos_pron_4, o = resumen$casos))
####
resumen_edit = resumen %>% select(-c(media_movil_3,media_movil_4,casos_pron_3,casos_pron_4))
casos = nrow(resumen_edit)
resumen_edit[nrow(resumen_edit)+5,] <- NA
resumen_edit$casos_pronostico <- NA
resumen_edit$casos_pronostico[casos] <- resumen_edit$casos[casos]
for (s in (casos+1):nrow(resumen_edit)) {
  resumen_edit$casos_pronostico[s] <- round(resumen_edit$media_movil_2[casos]*resumen_edit$casos_pronostico[s-1],0)
  resumen_edit$factor_crec[s] <- resumen_edit$casos_pronostico[s]/resumen_edit$casos_pronostico[s-1]
  resumen_edit$media_movil_2[s] <- mean(resumen_edit$factor_crec[(s-1):s])
  resumen_edit$Fecha[s] <- resumen_edit$Fecha[s-1]+1
  print(s)
}
##plot
xaxis <- list(title = "Fecha de Suceso",
              showline = F,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2)

yaxis <- list(title = "Cantidad de Casos",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2)
###
###
fig <- plot_ly(resumen_edit, x = ~Fecha) 
fig <- fig %>% add_trace(y = ~casos, type = 'scatter', mode = 'lines', line = list(color = 'rgba(67,67,67,1)', width = 2), name = "Casos Reales")
fig <- fig %>% add_trace(y = ~casos_pronostico, type = 'scatter', mode = 'lines', line = list(color = 'rgba(239,35,35,1)', width = 2), name = "Pronóstico")
fig <- fig %>% layout(title = "Pronóstico casos COVID-19 en Colombia", xaxis = xaxis, yaxis = yaxis)
#### export datos
#resumen_edit_export = resumen_edit %>% select(Fecha,casos,casos_pronostico)
#datos_24 = write.csv(resumen_edit_export, "resumen_24_marzo.csv")
