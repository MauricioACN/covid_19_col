
source("global.R")

#### Cálculo de factor de crecimiento del covid-19 -----------------------------------------

#Reusmen de casos agrgados por día
resumen = datos %>% group_by(Fecha) %>% summarize(cuenta = n())
#calculo de casos acumulados
resumen$casoso_acum <- cumsum(resumen$cuenta)
## Creación del factor de crecimiento
resumen$fact = resumen$casoso_acum / lag(resumen$casoso_acum)


#Tipo de casos por edad -----------------------------------------

edad_casos = datos %>% group_by(grup_edad) %>% summarize(recuperados = sum(recuperados,na.rm = T),
                                                    Feallecido = sum(Feallecido,na.rm = T),
                                                    Grave = sum(Grave,na.rm = T),
                                                    Casa = sum(Casa,na.rm = T),
                                                    Hospital = sum(Hospital,na.rm = T))

### creación de promedios moviles -----------------------------------------

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


### Cálculo del RMSE -----------------------------------------

results_rmse = data.frame(media_movil_2 = RMSE(m = resumen$casos_pron_2, o = resumen$casos),
                          media_movil_3 = RMSE(m = resumen$casos_pron_3, o = resumen$casos),
                          media_movil_4 = RMSE(m = resumen$casos_pron_4, o = resumen$casos))



#### Pronositco de casos -----------------------------------------

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


#### export datos -----------------------------------------
#resumen_edit_export = resumen_edit %>% select(Fecha,casos,casos_pronostico)
#datos_24 = write.csv(resumen_edit_export, "resumen_24_marzo.csv")


