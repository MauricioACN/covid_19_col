#### librarias ####
library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)
#### Funciones #### 

## Función RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2, na.rm = T))
}


## función para calcular los casos agregados día a día

casos_agg = function(datos,n_casos){

  datos$casos <- NA
  for (s in 1:nrow(resumen)) {
    if (s==1) {
      datos$casos[s] <- resumen$n_casos[s]
    }
    else{
      datos$casos[s] <- resumen$casos[s-1]+resumen$n_casos[s]
    }
    print(s)
  }
  
    return(datos)
}



## función para calcular el factor de crecimiento
