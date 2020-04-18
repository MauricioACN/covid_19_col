#### Funciones 

## Función RMSE -----------------------------------------
RMSE = function(m, o){
  sqrt(mean((m - o)^2, na.rm = T))
}

## Función ncol in a list of dataframes -----------------------------------------

validacion = function(data){
  
  x = sapply(data,ncol)
  x = unique(x)
  
  if (length(x)==1) {
    
    return("todo cool")
    
  }
  
  else{
    
    return("Existen bases con diferente cantidad de columnas, validar datos")
    
  }
  
}

## Función preprocesamiento base -----------------------------------------

prepro_historico = function(datos){
  
  etiquetas <- c("ID_caso","Fecha_diagnositco","Ciudad","Depto","Atencion","Edad","Sexo","Tipo","Pais_contagio")
  bases <- lapply(datos, setNames, nm = etiquetas)
  bases <- lapply(bases,function(x){
    
    if(is.numeric(x$Edad)){
      
      x$grupo_edad = NA
      
    }
    else{
      
      x$grupo_edad = x$Edad
      x$Edad = NA
      
    }
    
    return(x)
  })
  ####
  base_casos <- bind_rows(bases,.id = "id")
  ###
  base_casos$id <- gsub("historico/","",base_casos$id)
  base_casos$id <- gsub(".csv","",base_casos$id)
  base_casos <- base_casos %>% mutate(id = as.Date(id,"%d-%m-%Y"),
                                      Fecha_diagnositco = as.Date(Fecha_diagnositco,"%d/%m/%Y"),
                                     Atencion = ifelse(Atencion %in% c("recuperado","Recuperado"),"Recuperado",
                                                       ifelse(Atencion %in% c("casa","Casa","En casa"),"En Casa",
                                                              ifelse(Atencion %in% c("hospital","Hospital"),"Hospital",
                                                                     ifelse(Atencion %in% c("fallecido","Fallecido"),"Fallecido",Atencion))))) %>% arrange(id)
  
  return(base_casos)

  }


## Function of clean a character variables -----------------------------------------

mix_clean = function(data,vec_var){

  base_editada = apply(data[,vec_var], 2, function(x){
    
    x= gsub("\\Ñ","n",x)
    x = gsub("\\ñ","n",x)
    x = tolower(x)
    x = gsub("[:punct:]","",x)
    x = chartr("áéíóúü","aeiouu",x)
    return(x)
  }) 
  return(base_editada)
  
}
