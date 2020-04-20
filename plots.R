

##### Plot casos por día ------------------------------------------------

p <- ggplot(data=resumen, aes(x=Fecha, y=cuenta))+geom_line()+geom_point()
fig <- ggplotly(p)


##### grupos de edad ------------------------------------------------

fig <- plot_ly(edad_casos, x = ~grup_edad, y = ~Grave, type = 'bar', name = 'Hospitalizado en UCI')
fig <- fig %>% add_trace(y = ~Hospital, name = 'Hospitalizado')
fig <- fig %>% add_trace(y = ~recuperados, name = 'Recuperados')
fig <- fig %>% add_trace(y = ~Feallecido, name = 'Feallecido')
fig <- fig %>% layout(title = 'Casoso de COVI-19 por tipo',yaxis = list(title = 'Casos'), barmode = 'stack')



##### Plot casos agregados por día ------------------------------------------------

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


##### plot del pronostico de casos a 5 días ------------------------------------------------ 

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
fig <- plot_ly(resumen_edit, x = ~Fecha) 
fig <- fig %>% add_trace(y = ~casos, type = 'scatter', mode = 'lines', line = list(color = 'rgba(67,67,67,1)', width = 2), name = "Casos Reales")
fig <- fig %>% add_trace(y = ~casos_pronostico, type = 'scatter', mode = 'lines', line = list(color = 'rgba(239,35,35,1)', width = 2), name = "Pronóstico")
fig <- fig %>% layout(title = "Pronóstico casos COVID-19 en Colombia", xaxis = xaxis, yaxis = yaxis)


##### plot del comortamiento de sintomas ------------------------------------------------ 

hist = plot_ly(x = resumen$Conteo, type = "histogram")

xaxis <- list(title = "Fecha de Inicio de Sintomas",
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

cuarentena = resumen %>% filter(FIS == "2020-03-24")
cuarentena2 = resumen %>% filter(FIS == "2020-03-06")

t2 <- list(
  x = cuarentena2$FIS,
  y = cuarentena2$Conteo,
  text = "Primer Caso Confirmado",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = -5,
  ay = -40
)

t <- list(
  x = cuarentena$FIS,
  y = cuarentena$Conteo,
  text = "Inicio Cuarentena",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)


fig <- plot_ly(resumen, x = ~FIS) 
fig <- fig %>% add_trace(y = ~Conteo, type = 'scatter', mode = 'lines', line = list(color = 'rgba(67,67,67,1)', width = 2), name = "Cantidad de Casos")
fig <- fig %>% layout(title = "Comportamiento de los primeros sintomas de COVID-19 en Colombia", xaxis = xaxis, yaxis = yaxis, annotations = t)
fig <- fig %>% layout(annotations = t2)
