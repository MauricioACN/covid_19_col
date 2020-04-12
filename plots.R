




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



##plot del pronostico de casos a 5 días
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
