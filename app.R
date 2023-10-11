library(shiny)

library(tidyverse)
library(readr)
# 2023GestFinNomina - Notas_evuacion.csv
data2 <- read_csv("2023GestFinNomina - Notas_evuacion (1).csv", 
                  locale = locale(decimal_mark = ","))

data3 <- read_csv("2023GestFinNomina - Consolidado (3).csv", 
                  locale = locale(decimal_mark = ","))

data4 <- read_csv("2023GestFinNomina - Nota par 1.csv", 
                  locale = locale(decimal_mark = ","))

names(data3)[3] <- "sis_id"
names(data3)[6] <- "Diagnostico"
names(data4)[3] <- "Rut"

data2$Particip <- (data2$`Nota 0`+data2$`Nota 1`+data2$`Nota 2`+data2$`Nota 3`)/2

data3$Promedio_controles <- (data3$`Diagnostico`+data3$`Control 1`+data3$`Control 2`+data3$`Control 3`)/4
data3$Promedio_participacion <- (data3$`Participación 1`+data3$`Participación 2`+data3$`Participación 3`+data3$`Participación 4`)/4
data3$Promedio <- (data3$Promedio_controles + data3$Promedio_participacion)/2



# data2 <- data2 %>% select(sis_id,Nombre,`Controles y Tareas`,`Prueba 1`,`Prueba 2`,Proyecto,Examen,Part,`Bono Part`,Final)
# data2 <- data2[24:57,]
data2 <- rbind(data2,c("NOMBRE",NA,"123456789",mean(data2$`Nota 0`),mean(data2$`Nota 1`),mean(data2$Particip)))
data2$Particip <- as.numeric(data2$Particip)
# data2$`Prueba 1`[34] <- NA
# mean(data$`Parte 1`,na.rm=T)
# data2$`Prueba 2`[34] <- NA
# mean(data$`Parte 2`,na.rm=T)
# data2$pond[34] <- NA
# data2$Controles[34] <- NA
# data2$Participación[34] <- NA
# data2$Proyecto[34] <- NA
# data2$Examen[34] <- NA
# data2$Final[34] <- NA
# data2$Part[34] <- NA
# data2$`Bono Part`[34] <- NA

# USER INTERFACE
ui <- fluidPage(
  
  titlePanel("Resultados Gestion Financiera 2023-20"),
  
  
  mainPanel(
    sidebarPanel(
      textInput("rut",
                "RUT (sin guión ni puntos):",
                value = "123456789")
    ),
    h3(textOutput("selected_var")),
    # h4(textOutput("ex")),
    # h4(textOutput("part")),
    plotOutput("distPlot"),
    br(),
    br(),
    h4("El detalle de notas es:"),
    br(),
    fluidRow(
      dataTableOutput('tabla'),
      h6("Actualización: 21 Ago 2023")
    )
  )
)

# SERVIDOR CON OBJETOS DEPENDIENTES DEl input$rut
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    #HISTOGRAMA
    hist(data2$Particip, col = 'darkgray', border = 'white',
         xlab = 'Notas',
         main = 'Distribución Nota Participación')
    abline(v = (data3 %>% filter(`sis_id`==input$rut) %>% select(Promedio))$Promedio, col="red", lwd=3, lty=2)
    legend("bottomleft", legend = "Nota Ponderada", pch = "|", col = "red")
  })
  
  # NOMBRE ALUMNO
  output$selected_var <- renderText({ 
    paste(data3$Student[which(data3$`sis_id`==input$rut)]," tu nota de participación promedio por ahora es", round((data3 %>% filter(sis_id==input$rut) %>% select(Promedio))$Promedio,digits=1))
  })
  # MENSAJE EXIMICION 
  output$ex <- renderText({ 
    paste(ifelse(round(data2 %>% filter(sis_id==input$rut) %>% select(pond),digits=1)>5.9,"Te eximiste y validaste el ejercicio",""))
  })
  # MENSAJE PARTICIPACIÓN 
  output$part <- renderText({ 
    paste(ifelse(((data2 %>% filter(sis_id==input$rut) %>% select(Part))=="CUMPLE")[1],
                 paste("Cumples el criterio de participación y tienes un bono en la nota final de ",as.numeric(data2 %>% filter(sis_id==input$rut) %>% select(`Bono Part`))*10," décima(s)."),
                 "No cumpliste el criterio de participación"))
  })
  
  # TABLA CON DETALLE NOTAS
  output$tabla <- renderDataTable(data3 %>% filter(sis_id==input$rut) %>% select(`Diagnostico`:Promedio),
                                  options = 
                                    list(searching = FALSE, paging=FALSE) )
  
  #TABLA CON DETALLE DE NOMINACIONES
  output$tabla2 <- renderDataTable(data4 %>% filter(Rut==input$rut) %>% select(mejor, mencion),
                                   options = 
                                     list(searching = FALSE, paging=FALSE) )
  
}

# Run the application 
shinyApp(ui = ui, server = server)