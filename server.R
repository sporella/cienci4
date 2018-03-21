library(shiny)
library(rhandsontable)
library(leaflet)
library(raster)
library(htmltools)
library(rgdal)
library(dplyr)
library(googlesheets)
library(httr)

########################################################
iris1<-read.csv("tabla1.csv", header=T, sep=",", stringsAsFactors = F)
iris1<-iris1[,-c(2:3)]


table <- "responses1"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  withProgress(gs_add_row(sheet, input = data),min = 0, max=3, value =  c(1:4), message = "Guardando Información:", 
               detail= " Este proceso puede llevar unos segundos, no abandone la página hasta visualizar el mensaje de confirmación") 
}


shinyServer(function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  source("www/Login.R",  local = TRUE)
  
  getDat <- reactive({
    
        
        setuser <- isolate(input$userName)
                
        
        if (!is.null(setuser)) {
          
      Dat <- sample_n(iris1[which(iris1[,1] %in% setuser),],15)
      colnames(Dat)[c(1,4)]<-c("USUARIO", "SERVICIO ECOSISTÉMICO")

        } else {
          Dat <- NULL
        }
        
    return(Dat)
  })
  
  getSHP <- reactive({
    withProgress(
      message = 'Preparando Sitio: ',
      detail = 'cargando Clasificación de Ecosistemas', value=0 , {
    setuser <- isolate(input$userName)
    incProgress(0.5)
                  
        if (!is.null(setuser)) {

          SHP <-  shapefile(paste0(as.character(unique(iris1$MACROZONA[which(iris1[,1]==setuser)])[1]), ".shp"))
        } else {
          SHP <- NULL
        }
    setProgress(1)
      })
              
    return(SHP)
  }) 
  
  output$obs <- renderUI({

    setuser <- isolate(input$userName)
    
    if (USER$Logged == TRUE) {
      dat<-getDat()
      list(
        helpText("Puede visualizar la ubicación de un ecosistema en el siguiente mapa:"),
        selectizeInput(
          'selectecosistema', 'Seleccione Ecosistema', choices = as.character(unique(dat$ECOSISTEMA)),
          options = list(
            placeholder = 'Seleccione un ecosistema',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    }                   
  })
       
  
  output$dataTable <- renderUI({    
    if (USER$Logged == TRUE ) {
      observe("mymap")
      list(
        tags$style(HTML("
                    li {
                    font-size: 16px;
list-style-type: square;
                    }
                    li span {
                    font-size: 18px;
                    }
                    ul {
                    list-style-type: square;
                    }

                    ")),  
      h4("Objetivo: Editar la tabla de valoración y certeza de servicios ecosistémicos por ecosistema"),
      tags$li("Puede editar haciendo doble click sobre las celdas. La tabla funciona similar a una hoja de Excel"),
      tags$li("Criterios de valoración:números enteros entre 1 y 5" ),
      tags$li("Criterios de certeza:números enteros entre 1 y 3"),
      tags$li("Si el Servicio Ecosistémico no aplica para el ecosistema señalado, rellenar con: Valoración = 0 Certeza =3"),
      tags$li("Valores incorrectos se marcan en rojo"),  
      hr(),
      rHandsontableOutput('table'),
      helpText("No olvide enviar sus respuestas al finalizar"),
      actionButton("save", "Enviar Información")
      )
      }
      })
      
  
     
  output$table <- renderRHandsontable({
    tt<-getDat()
    MAT_comments = matrix(ncol = ncol(tt), nrow = nrow(tt))
    MAT_comments[, 4] = paste("Ejemplo:", tt$EJEMPLO)
    
      rhandsontable(tt[,1:6], useTypes = FALSE, stretchH = "all",highlightCol = TRUE, highlightRow = TRUE,rowHeaders = NULL,comments = MAT_comments,
                   readOnly = T)%>%
      hot_col(5, readOnly = FALSE)%>%
      hot_col(6, readOnly = FALSE)%>%
      hot_validate_numeric(col = 5, choices = c(0,1, 2, 3,4,5), allowInvalid = T)%>%
      hot_validate_numeric(col = 6, choices = c(1, 2, 3), allowInvalid = T)%>%
      hot_cols(colWidths = 160) %>%
      hot_rows(rowHeights = 160)%>%
      hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (col===4 || col==5) td.style.background = 'lightyellow';
  }")          
        })
  
  values <- reactiveValues()
  
  observe({
    if (!is.null(input$table)) {
            DF = hot_to_r(input$table)
    } else {
      if (is.null(values[["DF"]]))
        DF <- NA
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveData(finalDF)
  })
  
  output$map <- renderUI({    
    if (USER$Logged == TRUE) {      
      leafletOutput('mymap')
    }
  })
  
  output$mymap <- renderLeaflet({
    shp<-getSHP()
    shp<-shp[which(shp$tabla_c_11==as.character(input$selectecosistema)),]
    leaflet() %>% addTiles() %>% addMiniMap(width = 100, height = 100)%>% addPolygons(data=shp, weight=2, popup = ~htmlEscape(tabla_c_11), color = "orange", fillOpacity = 0.9)
  })
  
  observeEvent(input$save, {
    showModal(modalDialog(
      title = "Sus respuestas fueron enviadas de manera exitosa",
      "Agradecemos su participación en este proceso, sus respuestas son un factor fundamental para la clasificación de Ecosistemas Acuáticos en Chile",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
})