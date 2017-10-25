# install.packages("mongolite")
library(mongolite)

i = 0
dbEstablecimientos <- mongo("ESTABLECIMIENTOS", url = "mongodb://localhost:27017/sigsge")

# Read all the entries
establecimientos <- dbEstablecimientos$find()

shinyServer(function(input, output) {


  ubicacion_establecimiento <- function(input_region, cod_depe) {

    establecimiento_region <- subset (establecimientos, COD_REG_RBD==input_region & COD_DEPE2==cod_depe)
  
  }


  #################################################################################################################3
    
    output$establecimiento_datos = renderUI({
      # dbEstablecimientos <- mongo("ESTABLECIMIENTOS", url = "mongodb://localhost:27017/sigsge")
      establecimiento_seleccionado <-input$establecimiento_seleccionado
      # print(inputRBD)
      # Read all the entries
      ESTABLECIMIENTOS <- dbEstablecimientos$find(
        query = sprintf('{"RBD" : %s}',establecimiento_seleccionado)
        # ,fields = '{"_id" : false,"LATITUD" : true, "LONGITUD" : true, "COD_DEPE2" : true}'
      )
      
      if(!is.integer(ESTABLECIMIENTOS$CANT_ALUM)){
        dbAlumnos <- mongo("ALUMNOS", url = "mongodb://localhost:27017/sigsge")
        
      
        CANTIDAD_DE_ALUMNOS <- dbAlumnos$count(
          query = sprintf('{"RBD": %s}',input$establecimiento_seleccionado)
          )

        # dbEstablecimientos$update(
        # 
        #   query = sprintf('{"RBD": %s}',input$establecimiento_seleccionado ,'{"$set": {"CANT_ALUM" : %s}}',CANTIDAD_DE_ALUMNOS,' false',' true')
        #   )
        jsonRBD <- sprintf('{"RBD":%s}',input$establecimiento_seleccionado)
        jsonCANT_ALUM <- sprintf('{"$set":{"CANT_ALUM": %s}}',CANTIDAD_DE_ALUMNOS)
        
        dbEstablecimientos$update(jsonRBD, jsonCANT_ALUM)
        # print ("RBD: "+ repr(RBD_ESTABLECIMIENTO)+" -+- CANTIDAD DE ALUMNOS: "+ repr(CANTIDAD_DE_ALUMNOS) +" -+- PROGRESO: "+ repr(PROGRESO) +" -+- FECHA: "+ time.strftime("%H:%M:%S %d/%m/%y") )
        ESTABLECIMIENTOS$CANT_ALUM <-CANTIDAD_DE_ALUMNOS
      }
      if(!is.integer(ESTABLECIMIENTOS$CANT_ALUM_RBD)){
        dbALUMNOS_SEP <- mongo("ALUMNOS_SEP", url = "mongodb://localhost:27017/sigsge")
        
        
        CANTIDAD_DE_ALUMNOS_RBD <- dbALUMNOS_SEP$count(
          query = sprintf('{"RBD": %s}',input$establecimiento_seleccionado)
        )
        
        # dbEstablecimientos$update(
        # 
        #   query = sprintf('{"RBD": %s}',input$establecimiento_seleccionado ,'{"$set": {"CANT_ALUM" : %s}}',CANTIDAD_DE_ALUMNOS,' false',' true')
        #   )
        jsonRBD <- sprintf('{"RBD":%s}',input$establecimiento_seleccionado)
        jsonCANT_ALUM_RBD <- sprintf('{"$set":{"CANT_ALUM_RBD": %s}}',CANTIDAD_DE_ALUMNOS_RBD)
        
        dbEstablecimientos$update(jsonRBD, jsonCANT_ALUM_RBD)
        # print ("RBD: "+ repr(RBD_ESTABLECIMIENTO)+" -+- CANTIDAD DE ALUMNOS: "+ repr(CANTIDAD_DE_ALUMNOS) +" -+- PROGRESO: "+ repr(PROGRESO) +" -+- FECHA: "+ time.strftime("%H:%M:%S %d/%m/%y") )
        ESTABLECIMIENTOS$CANT_ALUM_RBD <-CANTIDAD_DE_ALUMNOS_RBD
      }
      # print(ESTABLECIMIENTOS)
      
      RBD_establecimiento_seleccionado <- as.numeric(input$inputRBD) 
      list(
        tags$p(class = "nombre_establecimiento","Nombre: ",ESTABLECIMIENTOS$NOM_RBD),
        tags$p("Longitud: ",ESTABLECIMIENTOS$LONGITUD),
        tags$p("Latitud: ",ESTABLECIMIENTOS$LATITUD),
        tags$p("Matricula:",ESTABLECIMIENTOS$CANT_ALUM),
        tags$p("Comuna: ",ESTABLECIMIENTOS$NOM_COM_RBD),
        tags$p("Cantidad de Alumnos SEP en la escuela: ",ESTABLECIMIENTOS$CANT_ALUM_RBD),
        tags$p("pi -> Proporcion SEP/ESC: "),
        tags$p("Indice Seg: ")
        
        
        
      )
    })
    
###########################################################################################################################3
  
  ubicacion_establecimiento_comuna<- function(input_comuna, cod_depe) {

    establecimiento_comuna <- subset (establecimientos, COD_COM_RBD==input_comuna & COD_DEPE2==cod_depe)
  }
  
  
  comunas <- function(input_region) {

    dbCOMUNAS <- mongo("COMUNAS", url = "mongodb://localhost:27017/sigsge")
    
    
    # Read all the entries
    COMUNAS <- dbCOMUNAS$find(
      query = sprintf('{"COD_REG" : %s}',input_region)
      # ,fields = '{"_id" : false,"LATITUD" : true, "LONGITUD" : true, "COD_DEPE2" : true}'
    )
    
    
  }
  
  #En la siguiente salida se crea un select para las comunas
  output$seleccionar_comuna = renderUI({
    
    
    dbCOMUNAS <- mongo("COMUNAS", url = "mongodb://localhost:27017/sigsge")
    
    
    # Read all the entries
    COMUNAS <- dbCOMUNAS$find(
      query = sprintf('{"COD_REG" : %s}',input$input_region)
      # ,fields = '{"_id" : false,"LATITUD" : true, "LONGITUD" : true, "COD_DEPE2" : true}'
    )
    
    
    RBD_establecimiento_seleccionado <- as.numeric(input$establecimiento_seleccionado) 
    choicesComunas <- setNames(COMUNAS$COD_COM,COMUNAS$NOM_COM )
    if(is.null(RBD_establecimiento_seleccionado)){
      selectInput("select_comuna", label = h4("Seleccionar Comuna"), choices = choicesComunas
      )
    }else{
      #selectInput("select_comuna", label = h4("Seleccionar Comuna"), choices = choicesComunas, selected = as.numeric(establecimiento_seleccionado(RBD_establecimiento_seleccionado)$COD_COM_RBD) 
      #)
      selectInput("select_comuna", label = h4("Seleccionar Comuna"), choices = choicesComunas)
    }
    
    
    
  })
  
  #Envia informacion sobre los establecimientos a una tabla 
  output$DatosEstablecimiento = renderDataTable({
    ubicacion_establecimiento(1,2)
  })
  
  
  #  Se declara la salida mimapa almacenando un renderleaflet
  output$mimapa <- renderLeaflet({ 
    
    
    #Se inicia la conexion con la base de datos
    dbRegiones <- mongo("REGIONES", url = "mongodb://localhost:27017/sigsge") 
    
    data <- dbRegiones$find(
      query = sprintf('{"ID" : %s}',input$input_region),
      fields = '{"_id" : false,"LATITUD" : true, "LONGITUD" : true}',
      limit = 5
    
    )
    
    #En la siguiente salida se envia a la UI.R un input del tipo numerico que servira pa seleecionar un establecimiento 
    output$inputRBD = renderUI({
      numericInput("establecimiento_seleccionado", h4("RBD"),  5578)
    })
    
    #Se carga la funcion leaflet con el argumento
    leaflet() %>% addTiles()%>%setView(lng = data[1,2], lat = data[1,1], zoom = 8)
    #cons<-dbListConnections(MySQL()) for(con in cons) dbDisconnect(con)
    
    
    
  })
  observe({
    if (input$estab_for_region == TRUE) {
      txtregion <-as.numeric(input$input_region)       
      
      leafletProxy("mimapa") %>%
        clearMarkers() %>%
        addCircleMarkers(lat =  ubicacion_establecimiento(txtregion,1)$LATITUD, lng =  ubicacion_establecimiento(txtregion,1)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#e52813"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento(txtregion,1)$RBD
        ) %>%
        addCircleMarkers(lat =  ubicacion_establecimiento(txtregion,2)$LATITUD, lng =  ubicacion_establecimiento(txtregion,2)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#ead942"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento(txtregion,2)$RBD
        ) %>%          
        addCircleMarkers(lat =  ubicacion_establecimiento(txtregion,3)$LATITUD, lng =  ubicacion_establecimiento(txtregion,3)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#61ab28"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento(txtregion,3)$RBD
        ) %>%          
        addCircleMarkers(lat =  ubicacion_establecimiento(txtregion,4)$LATITUD, lng =  ubicacion_establecimiento(txtregion,4)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#74402b"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento(txtregion,4)$RBD
        )        
      
      
      
      
    } else {
      
      
      if(is.null(input$select_comuna)){
        txtcomuna <- "9119"
        txtcomuna <- as.numeric(txtcomuna)
      } else{
        txtcomuna <-as.numeric(input$select_comuna)
      }
      # txtcomuna <-as.numeric(9101)       
      #, data = ubicacion_establecimiento_comuna(txtcomuna)
      leafletProxy("mimapa") %>%
        clearMarkers() %>%
        addCircleMarkers(lat =  ubicacion_establecimiento_comuna(txtcomuna,1)$LATITUD, lng =  ubicacion_establecimiento_comuna(txtcomuna,1)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#e52813"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento_comuna(txtcomuna,1)$RBD
        ) %>%
        addCircleMarkers(lat =  ubicacion_establecimiento_comuna(txtcomuna,2)$LATITUD, lng =  ubicacion_establecimiento_comuna(txtcomuna,2)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#ead942"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento_comuna(txtcomuna,2)$RBD
        ) %>%
        addCircleMarkers(lat =  ubicacion_establecimiento_comuna(txtcomuna,3)$LATITUD, lng =  ubicacion_establecimiento_comuna(txtcomuna,3)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#61ab28"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento_comuna(txtcomuna,3)$RBD
        ) %>%
        addCircleMarkers(lat =  ubicacion_establecimiento_comuna(txtcomuna,4)$LATITUD, lng =  ubicacion_establecimiento_comuna(txtcomuna,4)$LONGITUD, weight = 6, radius = 3,
                         
                         color = "#74402b"
                         
                         , stroke = TRUE, fillOpacity = 0.8, layerId = ubicacion_establecimiento_comuna(txtcomuna,4)$RBD
        )
    }
    
    
  })
  observeEvent(input$mimapa_marker_click, { # update the map markers and view on map clicks
    p <- input$mimapa_marker_click
    proxy <- leafletProxy("mimapa")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, 15)
      output$inputRBD = renderUI({
        numericInput("establecimiento_seleccionado", h4("RBD"), p$id  )
      })
      
    }
  })


  
  
  
  
  
  
  
  
  
#Fin del ShinyServer
})

