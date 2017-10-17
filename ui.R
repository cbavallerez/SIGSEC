library(shiny)
library(shinydashboard)
library(leaflet)


header <- dashboardHeader(title = "BrillanteSegregacion")

sidebar <- dashboardSidebar(
  
  fluidPage(
    sidebarMenu(
      
      menuItem("Tabla", tabName = "tabla"),
      menuItem("Mapa", tabName = "mapa")
   
      
    ),
    selectInput("input_region", label = h4("Seleccionar Region"), 
                
                choices = c("Región de Tarapacá"=  1,
                            "Región de Antofagasta"= 2,
                            "Región de Atacama" = 3,
                            "Región de Coquimbo" = 4,
                            "Región de Valparaíso" = 5,
                            "Región del Libertador Gral. Bernardo O’Higgins" = 6,
                            "Región del Maule" = 7,
                            "Región del Biobío" = 8,
                            "Región de la Araucanía" = 9,
                            "Región de los Lagos" = 10,
                            "Región de Aysén del Gral. Carlos Ibáñez del Campo"= 11,
                            "Región de Magallanes y de la Antártica Chilena"= 12,
                            "Región de Metropolitana de Santiago"= 13,
                            "Región de Los Ríos"= 14,
                            "Región de Arica y Parinacota" = 15), 
                selected = 9
    ),
    uiOutput("seleccionar_comuna"),
    uiOutput("inputRBD"),
    uiOutput("establecimiento_datos")
    
  )
)
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    tags$script(src="js/leaflet.js"),
    tags$script(src="js/stabsel.js")
    
  ),
  
  fluidRow(
    column(width = 9,

   
             tabItem("mapa",
                     leafletOutput("mimapa", width="100%",height="600px")
             )
          )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               checkboxInput("estab_for_region", "Ver establecimientos por región", TRUE),
               checkboxInput("ver_shapefile", "Ver division comunal", FALSE)
           ),
           box(width = NULL, status = "warning",
               tags$ul(
                 tags$div(`class` = "colores tipo1"),
                 tags$li("Municipal"), 
                 tags$div(`class` = "colores tipo2"),
                 tags$li("Particular Subvenciando"),
                 tags$div(`class` = "colores tipo3"),
                 tags$li("Particular Pagado"), 
                 tags$div(`class` = "colores tipo4"),
                 tags$li("Corp. de Administracion delegada")
               )
           )
           
           
           
    )
  )


shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
)