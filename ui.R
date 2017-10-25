# install.packages("shiny")
# install.packages("leaflet")
library(shiny)
library(leaflet)

htmlTemplate("template.html",
             mapa = leafletOutput("mimapa", width="100%",height="100%"),
             seleccionar_comuna = uiOutput("seleccionar_comuna"),
             inputRBD = uiOutput("inputRBD"),
             establecimiento_datos = uiOutput("establecimiento_datos")
             

)