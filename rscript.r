library(mongolite)
m <- mongo("ESTABLECIMIENTOS", url = "mongodb://localhost:27017/sigsge")
m$find()


library(mongolite)

options(mongodb = list(
  "host" = "localhost:27017",
  "username" = "admin",
  "password" = "admin"
))
databaseName <- "sigsge"
collectionName <- "ESTABLECIMIENTOS"

db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data

  
  ubicacion_establecimiento <- function() {
    # Connect to the database
    m <- mongo("ESTABLECIMIENTOS", url = "mongodb://localhost:27017/sigsge")
    
    
    # Read all the entries
    data <- m$find()
    data
  }
  
  #Envia informacion sobre los establecimientos a una tabla 
ubicacion_establecimiento()



# -------------------------------------
# Connect to the database
db <- mongo("ESTABLECIMIENTOS", url = "mongodb://localhost:27017/sigsge")


# Read all the entries
establecimientos_region <- db$find(
  query = sprintf('{"COD_REG_RBD" : %d , "COD_DEPE2" :  %d}',input_region, cod_depe)
  ,fields = '{"_id" : false,"LATITUD" : true, "LONGITUD" : true, "COD_DEPE2" : true}'
)

return(establecimientos_region)