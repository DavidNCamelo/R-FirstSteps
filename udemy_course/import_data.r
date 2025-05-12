library(readxl)

#importing data from diverse sources

#importing .csv file
ventas <- read.csv("./R-FirstSteps/udemy_course/data/ventas_ejemplo.csv")

#importing .xlsc files
clients <- read_excel("./R-FirstSteps/udemy_course/data/clientes_ejemplo.xlsx")
#example to impot data from a specific excel sheet
#clients <- read_excel("./R-FirstSteps/data/clientes_ejemplo.xlsx", sheet = "hojax")

# Import from an API, and read .json files
library(httr)
library(jsonlite)

url <- "https://pokeapi.co/api/v2/pokemon/pikachu"
respond <- GET(url)

datos_pokemon <- content(respond, as = "text", encoding = "UTF-8")
datos_pokemon <- fromJSON(datos_pokemon)

# Accessing to the data acording to the .json structure
name <- datos_pokemon$name
weight <- datos_pokemon$weight
height <- datos_pokemon$height
type <- datos_pokemon$types$type$name

# Linsting and concatenating the accessed values
cat("Name:", name, 
  "\nWight:", weight,
  "\nHeight:", height,
  "\nType:", paste(type, collapse = ", ")
)
