# Required Libraries
library(dplyr)

# Load data
#importing .csv file
ventas <- read.csv("./R-FirstSteps/udemy_course/data/ventas_ejemplo.csv")

#importing .xlsc files
clients <- readxl::read_excel(
  "./R-FirstSteps/udemy_course/data/clientes_ejemplo.xlsx"
)

# Counr NA values
colSums(is.na(ventas))
colSums(is.na(clients))

# Dropping NA
ventas_sin_na <- na.omit(ventas)

# Verify if was deleted correctly
colSums(is.na(ventas_sin_na))

# Filling NA
ventas <- ventas %>%
  mutate(Precio = ifelse(is.na(Precio), 0, Precio))


ventas <- ventas %>%
  mutate(Total = ifelse(is.na(Total), 0, Total))

# Renaming columns
ventas <- ventas %>%
  rename(Producto_Vendido = Producto)

#colnames(ventas)[2] <- "Prod"

# Filtering
ventas_laptops <- ventas %>%
  filter(Producto_Vendido == "Laptop")


recurring_clients <- clients %>%
  filter(Compras > 5)

# Show a sample
head(ventas_laptops)
head(recurring_clients)

# Filtering with multiple conditions
ventas_grandes <- ventas %>%
  filter(Producto_Vendido == "Laptop" & Total > 1000)

# Filtering using subset function
v_subset_laptops <- subset(ventas, Producto_Vendido == "Laptop")


#Sorting Data

ventas_ordenadas <- ventas %>%
  arrange(Total)

# Showing data
head(ventas_ordenadas)

# Implementing select()
data(iris)
iris_tbl <- as_tibble(iris) #convert the called virtual dataset into a table

#Other way to review the data
glimpse(iris_tbl)

# selecting columns
iris_tbl %>% select(Sepal.Length, Species)

# Ignore specific column
iris_tbl %>% select(-Petal.Width)

# Selecting by starts characters
iris_tbl %>% select(starts_with("Petal"))

# Selecting by existing characters
iris_tbl %>% select(contains("Lengt"))

# Selecting and changing order
iris_tbl %>% select(Species, everything())

# Selecting columns based on variables
columns <- c("Sepal.Length", "Species")
iris_tbl %>% select(all_of(columns))

# Relocating columns
iris_tbl %>% relocate(Species)

# Relocating in an specific place
iris_tbl %>% relocate(Species, .after = Petal.Width)
iris_tbl %>% relocate(Species, .before = Petal.Width)

# NOW USING TIDYR

# Required library
library(tidyr)

# Grouping data
ventas_resumidas <- ventas %>%
  group_by(Producto_Vendido) %>%
  summarise(
    Total_Ventas = sum(Total),
    Promedio_Precio = mean(Precio),
    Cantidad_Total = sum(Cantidad)
  )

ventas_por_dia <- ventas %>%
  group_by(Fecha) %>%
  summarise(Total_Vendido = sum(Total))

# Mutating the dataset, adding calculate columns
ventas <- ventas %>%
  mutate(Categoria = ifelse(Total > 500, "Alta", "Baja"))

# Pivoting datasets
# pivot_longer convert the called columns into values of specified columns
ventas_pivot <- ventas %>%
  pivot_longer(
    cols = c(Precio, Cantidad, Total),
    names_to = "Métrica",
    values_to = "Valor"
  )

# pivot_wider convert the called elements into columns again
ventas_ancha <- ventas_pivot %>%
  pivot_wider(names_from = Métrica, values_from = Valor)

# Changing date format
ventas <- ventas %>%
  separate(Fecha, into = c("Año", "Mes", "Día"), sep = "-")


ventas <- ventas %>%
  unite("Fecha_Complete", Año, Mes, Día, sep = "-")
