library(DBI)
library(RPostgres) # o RMySQL, o el conector correspondiente

# Conexión a PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  host = "127.0.0.1", # o el hostname del servidor
  port = 5432,
  dbname = "mi_base_datos",
  user = "mi_usuario",
  password = "mi_contraseña"
)

# Lista de tablas
dbListTables(con)

# Consulta de ejemplo
df <- dbGetQuery(con, "SELECT * FROM mi_tabla LIMIT 10")
print(df)

#Consulta de uso basado no query
df <- dbReadTable(con, "mi_tabla")

# Cerrar conexión
dbDisconnect(con)

# También existe un conexión más "directa" para trabajar con esta conexión
# siempre y cuando se implemente las siguiente convenciones
'https://www.postgresql.org/docs/current/libpq-envars.html'

# Puediendo realizar así la conexión de la siguiente forma
con <- dbConnect(RPostgreSQL::PostgreSQL())
