# Papildus nosacījums SQL pieprasījumam.
customer_ids <- "(100, 105)"

# Izveidot savienojumu.
library(RPostgreSQL)
db_driver <- dbDriver("PostgreSQL")
db_connection <- dbConnect(drv      = db_driver, 
                           host     = "ip_address", 
                           dbname   = "my_database_name",
                           user     = "username", 
                           password = "password",
                           port     = 5432)

# Importēt SQL sagatavi un papildināt ar vēl vienu nosacījumu.
query <- paste0(readLines("sql_template.sql"), collapse = "\n")
query <- paste0(query, "\n", 
                "AND customers.id in ", customer_ids, "\n")

# Eksportēt papildinātu SQL.
writeLines(query, con = "sql_used.sql")

# Uzņemt laiku un izpildīt SQL pieprasījumu.
start.time <- Sys.time()
sql_result <- dbGetQuery(conn      = db_connection, 
                         statement = query)
Sys.time() - start.time
