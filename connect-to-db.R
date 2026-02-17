library(DBI)
library(RMariaDB)
library(jsonlite)

server_credentials <- jsonlite::read_json("server-credentials.json")

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = server_credentials$dbname,
  username = server_credentials$username,
  password = server_credentials$password,
  host = "96.125.26.54",
  port = server_credentials$port
)
