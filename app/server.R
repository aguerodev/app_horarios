con <- dbConnect(duckdb::duckdb(), "data/horarios.duckdb", read_only = TRUE)

dbExecute(
  con,
  "
    CREATE TEMPORARY TABLE temp_horario AS
    SELECT row_number() OVER () AS uid, *, 1 AS cupo
    FROM tabla_horario
  "
)

server <- function(input, output, session) {
  logger$debug("Inicia la app")
  siglas <- mod_select$selectServer("codes", con, logger)
  selected_rows <- mod_table$tableServer("main_table", con, siglas)
  mod_ga$GAServer("ga", con, selected_rows)
}
