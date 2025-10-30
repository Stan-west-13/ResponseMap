read_all_tables <- function(db_files) {
  
  db_connections <- map(db_files, ~ dbConnect(RSQLite::SQLite(), .x))
  on.exit(walk(db_connections, dbDisconnect))
  
  list(
    cues = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM cues') |>
          as_tibble() |>
          mutate(cue = trimws(cue))
    }),
    responses = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM responses') |>
          as_tibble() |>
          mutate(response = trimws(response))
    }),
    cues_responses = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM cues_responses') |>
          as_tibble()
    }),
    response_map = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM response_map') |>
          as_tibble() |>
          mutate(
            revision = trimws(revision),
            cue_response_id = as.integer(cue_response_id),
            timestamp = suppressWarnings(if_else(
              str_detect(timestamp, "\\."),
              as.POSIXct(as.numeric(timestamp)),
              strptime(timestamp, "%Y-%m-%d %H:%M:%S")
            ))
          )
    }),
    kuperman = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM kuperman') |>
          as_tibble()
    }),
    subtlex = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM subtlex') |>
          as_tibble()
    }),
    words_meta = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM words_meta') |>
          as_tibble()
    }),
    decisions = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM decisions') |>
          as_tibble()
    }),
    subject_decisions = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM subject_decisions') |>
          as_tibble()
    }),
    subjects = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM subjects') |>
          as_tibble()
    }),
    researchers = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM researchers') |>
          as_tibble()
    }),
    response_behaviors = map(db_connections, \(db) {
        dbGetQuery(db, 'SELECT * FROM response_behaviors') |>
          as_tibble()
    })
  )
}