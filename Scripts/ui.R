library(shiny)
library(DT)


  ui <- fluidPage(
    shiny::titlePanel("Mapping responses to metadata"),
    shiny::fluidRow(
      shiny::column(4, shiny::tableOutput("current_response")),
      #shiny::column(2, shiny::numericInput("cue_response_id", label = "Cue-Response ID", value = NA, min = 1, max = 11195), offset = 1)
    ),
    shiny::textInput("search_pattern", "Search pattern"),
    DT::dataTableOutput("search_results"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectizeInput("kuperman_map", label = "Kuperman", choices = NULL)),
      shiny::column(4, shiny::selectizeInput("subtlex_map", label = "SUBTLEX", choices = NULL)),
      shiny::column(4, shiny::selectizeInput("cue_map", label = "Word Association Cue", choices = NULL))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::textInput("revision", "Revision")),
      shiny::column(4, shiny::selectizeInput(
        inputId = "researcher_id",
        label = "Researcher",
        choices = NULL
      )),
      shiny::column(4,
        shiny::actionButton("commit", "Commit", class = "btn-primary"),
        shiny::actionButton("reset", "Reset", class = "btn-warning"),
        shiny::actionButton("quit", "Quit", class = "btn-danger")
      )
    ),
    shiny::wellPanel(
      h4("Spelling Suggestions"),
      textOutput("spelling_suggestions"),
      h4("Stem suggestions"),
      textOutput("stem_suggestions")
    )
  )
