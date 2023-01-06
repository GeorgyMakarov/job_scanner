ui <- fluidPage(
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  shinyWidgets::useSweetAlert(),
  
  titlePanel(
    tags$div(
      span(tags$em(" Job"), style = "color:#03203C"),
      span("Scanner", style = "color:#207398")
    )
  ),
  
  fluidRow(
    column()
  ),
  
  fluidRow(
    column(),
    column()
  )
)