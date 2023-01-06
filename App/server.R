server <- function(input, output, session){
  session$onSessionEnded(stopApp)
}