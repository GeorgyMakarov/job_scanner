server <- function(input, output, session){
  
  r <- reactiveValues(
    input_tokens   = list(),
    stemmed_tokens = list(),
    freq_tables    = list(),
    merged_dt      = data.table::data.table(),
    raw_dt         = data.table::data.table(),
    final_dt       = data.table::data.table(),
    intersect_rate = c(),
    readable       = c(),
    overall        = c()
  )
  
  observeEvent(input$go, {
    req(input$go > 0)
    
    descr <- list(input$job, input$resume)
    preps <- './Maps/prepositions.txt'
    
    input_tokens <- lapply(
      descr,
      function(i){build_tokens(my_readtext(i$datapath, preps), input$n_grams)}
    )
    
    stemmed_tokens <- lapply(
      input_tokens,
      function(i){quanteda::tokens_wordstem(
        i, language = quanteda::quanteda_options("language_stemmer"))}
    )
    
    names(stemmed_tokens) = names(input_tokens) <- c('job', 'resume')
    
    r$input_tokens   <- input_tokens
    r$stemmed_tokens <- stemmed_tokens
  })
  
  observeEvent(r$stemmed_tokens, {
    req(length(r$stemmed_tokens) > 0)
    freq_tables <- lapply(
      r$stemmed_tokens,
      function(i){
        convert_to_dt(
          data.table::as.data.table(
            data.frame(
              as.list(
                make_freq(i, 1, 1)
              )
            )
          )
        )
      }
    )
    r$freq_tables <- freq_tables
  })
  
  output$word_cloud <- renderPlot({
    req(length(r$input_tokens) > 0)
    return(make_wd_plot(r$input_tokens$job))
  })
  
  observeEvent(r$freq_tables, {
    req(length(r$freq_tables) > 0)
    
    merged_dt <- 
      merge(x     = r$freq_tables$job, 
            y     = r$freq_tables$resume, 
            by    = 'words', 
            all.x = T) %>% 
      data.table::as.data.table(.) %>% 
      data.table::setnames(., 
                           old = colnames(.), 
                           new = c('words', 'job', 'resume')) %>% 
      data.table::setnafill(., cols = c('job', 'resume'), fill = 0)
    
    stop_words <- 
      tidytext::stop_words %>% 
      select(word)         %>% 
      unique(.)            %>% 
      pull(word)           %>% 
      build_tokens(.)      %>% 
      quanteda::tokens_wordstem(.) %>% 
      as.list(.) %>% 
      data.frame(.) %>% 
      data.table::as.data.table(.) %>% 
      convert_to_dt(.) %>% 
      pull(value) %>% 
      unique(.)
    
    merged_dt <- merged_dt[!(words %in% stop_words)]
    r$raw_dt  <- merged_dt
    
    merged_dt$job[merged_dt$job > 0]       <- 1
    merged_dt$resume[merged_dt$resume > 0] <- 1
    
    r$final_dt       <- merged_dt
    r$intersect_rate <- round(sum(merged_dt$resume) / sum(merged_dt$job), 2)
    r$readable <- round(nrow(r$freq_tables$job) / nrow(r$freq_tables$resume), 2)
    r$overall  <- round(mean(c(r$intersect_rate, r$readable)), 2)
  })
  
  output$intersection <- renderValueBox({
    req(r$intersect_rate > 0)
    valueBox(
      value    = r$intersect_rate,
      subtitle = 'Intersection rate',
      icon     = icon('chart-bar')
    )
  })
  
  output$readability <- renderValueBox({
    req(r$readable > 0)
    valueBox(
      value    = r$readable,
      subtitle = 'Readability',
      icon     = icon('book-open')
    )
  })
  
  output$overall <- renderValueBox({
    req(r$overall > 0)
    valueBox(
      value    = r$overall,
      subtitle = 'Overall rating',
      icon     = icon('star')
    )
  })
  
  output$table_out <- renderTable({
    req(nrow(r$final_dt) > 0)
    return(
      r$final_dt[resume == 0, ] %>% head(., 15)
    )
  })
  
  output$raw_table <- renderPrint({
    orig <- options(width = 1000)
    print(head(r$raw_dt, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  session$onSessionEnded(stopApp)
}