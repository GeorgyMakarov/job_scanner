ui <- dashboardPage(
  
  dashboardHeader(title = 'Job Scanner'),
  dashboardSidebar(
    fileInput(
      inputId = 'job',
      label   = 'Load job description',
      accept  = '.txt'
    ),
    fileInput(
      inputId = 'resume',
      label   = 'Load resume',
      accept  = '.txt'
    ),
    sliderInput(
      inputId = 'n_grams',
      label   = 'Select N grams',
      min     = 1,
      max     = 4,
      value   = 1
    ),
    actionButton(
      inputId = 'go',
      label   = 'Start',
      icon    = icon('play')
    ),
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard'),
      menuItem('Raw Data',  tabName = 'raw_data')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        'dashboard',
        fluidRow(
          valueBoxOutput('intersection'),
          valueBoxOutput('readability'),
          valueBoxOutput('overall')
        ),
        fluidRow(
          box(
            width = 8,
            status = 'info',
            solidHeader = T,
            title       = 'Job Description Word Cloud',
            plotOutput('word_cloud', width = '100%', height = 600)
          ),
          box(
            width = 4,
            height = "100%",
            status = 'info',
            title  = 'Top mismatches',
            tableOutput('table_out')
          )
        )
      ),
      tabItem(
        'raw_data',
        numericInput(
          inputId = 'maxrows',
          label   = 'Show rows',
          min     = 0,
          max     = 100,
          value   = 25,
          step    = 1
        ),
        verbatimTextOutput('raw_table'),
        downloadButton(
          outputId = 'dnld_csv',
          label    = 'Download',
          icon     = icon('file-download')
        )
      )
    )
  )
  
)