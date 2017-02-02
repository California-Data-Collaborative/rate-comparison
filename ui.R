
# automatic reading rate codes, renaming the tabs, consistent naming convention
shinyUI(navbarPage(
  #color #2c3e50
  title = HTML(paste('<img src="CaDC_logo_bluegrey.png" ',
             'href="http://californiadatacollaborative.com/" ', 'height="60" ',
             'style="position: relative; top:-20px; right:-15px">')),
  theme = "bootstrap.css",
  windowTitle = "RateComparison",
  # tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico")),
  tabPanel(
    "Rate Comparison",
    fluidRow(
      tags$head(tags$link(rel="icon", type="image/x-icon", href="favicon.ico")),
     

      column( 12, #"main panel",
              uiOutput('classTabs')
      ) #end column
    )#end row
  ),#end tabpanel for navar
  tabPanel("Scenario Planning",
     fluidRow(
       column(2, checkboxInput("Planning", "Enable Scenario Planning", value = TRUE, 
                               width = NULL)),
       column(3, numericInput("Months", "Number of months to Forecast", 
                              3, min = 1, max = 24, step = 1,
                              width = NULL),
       column(2, downloadButton('downloadData', 'Download Data'))
       )
     ),
     fluidRow(
       column(2),
       column(2, h4("Monthly Growth/Decline\n(# of accounts)")),
       column(2, h4("Estimated Usage per\nNew Account (ccf)"))
     ),
     fluidRow(
       column(2, strong("Single-Family Residential")),
       column(2, numericInput("ResidentialSingle", NULL, 
                              5, min = -1000, max = 1000, step = 1,
                              width = NULL)),
       column(2, numericInput("EstUsagePerAccount", NULL,
                              10, min = 0, max = 1000, step = NA,
                              width = NULL))
     ),
     fluidRow(
       column(2, strong("Multi-Family Residential")),
       column(2, numericInput("ResidentialMulti", NULL, 
                              1, min = -1000, max = 1000, step = 1,
                              width = NULL)),
       column(2, numericInput("EstUsagePerAccount_multi", NULL,
                              6, min = 0, max = 1000, step = NA,
                              width = NULL))
     ),
     fluidRow(
       column(2, strong("Irrigation")),
       column(2, numericInput("Irrigation", NULL, 
                              1, min = -1000, max = 1000, step = 1,
                              width = NULL)),
       column(2, numericInput("EstUsagePerAccount_irrigation", NULL,
                              75, min = 0, max = 1000, step = NA,
                              width = NULL))
     ),
     fluidRow(
       column(2, strong("Commercial")),
       column(2, numericInput("Commercial", NULL, 
                              1, min = -1000, max = 1000, step = 1,
                              width = NULL)),
       column(2, numericInput("EstUsagePerAccount_commercial", NULL,
                              24, min = 0, max = 1000, step = NA,
                              width = NULL))
     ),
     fluidRow(
       column(2, strong("Institutional")),
       column(2, numericInput("Institutional", NULL, 
                              1, min = -1000, max = 1000, step = 1,
                              width = NULL)),
       column(2, numericInput("EstUsagePerAccount_institutional", NULL,
                              35, min = 0, max = 1000, step = NA,
                              width = NULL))
     ),
     fluidRow(
       column(2, strong("Other")),
       column(2, numericInput("Other", NULL, 
                             1, min = -1000, max = 1000, step = 1,
                             width = NULL)),
       column(2, numericInput("EstUsagePerAccount_other", NULL,
                              10, min = 0, max = 1000, step = NA,
                              width = NULL))
     )
     
  )

))
