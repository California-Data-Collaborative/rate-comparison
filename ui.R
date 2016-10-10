
# automatic reading rate codes, renaming the tabs, consistent naming convention
shinyUI(navbarPage(
  #color #2c3e50
  HTML(paste('<img src="CaDC_logo_bluegrey.png" ',
             'href="http://californiadatacollaborative.com/" ', 'height="60" ',
             'style="position: relative; top:-20px; right:-15px">')),
  theme = "bootstrap.css",
  
  tabPanel(
    "Rate Comparison",
    fluidRow(
      #----------------------- Input widget panel -----------------------    
      column(4,
             wellPanel( 
               fluidRow(
                 column(5, 
                        radioButtons("rateType", label = "Rate Type",
                                     choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                     selected = "Flat")
                        
                        
                        
                 ),
                 column(7, 
                        numericInput("fixedCharge", label = "Fixed Charge ($)", value = default_fixed_charge),
                        radioButtons("displayType", label = "Display", selected = "Revenue", inline=TRUE,
                                     choices = list("Revenue" = "Revenue", "Usage" = "Usage"))
                        
                        
                 )
               ),#end row
               
               fluidRow(
                 column(1),
                 column(10,
                        sliderInput("timeSlider", label = "Time Range", min = min_date, 
                                    max = max_date, value = c(min_date, max_date), timeFormat="%Y-%m")
                 ),
                 column(1)
               ),
               
               
               # FLATE RATES
               conditionalPanel(
                 condition = "input.rateType == 'Flat'",
                 fluidRow(
                   column(12,
                          numericInput("flatRate", label = "Price per CCF ($)", value = 2.7)
                   )
                 )#end row
               ),#end conditionalPanel
               
               # TIERED RATES
               conditionalPanel(
                 condition = "input.rateType == 'Tiered'",
                 fluidRow(
                   column(6,
                          fluidRow(
                            strong("Tier start (CCF)")
                          ),
                          fluidRow(
                            HTML(default_tiered_tiers_html)
                          )
                   ),
                   column(6,
                          fluidRow(
                            strong("Tier prices")
                          ),
                          fluidRow(
                            HTML(default_tiered_prices_html)
                          )
                   )
                 )#end row
               ),#end conditionalPanel
               
               # BUDGET BASED
               conditionalPanel(
                 condition = "input.rateType == 'Budget'",
                 fluidRow(
                   column(6, 
                          sliderInput("galPerCapitaSlider", label = "GPCD", min = 25, 
                                      max = 75, value = default_gpcd, step=5)
                   ),
                   column(6, 
                          sliderInput("ETFactorSlider", label = "ET Factor", min = 0, 
                                      max = 1, value = default_et_factor, step=0.05)
                   )
                 ),#end row
                 fluidRow(
                   column(6,
                          fluidRow(
                            strong("Tier start")
                          ),
                          fluidRow(
                            HTML(default_budget_tiers_html)
                          )
                   ),
                   column(6,
                          fluidRow(
                            strong("Tier prices ($)")
                          ),
                          fluidRow(
                            HTML(default_budget_prices_html)
                          )
                   )
                 ),#end row
                 fluidRow(paste("Enter the starting value for each tier either as a CCF value, ",
                                " or as a percent of budget (water budget assumed as Indoor + Outdoor). ",
                                "Where:")
                 ),
                 fluidRow(br(),
                          em("Indoor = GPCD * HHSize * (365/12/748)"),
                          br(),
                          em("Outdoor = ET_Factor * ET * LA  * (0.62/748)")
                 )
                 
               )#end conditionalPanel
               
               #                conditionalPanel(
               #                  condition = "input.rateType == 'Tiered' || input.rateType == 'Budget'",
               #                  fluidRow(
               #                    column(12,
               #                           actionButton("updateTiers", "Update Tiers")
               #                    )
               #                  )#end row
               #                )
               
             )#end wellPanel
      ),#end column
      
      #----------------------- Output panels ------------------
      column( 8, #"main panel",
              tabsetPanel(type="tabs",
                          tabPanel("Residential_Single",
                                   checkboxGroupInput("RateCode", label = "Rate Code",inline=TRUE,
                                                      choices = variables, selected = "R1"),
                                   
                                   fluidRow(
                                     column(12, plotlyOutput("revenue_time_series", height=250) )
                                   ),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot", height=100),
                                            plotlyOutput("bill_change_histogram", height=250) )
                                     
                                   ),
                                   
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }" )
                          ),
                          tabPanel("Residential_Multi",
                                   checkboxGroupInput("RateCode1", label = "Rate Code",inline=TRUE,
                                                      choices = variables1, selected = "A1"),
                                   fluidRow( column(12, plotlyOutput("revenue_time_series1", height=250) )),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers1", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart1", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot1", height=100),
                                            plotlyOutput("bill_change_histogram1", height=250) )
                                     
                                   )
                          ),
                          
                          tabPanel("Commercial",
                                   checkboxGroupInput("RateCode2", label = "Rate Code",inline=TRUE,
                                                      choices = variables2,selected = "CM4" ),
                                   fluidRow( column(12, plotlyOutput("revenue_time_series2", height=250) )),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers2", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart2", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot2", height=100),
                                            plotlyOutput("bill_change_histogram2", height=250) )
                                     
                                   )
                          ) ,
                          
                          tabPanel("Irrigation",
                                   checkboxGroupInput("RateCode3", label = "Rate Code",inline=TRUE,
                                                      choices = variables3,selected = "RC1" ),
                                   fluidRow( column(12, plotlyOutput("revenue_time_series3", height=250) )),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers3", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart3", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot3", height=100),
                                            plotlyOutput("bill_change_histogram3", height=250) )
                                     
                                   )
                          ), 
                          tabPanel("Institutional",
                                   checkboxGroupInput("RateCode4", label = "Rate Code",inline=TRUE,
                                                      choices = variables4,selected = "RC2" ),
                                   fluidRow( column(12, plotlyOutput("revenue_time_series4", height=250) )),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers4", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart4", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot4", height=100),
                                            plotlyOutput("bill_change_histogram4", height=250) )
                                     
                                   )
                          ),
                          
                          tabPanel("Other",
                                   checkboxGroupInput("RateCode5", label = "Rate Code",inline=TRUE,
                                                      choices = variables5,selected = "FP" ),
                                   fluidRow( column(12, plotlyOutput("revenue_time_series5", height=250) )),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers5", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart5", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot5", height=100),
                                            plotlyOutput("bill_change_histogram5", height=250) )
                                     
                                   )
                          )
                          
              )#end tabsetpanel
      ) #end column
    )#end row
  )#end tabpanel for navar
))
