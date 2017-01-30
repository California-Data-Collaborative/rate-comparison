#****************************************************************************************** 
#class_graphs.R
#
# This file defines a module called classGraph that corresponds to a self-contained panel
# for a single customer class. This panel is self contained in the sense that it contains
# a set of inputs and graphical outputs for the specified customer class. These inputs are
# independent from the inputs of any other class, allowing for changes to be made to one
# customer class without affecting any others.
#
# most of the code needed to define the inputs on the left side, connect the inputs to the data,
# and connect the data to the graphs should will probably be located here.
#
#******************************************************************************************

classGraphOutput <- function(id, rate_codes){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, 
         wellPanel(
#            tabsetPanel(type = 'tabs', 
#                        tabPanel("Current Rate Comparision",
                                
              fluidRow(
                column(8, 
                       radioButtons(ns("rateType"), label = "Rate Type", inline=TRUE,
                                    choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                    selected = "Budget")
                ),
                column(4, 
                       radioButtons(ns("displayType"), label = "Display", selected = "Revenue", inline=FALSE,
                                    choices = list("Revenue" = "Revenue", "Usage" = "Usage"))
                       
                      
                )
                
              ),#end row
              
              fluidRow(
                column(1),
                column(10,
                       sliderInput(ns("timeSlider"), label = "Time Range", min = min_date, 
                                   max = max_date, value = c(min_date, max_date), timeFormat="%Y-%m")
                      
                      
                ),
                column(1)
              ),
              
              
              fluidRow(
                column(12,
                       ratePartInput(ns("service_charge"))
                )
              ),

              
              # FLAT RATES
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Flat'", ns("rateType")),
                fluidRow(
                  column(12,
                         ratePartInput(ns("flat_rate"))
                  )
                )#end row
              ),#end conditionalPanel
              
              # TIERED RATES
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Tiered'", ns("rateType")),
                fluidRow(
                  column(6,
                         fluidRow(
                           tierBoxInput(ns("tiered_starts"))
                         )
                  ),
                  column(6,
                         fluidRow(
                           tierBoxInput(ns("tiered_prices"))
                         )
                  )
                )#end row
              ),#end conditionalPanel
              
              # BUDGET BASED
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Budget'", ns("rateType")),
                fluidRow(
                  column(12,
                         ratePartInput(ns("gpcd"))
                  )
                ),
                
                fluidRow(
                  column(12,
                         ratePartInput(ns("landscape_factor"))
                  )
                ),
                fluidRow(
                  column(6,
                         fluidRow(
                           tierBoxInput(ns("budget_starts"))
                         )
                  ),
                  column(6,
                         fluidRow(
                           tierBoxInput(ns("budget_prices"))
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
              )
           
         )#end wellPanel
      ),# end column
      column(8,
             checkboxGroupInput(ns("RateCode"), label = "Rate Code", inline=TRUE,
                                choices = rate_codes, selected = rate_codes[1]),
             
             fluidRow(
               column(12, plotlyOutput(ns("revenue_time_series"), height=250) )
             ),
             fluidRow(
               column(4,
                      plotlyOutput(ns("barchart_by_tiers"), height=350),
                      radioButtons(ns("barType"), label = "",  selected = "Absolute", inline=TRUE,
                                   choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
               ),
               column(3,
                      plotlyOutput(ns("fixed_revenue_barchart"), height=350)
               ),
               column(5, 
                      plotlyOutput(ns("bill_change_boxplot"), height=100),
                      plotlyOutput(ns("bill_change_histogram"), height=250) )
               
             ),
             
             tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }" )
      ) # end column
    )# end row
  )#end tagList
}


classGraph <- function(input, output, session, cust_class, df_original, df_total_bill, 
                       df_baseline_bill, active_class, rate_list, has_planning=FALSE){
  ns <- session$ns
  
  input_list <- list()
  input_list$other_inputs <- input
  
  rate_part_name <- "service_charge"
  input_list[[rate_part_name]] <- callModule(ratePart, rate_part_name, 
             part_name=rate_part_name, part_name_long="Service Charge",
             depends_col_list=dropdown_cols,
             rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name]])

  rate_part_name2 <- "flat_rate"
  input_list[[rate_part_name2]] <- callModule(ratePart, rate_part_name2, 
             part_name=rate_part_name2, part_name_long="Charge per CCF",
             depends_col_list=dropdown_cols, 
             value_map = rate_list()$rate_structure[[active_class()]][[rate_part_name2]]$values)   
  
  rate_part_name3 <- "gpcd"
  input_list[[rate_part_name3]] <- callModule(ratePart, rate_part_name3, 
            part_name=rate_part_name3, part_name_long="GPCD",
            depends_col_list=dropdown_cols, 
            rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name3]]
            )   

  rate_part_name4 <- "landscape_factor"
  input_list[[rate_part_name4]] <- callModule(ratePart, rate_part_name4, 
            part_name=rate_part_name4, part_name_long="Landscape Factor",
            depends_col_list=dropdown_cols, 
            rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name4]]
  )  
  
  rate_part_name5 <- "tiered_prices"
  input_list[[rate_part_name5]] <- callModule(tierBox, rate_part_name5, 
            part_name=rate_part_name5, part_name_long="Tier Prices ($)", 
            rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
            rate_type_provided="Tiered", 
            rate_part=rate_list()$rate_structure[[active_class()]][["tier_prices"]]
  )
  
  rate_part_name6 <- "budget_prices"
  input_list[[rate_part_name6]] <- callModule(tierBox, rate_part_name6, 
            part_name=rate_part_name6, part_name_long="Tier Prices ($)", 
            rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
            rate_type_provided="Budget", 
            rate_part=rate_list()$rate_structure[[active_class()]][["tier_prices"]]
  )
  
  rate_part_name7 <- "tiered_starts"
  input_list[[rate_part_name7]] <- callModule(tierBox, rate_part_name7, 
                                              part_name=rate_part_name7, part_name_long="Tier Starts (CCF)", 
                                              rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
                                              rate_type_provided="Tiered", 
                                              rate_part=rate_list()$rate_structure[[active_class()]][["tier_starts"]]
  )
  
  rate_part_name8 <- "budget_starts"
  input_list[[rate_part_name8]] <- callModule(tierBox, rate_part_name8, 
                                              part_name=rate_part_name8, part_name_long="Tier Starts (CCF)", 
                                              rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
                                              rate_type_provided="Budget", 
                                              rate_part=rate_list()$rate_structure[[active_class()]][["tier_starts"]]
  )
  
  observe({
    
    updateSliderInput(session, "timeSlider", label = "Time Range", 
                      min = min(df_original()$usage_date), 
                      max = max(df_original()$usage_date), 
                      value = c(min(df_original()$usage_date), max(df_original()$usage_date)))
  })
  

  df_plots <- reactive({

    combined <- dplyr::bind_cols(df_original(), df_total_bill(), df_baseline_bill()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode)
   
    combined
  })
  
  #******************************************************************
  # Line plot of revenue over time
  #******************************************************************
  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(has_planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
           geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
           geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  #******************************************************************
  # Bar chart of revenue/usage by tier
  #******************************************************************
  output$barchart_by_tiers <- renderPlotly({
    plot_barchart_by_tiers( df_plots(), input$displayType, input$barType )
  })
  
 
  
  #******************************************************************
  # Reactive dataframe of changes to amount paid
  #******************************************************************
  df_change <- reactive({
    start.time <- Sys.time()
    df_change <- df_plots() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage)
    
    if(input$barType == "Absolute"){
      #calucating differences in usage
      df_change <- df_change %>% 
          mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    }else{
      #calucating percent differences in usage
      df_change <- df_change %>% 
        mutate(changes=((total_bill-baseline_bill)/baseline_bill)*100, changes_in_usage=((hypothetical_usage-baseline_usage)/baseline_usage)*100, change_group=1)
    }
    
    if (input$displayType == "Revenue"){
      df_change <- df_change %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    }
    else{
      df_change <- df_change %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                          2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change
  })
  
  # Plot histogram
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change(), input$displayType, input$barType )
  })
  
  # Plot boxplot
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change(), input$displayType, input$barType )
  })
  
  output$fixed_revenue_barchart <- renderPlotly({
    plot_fixed_revenue(df_plots(), input$barType)
  })
  
  return(input_list)
}#End of classGraph Function




get_rate_part_value <- function(class_rate, rate_part_name){
  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)
    
    if(name==rate_part_name){
      
      if(is_map(rate_part[[name]])){
        return(rate_part[[name]]$value)
      }else{
        return(rate_part[[name]])
      }
    }
  }
  return(0)
}

get_rate_part_depends_col <- function(class_rate, rate_part_name){
  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)
    
    if(name==rate_part_name){
      if(is_map(rate_part[[name]])){
        return(rate_part[[name]]$depends_on)
      }
    }
  }
  return(NULL)
}
