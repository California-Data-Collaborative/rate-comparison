options(shiny.error= NULL, shiny.minified=TRUE)
# Load functions


shinyServer(function(input, output, clientData, session) {


  #******************************************************************
  # Generate a planning dataframe either adding or deleting accounts
  # and forecasting usage into the future
  #******************************************************************
  
  planneddf <- reactive({
    
    #Make sure account growth inputs are not blank
    lapply(1:length(cust_class_list), function(i) {
      req(input[[cust_class_list[i]]])
    })
    
    #If each of the account growth inputs is zero
    is_it_zero <- sapply(1:length(cust_class_list), function(i) {
       input[[cust_class_list[i]]] == 0
      
    })
    no_growth <- all(is_it_zero)

    # get value of all class growth inputs
    class_inputs <- sapply(1:length(cust_class_list), function(i) {
      input[[cust_class_list[i]]]
    })
    total_monthly_change <- sum(class_inputs)
    
    if(input$Planning == TRUE & input$Months != 0){
     
      set.seed(1234)
      
      #for mean usage_ccf
      monthlyusagebyaccount <- get_monthly_usage_by_account(df)
      
      month_Vec <- 1:input$Months
      
      if(no_growth == FALSE){
        increment_Vec <- (month_Vec)*total_monthly_change
      }
      
      recent_date <- max(df$usage_date)
      recent_month_data <- df %>% filter(usage_date == recent_date)
      recent_month_len <- nrow(recent_month_data)
      
      recent_date_Vec <- c(recent_date %m+% months(month_Vec))
      
      #for rate code filling
      ratecode_filler <- df %>% group_by(cust_class, rate_code) %>% 
        summarise(count=length(unique(cust_id))) %>% 
        arrange(desc(count))
      ratecode_filler <- data.table(ratecode_filler)
      ratecode_filler <- ratecode_filler[,head(.SD, 1), by=cust_class]
      
      #for generating customer class
      class_proportions <- as.data.frame(prop.table(table(df$cust_class)), stringsAsFactors = FALSE)
      class_proportions$Freq <- class_inputs
  
      negative_classes_df <- class_proportions[class_proportions$Freq < 0, ]
      total_account_decline <- sum(negative_classes_df$Freq)
      
      positive_classes_df <- class_proportions[class_proportions$Freq > 0, ]
      total_account_growth <- sum(positive_classes_df$Freq)
      
      extra_rows <- sum(total_account_growth+total_account_decline)

      #initialize a list for accommodating artificial data frames generated in loops at a later stage
      planneddflist <- list()
      
      if("et_amount" %in% colnames(df)){
        #average et by month
        avg_et_df <-  df%>%  group_by(usage_month) %>% summarise(et_amount = mean(et_amount,na.rm=TRUE))
      }
      
      
      if("hhsize" %in% colnames(df)){
        #average hhsize by customer class
        mean_hhsize <- df %>% 
          group_by(cust_class) %>% 
          summarise(hhsize = round(mean(hhsize,na.rm=TRUE)))
      }
      
      if("irr_area" %in% colnames(df)){
        #irrarea <- mean(df$irr_area[!df$irr_area %in% boxplot.stats(df$irr_area)$out]) #removing outliers
        #average irr_area by customer class
        mean_irr_area <- df %>% 
          group_by(cust_class) %>% 
          summarise(irr_area = round(mean(irr_area,na.rm=TRUE)))
      }
     if(total_monthly_change >= 0 & no_growth == FALSE){
      
      if(is_budget){
        #Begin generating data if budget type
        for(i in month_Vec){
          
          new_month_data <- recent_month_data
          new_month_data <- grow_or_remove_accounts(new_month_data, class_proportions, i)
         
          new_rows_idx <- (recent_month_len+(total_account_decline*i)+1):(recent_month_len+(extra_rows*i))
          
          max_cust_id <- max(new_month_data$cust_id, na.rm=TRUE)
          new_cust_ids <- (max_cust_id+1):(max_cust_id+total_account_growth*i)
          new_month_data[(recent_month_len +(total_account_decline*i)+1):(recent_month_len+(extra_rows*i)), "cust_id"] <- new_cust_ids
          
          #populate year/month/date columns
          new_month_data <- add_date_cols(new_month_data, recent_date_Vec[i])
          
          #for filling hhsize to new accounts
          tmp <- new_month_data[new_rows_idx , ]
          tmp <- left_join(tmp, mean_hhsize, by = c("cust_class")) %>% arrange(cust_id)
          new_month_data[new_rows_idx , "hhsize"] <- tmp$hhsize.y
          
          #for filling irr_area to new accounts
          tmp <- new_month_data[new_rows_idx ,]
          tmp <- left_join(tmp, mean_irr_area, by = c("cust_class")) %>% arrange(cust_id)
          new_month_data[new_rows_idx , "irr_area"] <- tmp$irr_area.y
          
          #fill in average et by month
          tmp <- left_join(new_month_data, avg_et_df, by = 'usage_month')
          new_month_data$et_amount <- tmp$et_amount.y
          
          #fill in average usage by account and month
          tmp <- left_join(new_month_data[1:recent_month_len,], monthlyusagebyaccount, by = c('cust_id','usage_month'))
          new_month_data$usage_ccf[1:recent_month_len] <- tmp$usage_ccf.y
          
          #fill in the usage for new accounts with the estimated usage input
          lapply(1:length(cust_class_list), function(i) {
            new_month_data[is.na(new_month_data$sort_index) & new_month_data$cust_class == cust_class_list[i], "usage_ccf"] <- input[[paste0("EstUsagePerAccount_",cust_class_list[i])]] 
          })
          
          #fill in meter size for new accounts
          if("meter_size" %in% colnames(df)){
            new_month_data[new_rows_idx ,"meter_size"] <- getmode(df$meter_size)
                                                                                                                            
          }
          
          #fill in water type for new accounts
          if("water_type" %in% colnames(df)){
            new_month_data[new_rows_idx ,"water_type"] <- getmode(df$water_type)
          }
          
          #fill in rate code for new accounts
          tmp <- new_month_data[new_rows_idx ,]
          tmp <- left_join(tmp, ratecode_filler, by = c("cust_class"))%>% arrange(cust_id)
          new_month_data[new_rows_idx , "rate_code"] <- tmp$rate_code.y

          planneddflist[[i]] <- new_month_data
          
        }#End generating data for budget type
        
       }else{
         #Begin generating data if any other type
         for(i in month_Vec){
           
           new_month_data <- recent_month_data
           new_month_data <- grow_or_remove_accounts(new_month_data, class_proportions, i)
           
           new_rows_idx <- (recent_month_len+(total_account_decline*i)+1):(recent_month_len+(extra_rows*i))
        
           new_month_data[new_rows_idx , "cust_id"] <- 1:(total_account_growth*i)
           new_month_data <- add_date_cols(new_month_data, recent_date_Vec[i])
           
           #fill in average usage by account and month
           tmp <- left_join(new_month_data[1:recent_month_len,], monthlyusagebyaccount, by = c('cust_id','usage_month'))
         
           new_month_data$usage_ccf[1:recent_month_len] <- tmp$usage_ccf.y
           
           #fill in the usage for new accounts with the estimated usage input
           lapply(1:length(cust_class_list), function(i) {
             new_month_data[is.na(new_month_data$sort_index) & new_month_data$cust_class == cust_class_list[i], "usage_ccf"] <- input[[paste0("EstUsagePerAccount_",cust_class_list[i])]] 
           })

           if("cust_loc_meter_size" %in% colnames(df)){
           #fill in meter size for new accounts
           new_month_data[new_rows_idx ,"meter_size"] <- getmode(df$meter_size)
           }
           if("cust_loc_water_type" %in% colnames(df)){
           #fill in water type for new accounts
           new_month_data[new_rows_idx ,"water_type"] <- getmode(df$water_type)
           
           }
           #fill in rate code for new accounts
           tmp <- new_month_data[new_rows_idx ,]
           tmp <- left_join(tmp, ratecode_filler, by = c("cust_class"))%>% arrange(cust_id)
           new_month_data[new_rows_idx , "rate_code"] <- tmp$rate_code.y
           
           planneddflist[[i]] <- new_month_data
         }
       }#End generating data if any other type
 
     }else if(no_growth == TRUE){

       if(is_budget){
         
         #Begin generating data if budget type
         for(i in month_Vec){
           
           new_month_data <- recent_month_data
           new_month_data <- add_date_cols(new_month_data, recent_date_Vec[i])
           
           #fill in average et by month
           tmp <- left_join(new_month_data, avg_et_df, by = 'usage_month')
           new_month_data$et_amount <- tmp$et_amount.y
           
           #fill in average usage by account and month
           tmp <- left_join(new_month_data, monthlyusagebyaccount, by = c('cust_id','usage_month'))
           new_month_data$usage_ccf <- tmp$usage_ccf.y
           
           planneddflist[[i]] <- new_month_data
           
         }#End generating data for budget type
       
         
       }else{
         #Begin generating data for non-budget types
         for(i in month_Vec){
           
           new_month_data <- recent_month_data
           new_month_data <- add_date_cols(new_month_data, recent_date_Vec[i])
           
           #fill in average usage by account and month
           tmp <- left_join(new_month_data, monthlyusagebyaccount, by = c('cust_id','usage_month'))
           new_month_data$usage_ccf <- tmp$usage_ccf.y
           
           planneddflist[[i]] <- new_month_data
         }
       }#End generating data if any other type
       
     }else{#If decline in estimated accounts i.e growth < 0
      #Begin degenerating data
      for(i in month_Vec){
        
        new_month_data <- recent_month_data
        new_month_data <- grow_or_remove_accounts(new_month_data, class_proportions, i)
        new_month_data <- add_date_cols(new_month_data, recent_date_Vec[i])
        
        #if budget type, then et must be filled
        if(is_budget){
          #fill in average et by month
          tmp <- left_join(new_month_data, avg_et_df, by = 'usage_month')
          new_month_data$et_amount <- tmp$et_amount.y
        }
        
        #fill in average usage by account and month
        tmp <- left_join(new_month_data, monthlyusagebyaccount, by = c('cust_id','usage_month'))
        new_month_data$usage_ccf <- tmp$usage_ccf.y
        
        planneddflist[[i]] <- new_month_data
        
      }#End degenerating data
     }
  
    planneddf = do.call(rbind, planneddflist)
    planneddf <- rbind(df, planneddf)
    planneddf$sort_index <- 1:nrow(planneddf)
    
    planneddf
  }

})  
 
 
  #******************************************************************
  # Map tier boxes to either tierBox inputs or default values
  #******************************************************************
  hypothetical_tier_boxes <- reactive({
   
     boxes <- tier_boxes
  
     for(cust_class in cust_class_list){
       class_input <- generated_inputs[[cust_class]]
       
       the_input <- class_input[["tiered_prices"]]$tier_box
       if(!is.null(the_input)){
         boxes[[cust_class]][["Tiered"]][["tier_prices"]] <- parse_numerics(strsplit(the_input, "\n")[[1]])
       }
       
       the_input <- class_input[["tiered_starts"]]$tier_box
       if(!is.null(the_input)){
         boxes[[cust_class]][["Tiered"]][["tier_starts"]] <- parse_numerics(strsplit(the_input, "\n")[[1]])
       }
       
       the_input <- class_input[["budget_prices"]]$tier_box
       if(!is.null(the_input)){
         boxes[[cust_class]][["Budget"]][["tier_prices"]] <- parse_numerics(strsplit(the_input, "\n")[[1]])
       }
       
       the_input <- class_input[["budget_starts"]]$tier_box
       if(!is.null(the_input)){
         boxes[[cust_class]][["Budget"]][["tier_starts"]] <- parse_strings(strsplit(the_input, "\n")[[1]])
       }
     }
     
     boxes
  })
  

  #******************************************************************
  # Map inputs to nested list structure needed by RateParser
  #******************************************************************
  hypothetical_rate_list <- reactive({
    ls <- baseline_rate_list
    
    
    rate_parts <- c("service_charge", "flat_rate", "gpcd", "landscape_factor","PED")
    
    for(cust_class in cust_class_list){
      
      class_input <- generated_inputs[[cust_class]]
      
      for(rate_part_name in rate_parts){
        
        the_input <- class_input[[rate_part_name]]
        is_expanded <- the_input$expanded
        if(!is.null(is_expanded)){
          
          #box is expanded?
          if(is_expanded && nchar(the_input$depend_values) > 0 && nchar(the_input$depend_charges)){
            ls$rate_structure[[cust_class]][[rate_part_name]]$depends_on <- the_input$depend_cols
            
            #convert the strings to a list
            values <- parse_strings(the_input$depend_values)
            charges <- parse_numerics(the_input$depend_charges)
            names(charges) <- values
            value_list <- as.list(charges)
            
            ls$rate_structure[[cust_class]][[rate_part_name]]$values <- value_list
            
           #box not expanded
          }else if(!is_expanded && !is.null(the_input$simpleValue)){
            ls$rate_structure[[cust_class]][[rate_part_name]] <- the_input$simpleValue
          }
          
        }
      }
     
      # link commodity_charge to hypothetical_rate_list
      if(!is.null(class_input$other_inputs$rateType)){
        if(class_input$other_inputs$rateType == "Flat" && !is.null(ls$rate_structure[[cust_class]][["flat_rate"]])){
          ls$rate_structure[[cust_class]][["commodity_charge"]] <- "flat_rate*usage_ccf"
        }else if(class_input$other_inputs$rateType %in% c("Tiered", "Budget")){
          rate_type <- class_input$other_inputs$rateType
          ls$rate_structure[[cust_class]][["commodity_charge"]] <- rate_type
          ls$rate_structure[[cust_class]][["tier_starts"]] <- hypothetical_tier_boxes()[[cust_class]][[rate_type]]$tier_starts
          ls$rate_structure[[cust_class]][["tier_prices"]] <- hypothetical_tier_boxes()[[cust_class]][[rate_type]]$tier_prices
          
          if(class_input$other_inputs$rateType == "Budget" && is.null(ls$rate_structure[[cust_class]]$budget)){
            ls$rate_structure[[cust_class]][["budget"]] <- hypothetical_tier_boxes()[[cust_class]][[rate_type]]$budget
          }
        }
      }
      
    }
  
    ls
  })
  
  #******************************************************************
  # Calculate total bill
  #******************************************************************
  total_bill_info <- reactive({
 
    bill_info <- RateParser::calculate_bill(DF(), hypothetical_rate_list())
    
    bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
   
    bill_info <- bill_info %>% dplyr::rename(variable_bill=commodity_charge,
                                             total_bill=bill)
    
    #in this case hypothetical same as baseline usage
    # bill_info$hypothetical_usage <- bill_info$usage_ccf
    # bill_info$forecast_usage <- bill_info$usage_ped
  
    
    # select and return only relevent columns
    mask <- grepl("XR?[0-9].*|variable.*|total.*|hypothetical.*", names(bill_info))
    bill_info <- bill_info[mask]
    
    # This should work but weird bug causes "cust_class" to get matched also
    #bill_info <- bill_info %>% select(matches("BR?[0-9]|baseline.*"))
    
    return(bill_info)
  })
  
  forecast_bill_info <- reactive({
    tmp <- df_forecast()
    usage_col_mask <- !grepl("XR?[0-9]", names(tmp))
    tmp <- tmp[,usage_col_mask]
    bill_info <- RateParser::calculate_bill(tmp, hypothetical_rate_list())
    bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
    bill_info <- bill_info %>% dplyr::rename(variable_ped_bill =commodity_charge,
                                             hypothetical_ped_bill=bill)
    usage_col_mask <- grepl("X[0-9]", names(bill_info))
    revenue_col_mask <- grepl("XR[0-9]", names(bill_info))
    num_tiers <- sum(usage_col_mask)
    colnames(bill_info)[usage_col_mask] <- c( paste("T", 1:num_tiers, sep=""))
    colnames(bill_info)[revenue_col_mask] <- c( paste("TR", 1:num_tiers, sep=""))
    
    # PED has already ben factored in at this point
    bill_info$hypothetical_usage <- bill_info$usage_ccf
    
    # select and return only relevent columns
    mask <- grepl("TR?[0-9].*|commodity.*|hypothetical.*|ped_change_in_usage|variable_ped_bill", names(bill_info))
    bill_info <- bill_info[mask]
    
    # This should work but weird bug causes "cust_class" to get matched also
    #bill_info <- bill_info %>% select(matches("BR?[0-9]|baseline.*"))
    
    return(bill_info)
  })
  
  
  # Return the proper dataframe given planning status
  DF <- reactive({
    req(input$Months)
    if(input$Planning & input$Months != 0){
      planneddf()
    }
    else if(input$Planning & input$Months == 0){
      df
    }
    else{
      df
    }
  })
  
  df_download <- reactive({
    combined <- dplyr::bind_cols(DF(), total_bill_info(),baseline_bill_info()) 
    
    combined
  })
  
  df_forecast <- reactive({
    forecasted <- dplyr::bind_cols(DF(), total_bill_info(),baseline_bill_info()) 
    forecasted <- forecasted  %>%
      mutate(ped_change_in_usage = input$PED*((variable_bill-baseline_variable_bill)*usage_ccf)/baseline_variable_bill)
    forecasted$usage_ccf <- forecasted$usage_ccf - forecasted$ped_change_in_usage
    forecasted
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("TierUse_BillReport", '.csv', sep='') },
    content = function(file) {
      write.csv(df_download(),file)
    }
  )
  # Generate output panels for each customer class in the data
  output$classTabs <- renderUI({
    myTabs = lapply(1:length(cust_class_list), function(i){
      tabPanel(cust_class_list[i],
               classGraphOutput(paste0("panel_",cust_class_list[i]), rate_code_dict[[cust_class_list[i]]]),
               value=cust_class_list[i]
      )
    })
    do.call(tabsetPanel, c(myTabs, list(id="classTabs")) )
  })
  
  active_tab <- reactive({
    input$classTabs
  })
  
  # generated_inputs <- list()
  # callModule to connect server code with each of the customer class panels
  for(c in cust_class_list){
    # class_rate <- baseline_rate_list$rate_structure[[c]]
    generated_inputs[[c]] <- callModule(classGraph, paste0("panel_",c), c, DF, total_bill_info, baseline_bill_info, 
                                        forecast_bill_info,active_tab, hypothetical_rate_list, 
                                        has_planning=input$Planning)
  }
  
  
  

  
  #******************************************************************
  # Calculate bills and tiers for the baseline rate
  #******************************************************************
  baseline_bill_info <- reactive({
    baseline(basedata=DF())
  })
  
})



baseline <- function(basedata){
  
  bill_info <- RateParser::calculate_bill(basedata, baseline_rate_list)
  bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
  
  #mask for columns representing tier usage
  usage_col_mask <- grepl("X[0-9]", names(bill_info))
  revenue_col_mask <- grepl("XR[0-9]", names(bill_info))
  num_tiers <- sum(usage_col_mask)
  colnames(bill_info)[usage_col_mask] <- c( paste("B", 1:num_tiers, sep=""))
  colnames(bill_info)[revenue_col_mask] <- c( paste("BR", 1:num_tiers, sep=""))
 
  bill_info <- bill_info %>% dplyr::rename(baseline_variable_bill=commodity_charge,
                                           baseline_bill=bill)
  #adding baseline usage
  bill_info$baseline_usage <- bill_info$usage_ccf
  
  # select and return only relevent baseline columns
  baseline_mask <- grepl("BR?[0-9]|baseline.*", names(bill_info))
  bill_info <- bill_info[baseline_mask]
  
  # This should work but weird bug causes "cust_class" to get matched also
  #bill_info <- bill_info %>% select(matches("BR?[0-9]|baseline.*"))
  
  return(bill_info)
}








lvmwd_baseline <- function(basedata){
  
  #2014 rate
  tmp <- filter(basedata, usage_year < 2015)
  
  bill_2014 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Tiered", 
                                       tier_starts=parse_numerics("0\n16\n67\n200"),
                                       tier_prices=parse_numerics("2.19\n2.60\n3.56\n5.02") )
  


  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2014) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2014 <- bill_2014 %>% mutate(baseline_bill=baseline_variable_bill + 30.21)
  #adding baseline usage
  bill_2014$baseline_usage <- bill_2014 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  
  
  #2015 before the September switch to monthly billing
  tmp <- filter(basedata, usage_year >= 2015, usage_year < 2016, usage_month < 9)
  
  bill_2015_1 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n16\n67\n200"),
                                         tier_prices=parse_numerics("2.31\n2.80\n3.81\n5.34") )
  
  
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_1) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_1 <- bill_2015_1 %>% mutate(baseline_bill=baseline_variable_bill + 31.73)
  #adding baseline usage
  bill_2015_1$baseline_usage <- bill_2015_1 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #2015 after the September switch to monthly billing
  tmp <- filter(basedata, usage_date >= as.Date("2015-09-01"), usage_year < 2016)
  
  bill_2015_2 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n8\n34\n100"),
                                         tier_prices=parse_numerics("2.31\n2.80\n3.81\n5.34") )

  
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_2) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_2$baseline_bill <- bill_2015_2$baseline_variable_bill + 15.87
  #adding baseline usage
  bill_2015_2$baseline_usage <- bill_2015_2 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #2016 budgets
  tmp <- filter(basedata, usage_year >= 2016)
  
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\nIndoor\n101%\n151%"), 
                                  get_indoor_tier(tmp, 55), get_outdoor_tier(tmp, 0.8))
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("2.36\n3.18\n3.96\n4.98") )
  num_tiers <- length(parse_strings("2.36\n3.18\n3.96\n4.98"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 18.30
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  return( bind_rows(bill_2014, bill_2015_1, bill_2015_2, bill_2016) )
}

smwd_baseline <- function(basedata){
  
  
  #before March 2015
  tmp <- filter(basedata, usage_date < as.Date("2015-03-01"))
  
  bill_2015_1 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n7\n21\n36\n71"),
                                         tier_prices=parse_numerics("2.23\n2.46\n2.94\n3.45\n4.33") )
  num_tiers <- length(parse_strings("0\n7\n21\n36\n71"))
  colnames(bill_2015_1) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_1 <- bill_2015_1 %>% mutate(baseline_bill=baseline_variable_bill + 6.41)
  #adding baseline usage
  bill_2015_1$baseline_usage <- bill_2015_1 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  
  #after March 2015
  tmp <- filter(planneddf(), usage_date >= as.Date("2015-03-01"), usage_year < 2016)
  
  bill_2015_2 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n7\n21\n36\n71"),
                                         tier_prices=parse_numerics("2.04\n2.29\n2.77\n3.28\n4.50") )
  num_tiers <- length(parse_strings("0\n7\n21\n36\n71"))
  colnames(bill_2015_2) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_2$baseline_bill <- bill_2015_2$baseline_variable_bill + 8.72
  #adding baseline usage
  bill_2015_2$baseline_usage <- bill_2015_2 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #   #2016 budgets
  #   tmp <- filter(df, usage_year >= 2016)
  #   tier_starts <- get_budget_tiers(tmp, parse_strings("0\nIndoor\n101%\n151%"), 
  #                                   get_indoor_tier(tmp, 55), get_outdoor_tier(tmp, 0.8))
  #   bill_2016 <- calculate_variable_bill(data=tmp, 
  #                                        rate_type="Budget", 
  #                                        tier_starts=tier_starts,
  #                                        tier_prices=parse_numerics("2.36\n3.18\n3.96\n4.98") )
  #   num_tiers <- length(parse_strings("2.36\n3.18\n3.96\n4.98"))
  #   colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
  #                             paste("BR", 1:num_tiers, sep=""),
  #                             "baseline_variable_bill")
  #   bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 14.89
  
  
  return( bind_rows(bill_2015_1, bill_2015_2) )
}


smc_baseline <- function(basedata){
  
  #before 2016
  tmp <- filter(basedata, usage_date < as.Date("2016-01-01"))
  
  bill_2015 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n15\n41\n149"),
                                         tier_prices=parse_numerics("2.73\n4.09\n6.13\n9.59") )
  num_tiers <- length(parse_strings("0\n15\n41\n149"))
  colnames(bill_2015) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015 <- bill_2015 %>% mutate(baseline_bill=baseline_variable_bill + 0)
  #adding baseline usage
  bill_2015$baseline_usage <- bill_2015 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #after 2016
  tmp <- filter(basedata, usage_date >= as.Date("2016-01-01"))
  
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n15\n41\n149"),
                                         tier_prices=parse_numerics("2.87\n4.29\n6.44\n10.07") )
  
  num_tiers <- length(parse_strings("0\n15\n41\n149"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  
  bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 0
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  return( bind_rows(bill_2015, bill_2016) )
}


irwd_baseline <- function(basedata){
  
  #FY2014  
  tmp <- filter(basedata, usage_date < as.Date("2014-07-01"))
  
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n151%\n201%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2014 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("0.91\n1.27\n2.86\n4.80\n9.84") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n151%\n201%"))
  colnames(bill_2014) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2014 <- bill_2014 %>% mutate(baseline_bill=baseline_variable_bill + 9.85)
  #adding baseline usage
  bill_2014$baseline_usage <- bill_2014 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #FY2015
  
  tmp <- filter(basedata, usage_date < as.Date("2015-07-01"), usage_date >= as.Date("2014-07-01"))
  
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n131%\n161%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2015 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("0.88\n1.34\n3.91\n6.22\n12.60") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n151%\n201%"))
  colnames(bill_2015) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2015 <- bill_2015 %>% mutate(baseline_bill=baseline_variable_bill + 10.50)
  #adding baseline usage
  bill_2015$baseline_usage <- bill_2015 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #FY2016
  tmp <- filter(basedata, usage_date < as.Date("2016-07-01"), usage_date >= as.Date("2015-07-01"))
  
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n131%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("1.11\n1.62\n3.92\n14.53") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n131%"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2016 <- bill_2016 %>% mutate(baseline_bill=baseline_variable_bill + 10.30)
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  return( bind_rows(bill_2014, bill_2015, bill_2016) )
}
#test_dir("C:/Users/anude/Documents/GitHub/RateComparison/tests/testthat", reporter="summary")

