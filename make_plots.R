library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(reshape2)
library(stringi)

#******************************************************************
# Line chart showing revenue over time for both baseline
# and hypothetical rate structures
#******************************************************************
plot_revenue_over_time <- function(data){
  start.time <- Sys.time()

  monthly_revenue <- data %>%  group_by(usage_date) %>% 
                      summarise(revenue=sum(total_bill, na.rm=TRUE),
                                baseline_revenue=sum(baseline_bill, na.rm=TRUE)) %>% 
                      mutate(Baseline = baseline_revenue/1000000) %>%
                      mutate(Hypothetical = revenue/1000000) %>%
                      select(usage_date,Baseline,Hypothetical)
  monthly_revenue <- melt(monthly_revenue, id="usage_date") %>% rename(Revenue=variable)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("Calcing monthly_revenue")
  print(time.taken)
  start.time <- Sys.time()

    p <- ggplot(monthly_revenue, aes(x=usage_date, y=value, color=Revenue)) + 
    # geom_ribbon(aes(x=usage_date, ymax=rev_mill, ymin=base_rev_mill), fill="grey", alpha=.5) +
    geom_line() + 
    scale_linetype_manual(values = c("Baseline"="dashed", "Hypothetical"="solid")) +
    scale_color_manual(values=c("Baseline"="black", "Hypothetical"="steelblue")) +
    xlab("") + ylab("Revenue (Million $)") + 
    # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    # scale_x_date(labels = date_format("%m-%y"), date_breaks="1 months") +
    scale_y_continuous(labels = comma)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("Making line chart") 
  print(time.taken)

  p
}

#******************************************************************
# Histogram of changes (hypothetical - baseline) in total amount 
# paid during the time period for each customer
#******************************************************************
plot_bill_change_histogram <- function(data){
  start.time <- Sys.time()
  
  if(sum(abs(data$changes)) < 1){
    p <- ggplot() + 
      geom_vline(xintercept = 0, color="#CC0000") +
      xlab("Change in total amount paid ($)")
  }
  else{
    p <- ggplot(data, aes(x=changes)) + geom_histogram() + 
      geom_vline(xintercept = mean(data$changes, na.rm=TRUE), color="#CC0000") +
      xlab("Change in total amount paid ($)") + ylab("") +
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("Making histogram") 
  print(time.taken)
  
  p
}

#******************************************************************
# Small boxplot of changes (hypothetical - baseline) in total amount 
# paid during the time period for each customer. Designed to 
# complement the histogram
#******************************************************************
plot_bill_change_boxplot <- function(data){
  if(sum(abs(data$changes)) < 1){
    p <- ggplot() + 
      geom_vline(xintercept = 0, color="#CC0000") +
      xlab("")
  }
  else{
    p <- ggplot(data, aes(change_group, changes)) + geom_boxplot(outlier.size=1) +
      coord_flip() + xlab("") + ylab("") + 
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  }
  p
}

#******************************************************************
# Barchart showing total revenue/usage in each tier in both rates
#******************************************************************
plot_barchart_by_tiers <- function(data, display_type, bar_type){

  if(display_type=="Revenue"){
    # Select revenue in each tier
    d <- colSums(data %>% select(matches("[B|X]R[0-9]")), na.rm=TRUE)
    d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
         mutate(id=1)
    d <- melt(d, id.vars="id" ) %>% 
         mutate(type=ifelse(grepl("B.*", variable), "Baseline", "Hypothetical"),
                Tier = get_tier_name(variable),
                value = value/1000000.0)
    lab_str <- "Variable Rev. During Time Period (Mill. $)"
  }
  else{
    # Select usage in each tier
    d <- colSums(data %>% select(matches("[B|X][0-9]")), na.rm=TRUE)
    d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
         mutate(id=1)
    d <- melt(d, id.vars="id" ) %>% 
         mutate(type=ifelse(grepl("B.*", variable), "Baseline", "Hypothetical"),
                Tier = get_tier_name(variable),
                value = value*0.00229569/1000)
    lab_str <- "Usage During Time Period (Thousand AF)"
  }

  if(bar_type == "Absolute"){
    p <- ggplot(d, aes(type, value, fill=Tier)) + geom_bar(stat="identity") +
      xlab("") + ylab(lab_str)
  }
  else{
    
  }
  p
}

#******************************************************************
# Barchart showing fixed revenue
#******************************************************************
plot_fixed_revenue <- function(data, bar_type){
  
  # Select revenue in each tier
  d <- colSums(data %>% select(baseline_variable_bill, baseline_bill, variable_bill, total_bill), 
               na.rm=TRUE)
  d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
       mutate(baseline_fixed=baseline_bill-baseline_variable_bill,
              hypthetical_fixed=total_bill-variable_bill, id=1) %>%
       mutate(Baseline=100*baseline_fixed/baseline_bill,
              Hypothetical=100*hypthetical_fixed/total_bill) %>%
       select(Baseline, Hypothetical, id)
  d <- melt(d, id.vars="id" ) 
  lab_str <- "Percent Fixed Revenue"

  
  if(sum(d$value) < 0.1){
    p <- ggplot() + 
      geom_hline(yintercept = 0, color="#CC0000") +
      xlab("") + ylab(lab_str)
  }else{
    if(bar_type == "Absolute"){
      p <- ggplot(d, aes(variable, value, fill=variable)) + geom_bar(stat="identity") +
        xlab("") + ylab(lab_str) + #coord_flip() + 
        scale_fill_manual( values=c("Hypothetical"="steelblue", "Baseline"="black") ) +
        guides(fill=FALSE)
    }
    else{
      
    }
  }
  p
}

#******************************************************************
# Given a label like "X1", or "BR3", returns the tier name as 
# "Tier 1" or "Tier 3" respectively
#******************************************************************
get_tier_name <- function(labels){
  return(paste("Tier", stri_sub(labels, -1, -1) ))
}







