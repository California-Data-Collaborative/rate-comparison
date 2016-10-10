
library(plotly)
library(shiny)
library(dplyr)

#set the utility_code from the config file
source("utility_code.R")


#******************************************************************
# Read in the data and map the columns to application columns
#******************************************************************
read_data <- function(filename, cust_col, usage_col, month_col, year_col, et_col, hhsize_col, irr_area_col, 
                      rate_code_col, cust_class_col,less_than_date){
  print("Reading data...")
  start.time <- Sys.time()
  
  data <- tbl_df(read.csv(filename)) %>% 
    dplyr::rename_(.dots=setNames(list(cust_col), "cust_id")) %>%
    dplyr::rename_(.dots=setNames(list(usage_col), "usage_ccf")) %>%
    dplyr::rename_(.dots=setNames(list(month_col), "usage_month")) %>%
    dplyr::rename_(.dots=setNames(list(year_col), "usage_year")) %>%
    dplyr::rename_(.dots=setNames(list(et_col), "et_amount")) %>%
    dplyr::rename_(.dots=setNames(list(hhsize_col), "hhsize")) %>%
    dplyr::rename_(.dots=setNames(list(irr_area_col), "irr_area")) %>%
    dplyr::rename_(.dots=setNames(list(rate_code_col), "rate_code")) %>%
    dplyr::rename_(.dots=setNames(list(cust_class_col), "cust_class")) %>%
    dplyr::mutate(usage_date = as.Date(usage_date)) %>%
    dplyr::arrange(usage_date) %>%
    filter(usage_date < as.Date(less_than_date))
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  print("...loaded.")
  return(data)
}


less_than_date <- switch(utility_code,
                         "MNWD"="2017-01-01",
                         "LVMWD"="2017-01-01",
                         "SMWD"="2016-01-01")

test_file <- switch(utility_code,
                    "MNWD"="data/mnwd_test.csv",
                    "LVMWD"="data/lvmwd_test.csv",
                    "SMWD"="data/smwd_test.csv")

#---------------------Utility Specific UI Defaults --------------------------
default_fixed_charge <- switch(utility_code,
                               "MNWD"=11.39,
                               "LVMWD"=18.30,
                               "SMWD"=8.72)

default_gpcd <- switch(utility_code,
                       "MNWD"=60,
                       "LVMWD"=55,
                       "SMWD"=55)

default_et_factor <- switch(utility_code,
                            "MNWD"=0.7,
                            "LVMWD"=0.8,
                            "SMWD"=0.8)

#******* TIERED RATES ******
default_tiered_tiers_html <- switch(utility_code,
                                    "MNWD"=,
                                    "LVMWD"='<textarea id="tieredTiers" rows="6" cols="15" style="resize: none;">0\n16\n67\n200</textarea>',
                                    "SMWD"='<textarea id="tieredTiers" rows="6" cols="15" style="resize: none;">0\n7\n21\n36\n71</textarea>')

default_tiered_prices_html <- switch(utility_code,
                                     "MNWD"=,
                                     "LVMWD"='<textarea id="tieredPrice" rows="6" cols="15" style="resize: none;">2.31\n2.80\n3.81\n5.34</textarea>',
                                     "SMWD"='<textarea id="tieredPrice" rows="6" cols="15" style="resize: none;">2.04\n2.29\n2.77\n3.28\n4.50</textarea>')

#******* BUDGET RATES ******
default_budget_tiers_html <- switch(utility_code,
                                    "MNWD"='<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n101%\n126%\n151%</textarea>',
                                    "LVMWD"='<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n101%\n151%</textarea>',
                                    "SMWD"='<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n101%\n151%\n201%</textarea>') 

default_budget_prices_html <- switch(utility_code,
                                     "MNWD"='<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">1.49\n1.70\n2.62\n4.38\n9.17</textarea>',
                                     "LVMWD"='<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">2.36\n3.18\n3.96\n4.98</textarea>',
                                     "SMWD"='<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">1.86\n2.11\n2.61\n3.21\n4.67</textarea>') 


# Read data from file and rename the columns to be compatable with internal calls
df <- read_data(test_file, cust_col="cust_loc_id", usage_col="usage_ccf", month_col="usage_month", 
                year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                irr_area_col="cust_loc_irr_area_sf", rate_code_col= "cust_loc_class", cust_class_col = "cust_loc_class_from_utility",
                less_than_date=less_than_date)


# Update the time slider with the actual date values in the data
min_date <- min(df$usage_date)
max_date <- max(df$usage_date)
print(min_date, max_date)

# filtering rate codes by customer class
df1 <- df %>% 
  select(cust_class,rate_code)

df1$rate_code <- as.character(df1$rate_code)


r2 <- aggregate(cust_class~rate_code, df1, FUN=unique)

r3.1 <-  df1 %>%
  filter(cust_class == "RESIDENTIAL_SINGLE")

r3.2 <-  df1 %>%
  filter(cust_class == "RESIDENTIAL_MULTI")

r3.3 <-  df1 %>%
  filter(cust_class == "COMMERCIAL")

r3.4 <-  df1 %>%
  filter(cust_class == "IRRIGATION")

r3.5 <-  df1 %>%
  filter(cust_class == "INSTITUTIONAL")

r3.6 <-  df1 %>%
  filter(cust_class == "OTHER")


variables <- unique(r3.1$rate_code)
variables1 <- unique(r3.2$rate_code)
variables2 <- unique(r3.3$rate_code)
variables3 <- unique(r3.4$rate_code)
variables4 <-  unique(r3.5$rate_code)
variables5 <-  unique(r3.6$rate_code)





