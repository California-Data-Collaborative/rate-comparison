
library(plotly)
library(shiny)
library(dplyr)
library(lubridate)
library(data.table)

#set the utility_code from the config file
source("R/utility_code.R")
source("R/helper_fns.R", local=TRUE)
source("R/make_plots.R", local=TRUE)

source("R/class_graphs.R")
source("R/rate_inputs.R")
#******************************************************************
# Read in the data and map the columns to application columns
#******************************************************************

read_data <- function(filename, cust_col, usage_col, month_col, year_col, et_col=NULL, 
                      hhsize_col=NULL, irr_area_col=NULL, rate_code_col, cust_class_col, 
                      water_type_col, meter_size_col, less_than_date){
  
  print("Reading data...")
  start.time <- Sys.time()
  data <- tbl_df(read.csv(filename,stringsAsFactors = FALSE)) %>% 
    dplyr::rename_(.dots=setNames(list(cust_col), "cust_id")) %>%
    dplyr::rename_(.dots=setNames(list(usage_col), "usage_ccf")) %>%
    dplyr::rename_(.dots=setNames(list(month_col), "usage_month")) %>%
    dplyr::rename_(.dots=setNames(list(year_col), "usage_year")) %>%
    dplyr::rename_(.dots=setNames(list(rate_code_col), "rate_code")) %>%
    dplyr::rename_(.dots=setNames(list(cust_class_col), "cust_class")) %>%
    dplyr::rename_(.dots=setNames(list(water_type_col), "water_type")) %>%
    dplyr::rename_(.dots=setNames(list(meter_size_col), "meter_size")) %>%
    dplyr::mutate(usage_date = as.Date(usage_date)) %>%
    dplyr::mutate(rate_code = as.character(rate_code)) %>%
    dplyr::mutate(cust_class = as.character(cust_class)) %>%
    dplyr::mutate(water_type = as.character(water_type)) %>%
    dplyr::mutate(meter_size = as.character(meter_size)) %>%
    dplyr::filter(usage_date < as.Date(less_than_date)) %>%
    dplyr::arrange(usage_date, cust_class) %>%
    dplyr::mutate(sort_index=1:nrow(.))
  
  if(!is.null(et_col)&!is.null(hhsize_col)&!is.null(irr_area_col)){
    data <- data %>%
      dplyr::rename_(.dots=setNames(list(et_col), "et_amount")) %>%
      dplyr::rename_(.dots=setNames(list(hhsize_col), "hhsize")) %>%
      dplyr::rename_(.dots=setNames(list(irr_area_col), "irr_area"))
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  print("...loaded.")
  return(data)
}

generated_inputs <- list()
baseline_rate_list <- RateParser::read_owrs_file("mnwd.owrs")


is_budget <- switch(utility_code,
                    "IRWD"=,
                    "MNWD"=,
                    "LVMWD"=,
                    "SMWD"=TRUE,
                    "SMC"=FALSE)

less_than_date <- switch(utility_code,
                         "IRWD"="2017-01-01",
                         "MNWD"="2017-01-01",
                         "LVMWD"="2017-01-01",
                           "SMWD"="2016-01-01",
                         "SMC"="2017-01-01")

test_file <- switch(utility_code,
                    "IRWD"="data/irwd_test.csv",
                    "MNWD"="data/mnwd_test1.csv",
                    "LVMWD"="data/lvmwd_test.csv",
                    "SMWD"="data/smwd_test.csv",
                    "SMC"="data/smc_test.csv")



# Read data from file and rename the columns to be compatable with internal calls
if(is_budget){
  df <- read_data(test_file, cust_col="cust_loc_id", usage_col="usage_ccf", month_col="usage_month", 
                  year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                  irr_area_col="cust_loc_irr_area_sf", cust_class_col= "cust_loc_rate_class", 
                  rate_code_col = "cust_loc_class_from_utility", water_type_col="cust_loc_water_type",
                  meter_size_col="cust_loc_meter_size", less_than_date=less_than_date)
} else{
  df <- read_data(test_file, cust_col="cust_loc_id", usage_col="usage_ccf", month_col="usage_month", 
                  year_col="usage_year", cust_class_col= "cust_loc_rate_class", 
                  rate_code_col = "cust_loc_class_from_utility", water_type_col="cust_loc_water_type",
                  meter_size_col="cust_loc_meter_size", less_than_date=less_than_date)
}


# The columns that will appear in the "depends on" dropdowns
not_included_cols <- c("cust_id", "sort_index", "usage_year", "usage_ccf", "irr_area", 
                        "hhsize", "cust_class", "et_amount", "usage_date")
dropdown_cols <- names(df)[which(!(names(df) %in% not_included_cols) )]


# Update the time slider with the actual date values in the data
min_date <- min(df$usage_date)
max_date <- max(df$usage_date)
print(min_date, max_date)

# list of unique customer classes in the data
cust_class_list <- names(baseline_rate_list$rate_structure)
#in case there are any classes in the data that are not defined in the ORWS file
cust_class_list_from_data <- unique(df$cust_class)
cust_class_list_from_data[(cust_class_list_from_data %in% cust_class_list)]
classes_not_in_OWRS <- cust_class_list_from_data[!(cust_class_list_from_data %in% cust_class_list)]
cust_class_list <- c(cust_class_list, classes_not_in_OWRS )

# Generate the defaults that will populate tier boxes for which a utility
# has no value. For example, a budget-based utility still needs default values
# when they switch to try our a Tiered rate.
tier_boxes <- list()
for(c in cust_class_list){
  tier_boxes[[c]] <- list("Tiered"=list(), "Budget"=list())
  tier_boxes[[c]][["Tiered"]][["tier_starts"]] <- c(0, 15, 41, 149)
  tier_boxes[[c]][["Tiered"]][["tier_prices"]] <- c(2.87, 4.29, 6.44, 10.07)
  tier_boxes[[c]][["Budget"]][["tier_starts"]] <- list(0, "40%", "101%", "131%")
  tier_boxes[[c]][["Budget"]][["tier_prices"]] <- c(1.11, 1.62, 3.92, 14.53)
  
  if(baseline_rate_list$rate_structure[[c]]$commodity_charge %in% c("Tiered", "Budget") &&
     !is.null(baseline_rate_list$rate_structure[[c]]$tier_starts) &&
     !is.null(baseline_rate_list$rate_structure[[c]]$tier_prices)){
    rate_type <- baseline_rate_list$rate_structure[[c]]$commodity_charge
    
    tier_boxes[[c]][[rate_type]][["tier_starts"]] <- baseline_rate_list$rate_structure[[c]]$tier_starts
    tier_boxes[[c]][[rate_type]][["tier_prices"]] <- baseline_rate_list$rate_structure[[c]]$tier_prices
  }
}


# filtering rate codes by customer class
rate_code_counts <- df %>% group_by(cust_class, rate_code) %>% 
                      summarise(count=length(unique(cust_id))) %>% arrange(desc(count))

# create a list of the rate code counts within each customer class
rate_code_dict <- list()
for(c in cust_class_list){
  rate_code_dict[[c]] <- filter(rate_code_counts, cust_class == c)$rate_code
}


