# THE EDA FILE --------
library(tidyverse)
library(janitor)

setwd('~/byU/wint_24/is555/code/gp-automobile-kicks/')

# load the kick data
kick_df <- read_csv("clean_kick.csv")

summary(kick_df)
price_columns <- c("mmr_acquisition_auction_average_price", "mmr_acquisition_auction_clean_price", "mmr_acquisition_retail_average_price", "mmr_acquisiton_retail_clean_price", "mmr_current_auction_average_price", "mmr_current_auction_clean_price", "mmr_current_retail_average_price", "mmr_current_retail_clean_price")

# "id", "is_bad_buy", "purch_date", "auction", "veh_year", "vehicle_age", "make", "model", "trim", "sub_model", "color", "transmission"                        , "wheel_type_id", "wheel_type"                          , "veh_odo", "nationality"              , "size", "top_three_american_name", "mmr_acquisition_auction_average_price", "mmr_acquisition_auction_clean_price", "mmr_acquisition_retail_average_price", "mmr_acquisiton_retail_clean_price", "mmr_current_auction_average_price", "mmr_current_auction_clean_price", "mmr_current_retail_average_price", "mmr_current_retail_clean_price", "primeunit", "aucguart", "byrno", "vnzip1", "vnst", "veh_b_cost", "is_online_sale"

kick_df %>% 
  count(mmr_current_retail_clean_price)

kick_df <- kick_df %>% 
  mutate(
    mmr_acquisition_auction_average_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_average_price)), mmr_acquisition_auction_average_price, as.numeric((mmr_acquisition_auction_average_price))),
    mmr_acquisition_auction_clean_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_clean_price)), mmr_acquisition_auction_clean_price, as.numeric((mmr_acquisition_auction_clean_price))),
    mmr_acquisition_retail_average_price = ifelse(is.na(as.numeric(mmr_acquisition_retail_average_price)), mmr_acquisition_retail_average_price, as.numeric((mmr_acquisition_retail_average_price))),
    mmr_acquisiton_retail_clean_price = ifelse(is.na(as.numeric(mmr_acquisiton_retail_clean_price)), mmr_acquisiton_retail_clean_price, as.numeric((mmr_acquisiton_retail_clean_price))),
    mmr_current_auction_average_price = ifelse(is.na(as.numeric(mmr_current_auction_average_price)), mmr_current_auction_average_price, as.numeric((mmr_current_auction_average_price))),
    mmr_current_auction_clean_price = ifelse(is.na(as.numeric(mmr_current_auction_clean_price)), mmr_current_auction_clean_price, as.numeric((mmr_current_auction_clean_price))),
    mmr_current_retail_average_price = ifelse(is.na(as.numeric(mmr_current_retail_average_price)), mmr_current_retail_average_price, as.numeric((mmr_current_retail_average_price))),
    mmr_current_retail_clean_price = ifelse(is.na(as.numeric(mmr_current_retail_clean_price)), mmr_current_retail_clean_price, as.numeric((mmr_current_retail_clean_price)))
  )

# For numerical variables
summary(kick_df$veh_year)
summary(kick_df$vehicle_age)
summary(kick_df$veh_odo)
hist(kick_df$veh_odo, main = "Histogram of Vehicle Odometer", xlab = "Odometer Reading", col = "lightblue", border = "black")
hist(kick_df$mmr_acquisition_auction_average_price, main = "Histogram of MMR Acquisition Auction Average Price", xlab = "MMR Acquisition Auction Average Price", col = "lightblue", border = "black")

kick_df %>% 
  select(mmr_acquisition_auction_average_price)

# For categorical variables
table(kick_df$color)
table(kick_df$transmission)
table(kick_df$auction)
table(kick_df$nationality)
table(kick_df$size)
table(kick_df$top_three_american_name)
table(kick_df$wheel_type)
table(kick_df$primeunit)
table(kick_df$aucguart)

