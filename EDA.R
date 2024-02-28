# THE EDA FILE --------
library(tidyverse)
library(lubridate)
library(janitor)

setwd('~/byU/wint_24/is555/code/gp-automobile-kicks/')

kick_df_raw <- read_csv("https://www.dropbox.com/scl/fi/149un0pc15iid73m5i50r/09_train.csv?rlkey=z08w7u5n0z4tqji6wub1up9fc&dl=1")

# what does mmr mean?
# MMR stands for Manheim Market Report, a tool used to determine the wholesale value of a vehicle.
# does MMR track by VIN?
# MMR is a tool used to determine the wholesale value of a vehicle. It is not a VIN-specific report. MMR is a pricing tool that uses a proprietary algorithm to determine the average price of a vehicle based on its model and model year.

kick_df <- kick_df_raw %>%
  mutate(
    id = row_number(),
    mmr_acquisition_auction_average_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_average_price)), mmr_acquisition_auction_average_price, as.numeric((mmr_acquisition_auction_average_price))),
    mmr_acquisition_auction_clean_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_clean_price)), mmr_acquisition_auction_clean_price, as.numeric((mmr_acquisition_auction_clean_price))),
    mmr_acquisition_retail_average_price = ifelse(is.na(as.numeric(mmr_acquisition_retail_average_price)), mmr_acquisition_retail_average_price, as.numeric((mmr_acquisition_retail_average_price))),
    mmr_acquisiton_retail_clean_price = ifelse(is.na(as.numeric(mmr_acquisiton_retail_clean_price)), mmr_acquisiton_retail_clean_price, as.numeric((mmr_acquisiton_retail_clean_price))),
    mmr_current_auction_average_price = ifelse(is.na(as.numeric(mmr_current_auction_average_price)), mmr_current_auction_average_price, as.numeric((mmr_current_auction_average_price))),
    mmr_current_auction_clean_price = ifelse(is.na(as.numeric(mmr_current_auction_clean_price)), mmr_current_auction_clean_price, as.numeric((mmr_current_auction_clean_price))),
    mmr_current_retail_average_price = ifelse(is.na(as.numeric(mmr_current_retail_average_price)), mmr_current_retail_average_price, as.numeric((mmr_current_retail_average_price))),
    mmr_current_retail_clean_price = ifelse(is.na(as.numeric(mmr_current_retail_clean_price)), mmr_current_retail_clean_price, as.numeric((mmr_current_retail_clean_price))),
    purchase_date = as_datetime(purch_date),
    auction_name = auction,
    year = as.numeric(veh_year),
    age = as.numeric(vehicle_age),
    odometer = as.numeric(veh_odo),
    buyer_number = as.numeric(byrno),
    vendor_zip = as.numeric(vnzip1),
    vendor_state = vnst
  ) %>%
  select(!c(primeunit,
    aucguart,
    wheel_type_id,
    purch_date,
    auction,
    veh_year,
    vehicle_age,
    veh_odo,
    byrno,
    vnzip1,
  ))

kick_df %>%
  select(purch_date) %>%
  arrange(desc(purch_date))

kick_df %>%
  select(id, make, model, sub_model, color, transmission, wheel_type, wheel_type_id)

price_columns <- c("mmr_acquisition_auction_average_price", "mmr_acquisition_auction_clean_price", "mmr_acquisition_retail_average_price", "mmr_acquisiton_retail_clean_price", "mmr_current_auction_average_price", "mmr_current_auction_clean_price", "mmr_current_retail_average_price", "mmr_current_retail_clean_price")

kick_df %>%
  select(sub_model)

# "is_bad_buy",
# --> "purch_date", "purchase_date"
# --> "auction", "auction_name"
# --> "veh_year", "year"
# --> "vehicle_age", "age"
# "make",
# "model",
# "trim",
# "sub_model",
# "color",
# "transmission",
# --> "wheel_type_id", drop column?
# "wheel_type",
# --> "veh_odo", "odometer"
# "nationality",
# "size",
# "top_three_american_name",
# "mmr_acquisition_auction_average_price",
# "mmr_acquisition_auction_clean_price",
# "mmr_acquisition_retail_average_price",
# "mmr_acquisiton_retail_clean_price",
# "mmr_current_auction_average_price",
# "mmr_current_auction_clean_price",
# "mmr_current_retail_average_price",
# "mmr_current_retail_clean_price",
# --> "primeunit", drop column
# --> "aucguart", drop column
# --> "byrno", "buyer_number"
# --> "vnzip1", "vendor_zip"
# --> "vnst", "vendor_state"
# "veh_b_cost", "vehicle_cost"
# "is_online_sale"

# what does MMR stand for?
# MMR stands for Manheim Market Report, a tool used to determine the wholesale value of a vehicle.

kick_df %>%
  select(primeunit) %>%
  count(primeunit)

kick_df %>%
  select(year) %>%
  head()

kick_df %>%
  count(mmr_current_retail_clean_price)


kick_df %>%
  glimpse()


# graphing
kick_df %>%
  select()
