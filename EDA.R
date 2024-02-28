# THE EDA FILE --------
library(tidyverse)
library(lubridate)
library(janitor)

kick_df_raw <- read_csv("https://www.dropbox.com/scl/fi/149un0pc15iid73m5i50r/09_train.csv?rlkey=z08w7u5n0z4tqji6wub1up9fc&dl=1")

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
    vendor_state = vnst,
    is_bad_buy = as.factor(is_bad_buy)
  ) %>%
  select(!c(
    primeunit,
    aucguart,
    wheel_type_id,
    purch_date,
    auction,
    veh_year,
    vehicle_age,
    veh_odo,
    byrno,
    vnzip1,
    vnst
  ))

kick_df <- kick_df %>%
  mutate(parent_company = case_when(
    make %in% c("BUICK", "CADILLAC", "CHEVROLET", "GMC", "HUMMER", "OLDSMOBILE", "PONTIAC", "SATURN") ~ "General Motors",
    make %in% c("FORD", "LINCOLN", "MERCURY") ~ "Ford Motor Company",
    make %in% c("CHRYSLER", "DODGE", "JEEP", "PLYMOUTH") ~ "Fiat Chrysler Automobiles",
    make %in% c("ACURA", "HONDA") ~ "Honda Motor Co.",
    make %in% c("HYUNDAI", "KIA") ~ "Hyundai Motor Group",
    make %in% c("MAZDA") ~ "Mazda Motor Corporation",
    make %in% c("MITSUBISHI") ~ "Mitsubishi Motors Corporation",
    make %in% c("NISSAN", "INFINITI") ~ "Nissan Motor Corporation",
    make %in% c("SUBARU") ~ "Subaru Corporation",
    make %in% c("SUZUKI") ~ "Suzuki Motor Corporation",
    make %in% c("TOYOTA", "LEXUS", "SCION", "TOYOTA SCION") ~ "Toyota Motor Corporation",
    make %in% c("VOLKSWAGEN") ~ "Volkswagen Group",
    make %in% c("VOLVO") ~ "Volvo Car Group",
    TRUE ~ "Other"
  ))

kick_df <- kick_df %>%
  mutate(region = case_when(
    make %in% c("ACURA", "HONDA", "HYUNDAI", "INFINITI", "ISUZU", "LEXUS", "MITSUBISHI", "NISSAN", "TOYOTA", "TOYOTA SCION", "SCION", "SUBARU", "SUZUKI") ~ "Japan",
    make %in% c("VOLKSWAGEN") ~ "Germany",
    make %in% c("BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "GMC", "HUMMER", "JEEP", "LINCOLN", "OLDSMOBILE", "PONTIAC", "SATURN", "MERCURY", "PLYMOUTH") ~ "USA",
    make %in% c("MINI") ~ "UK",
    make %in% c("VOLVO") ~ "Sweden",
    make %in% c("KIA", "HYUNDAI") ~ "South Korea",
    TRUE ~ "Other"
  ))


kick_df  %>%
  glimpse()

# EDA

ggplot(kick_df, aes(x = is_bad_buy, fill = is_bad_buy)) +
  geom_bar() +
  labs(title = "Is_Bad_Buy Distribution",
       x = "Is Bad Buy",
       y = "Frequency") +
  scale_fill_manual(values = c("lightgreen", "red"))

ggplot(kick_df, aes(x = odometer, fill = is_bad_buy)) +
  geom_density(alpha = 0.5) +
  labs(title = "Odometer Distribution / Is Bad Buy",
       x = "Odometer",
       y = "Frequency")

ggplot(kick_df, aes(x = age, fill = is_bad_buy)) +
  geom_histogram(alpha = 0.5, binwidth = 1) +
  labs(title = "Age Distribution / Is Bad Buy",
       x = "Age",
       y = "Frequency")


ggplot(kick_df, aes(x = mmr_acquisition_auction_average_price, fill = is_bad_buy)) +
  geom_density(alpha = 0.5) +
  labs(title = "MMR Acquisition Auction Average Price Distribution / Is Bad Buy",
       x = "MMR Acquisition Auction Average Price",
       y = "Frequency")


ggplot(kick_df, aes(x = odometer, y=mmr_current_auction_average_price, color = is_bad_buy)) +
  geom_point(alpha=.5) +
  labs(title = "Odometer vs MMR Current Auction Average Price",
    x = "Odometer",
    y = "MMR Current Auction Average Price"
  )

# is_bad_buy
# make
# model
# trim
# sub_model
# color
# transmission
# wheel_type
# nationality
# size
# top_three_american_name
# mmr_acquisition_auction_average_price
# mmr_acquisition_auction_clean_price
# mmr_acquisition_retail_average_price
# mmr_acquisiton_retail_clean_price
# mmr_current_auction_average_price
# mmr_current_auction_clean_price
# mmr_current_retail_average_price
# mmr_current_retail_clean_price
# veh_b_cost
# is_online_sale
# warranty_cost
# id
# purchase_date
# auction_name
# year
# age
# odometer
# buyer_number
# vendor_zip
# vendor_state
# region
# parent_company