# THE EDA FILE --------
library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)
library(ggplot2)
library(ggcorrplot)

setwd('~/byU/wint_24/is555/code/gp-automobile-kicks/')

kick_df_raw <- read_csv("https://www.dropbox.com/scl/fi/149un0pc15iid73m5i50r/09_train.csv?rlkey=z08w7u5n0z4tqji6wub1up9fc&dl=1")

## format the dataframe
kick_df <- kick_df_raw %>%
  mutate(
    id = row_number(),
    mmr_acq_auction_avg_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_average_price)), mmr_acquisition_auction_average_price, as.numeric((mmr_acquisition_auction_average_price))),
    mmr_acq_auction_clean_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_clean_price)), mmr_acquisition_auction_clean_price, as.numeric((mmr_acquisition_auction_clean_price))),
    mmr_acq_retail_avg_price = ifelse(is.na(as.numeric(mmr_acquisition_retail_average_price)), mmr_acquisition_retail_average_price, as.numeric((mmr_acquisition_retail_average_price))),
    mmr_acq_retail_clean_price = ifelse(is.na(as.numeric(mmr_acquisiton_retail_clean_price)), mmr_acquisiton_retail_clean_price, as.numeric((mmr_acquisiton_retail_clean_price))),
    mmr_curr_auction_avg_price = ifelse(is.na(as.numeric(mmr_current_auction_average_price)), mmr_current_auction_average_price, as.numeric((mmr_current_auction_average_price))),
    mmr_curr_auction_clean_price = ifelse(is.na(as.numeric(mmr_current_auction_clean_price)), mmr_current_auction_clean_price, as.numeric((mmr_current_auction_clean_price))),
    mmr_curr_retail_avg_price = ifelse(is.na(as.numeric(mmr_current_retail_average_price)), mmr_current_retail_average_price, as.numeric((mmr_current_retail_average_price))),
    mmr_curr_retail_clean_price = ifelse(is.na(as.numeric(mmr_current_retail_clean_price)), mmr_current_retail_clean_price, as.numeric((mmr_current_retail_clean_price))),
    purchase_date = as_datetime(purch_date),
    auction_name = auction,
    transmission = ifelse(transmission == "Manual", "MANUAL", transmission),
    year = as.numeric(veh_year),
    age = as.numeric(vehicle_age),
    odometer = as.numeric(veh_odo),
    buyer_number = as.numeric(byrno),
    vendor_zip = as.numeric(vnzip1),
    auction_final_cost = as.numeric(veh_b_cost),
    vendor_state = vnst,
    prime_unit = primeunit,
    region = case_when(
      make %in% c("ACURA", "HONDA", "HYUNDAI", "INFINITI", "ISUZU", "LEXUS", "MITSUBISHI", "NISSAN", "TOYOTA", "TOYOTA SCION", "SCION", "SUBARU", "SUZUKI") ~ "Japan",
      make %in% c("VOLKSWAGEN") ~ "Germany",
      make %in% c("BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "GMC", "HUMMER", "JEEP", "LINCOLN", "OLDSMOBILE", "PONTIAC", "SATURN", "MERCURY", "PLYMOUTH") ~ "USA",
      make %in% c("MINI") ~ "UK",
      make %in% c("VOLVO") ~ "Sweden",
      make %in% c("KIA", "HYUNDAI") ~ "South Korea",
      TRUE ~ "Other"
    ),
    parent_company = case_when(
      make %in% c("BUICK", "CADILLAC", "CHEVROLET", "GMC", "HUMMER", "OLDSMOBILE", "PONTIAC", "SATURN") ~ "GM",
      make %in% c("FORD", "LINCOLN", "MERCURY") ~ "Ford",
      make %in% c("CHRYSLER", "DODGE", "JEEP", "PLYMOUTH") ~ "Fiat-Chrysler",
      make %in% c("ACURA", "HONDA") ~ "Honda",
      make %in% c("HYUNDAI", "KIA") ~ "Hyundai",
      make %in% c("MAZDA") ~ "Mazda",
      make %in% c("MITSUBISHI") ~ "Mitsubishi",
      make %in% c("NISSAN", "INFINITI") ~ "Nissan",
      make %in% c("SUBARU") ~ "Subaru",
      make %in% c("SUZUKI") ~ "Suzuki",
      make %in% c("TOYOTA", "LEXUS", "SCION", "TOYOTA SCION") ~ "Toyota",
      make %in% c("VOLKSWAGEN") ~ "Volkswagen",
      make %in% c("VOLVO") ~ "Volvo",
      TRUE ~ "Other"
    )) %>% 
  select(-c(purch_date, veh_year, vehicle_age, veh_odo, wheel_type_id, byrno, vnzip1, vnst, auction, primeunit, mmr_acquisition_auction_average_price, mmr_acquisition_auction_clean_price, mmr_acquisition_retail_average_price, mmr_acquisiton_retail_clean_price, mmr_current_auction_average_price, mmr_current_auction_clean_price, mmr_current_retail_average_price, mmr_current_retail_clean_price)) %>% 
  select(id, is_bad_buy, make, top_three_american_name, parent_company, region, nationality, model, trim, sub_model, size, color, transmission, year, age, odometer, prime_unit, wheel_type, mmr_acq_auction_avg_price, mmr_acq_auction_clean_price, mmr_acq_retail_avg_price, mmr_acq_retail_clean_price, mmr_curr_auction_avg_price, mmr_curr_auction_clean_price, mmr_curr_retail_avg_price, mmr_curr_retail_clean_price, auction_name, vendor_zip, vendor_state, is_online_sale, warranty_cost, buyer_number, auction_final_cost)

## first batch EDA
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

## second batch EDA
kick_df %>% 
  ggplot(aes(x = year, fill = auction_name)) +
  geom_density(alpha = 0.5) +
  facet_wrap(auction_name~is_bad_buy, nrow = 3)

## extremely useful charts

# Calculate the proportion of bad buys in the dataset
overall_bad_buy_proportion <- mean(kick_df$is_bad_buy)

# Example 1: Proportion of Bad Buys by Parent Company
parent_company_bad_buy <- kick_df %>%
  group_by(parent_company) %>%
  summarise(prop_bad_buy = mean(is_bad_buy))

parent_company_bad_buy %>%
  ggplot(aes(x = parent_company, y = prop_bad_buy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = overall_bad_buy_proportion, color = "red", linetype = "dashed") +
  labs(x = "Parent Company", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Parent Company",
       subtitle = paste("Overall Proportion of Bad Buys:", round(overall_bad_buy_proportion, 3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example 2: Proportion of Bad Buys by Make
make_bad_buy_lt_100 <- kick_df %>%
  filter(!(make %in% c("PLYMOUTH", "TOYOTA SCION", "MINI", "SUBARU", "LEXUS", "CADILLAC", "ACURA", "VOLVO", "INFINITI", "LINCOLN", "SCION", "VOLKSWAGEN", "ISUZU"))) %>%
  group_by(make) %>%
  summarise(prop_bad_buy = mean(is_bad_buy),
            total_cars = n())

bad_buy_prop_lt_100_makes <- kick_df %>% 
  filter(!(make %in% c("PLYMOUTH", "TOYOTA SCION", "MINI", "SUBARU", "LEXUS", "CADILLAC", "ACURA", "VOLVO", "INFINITI", "LINCOLN", "SCION", "VOLKSWAGEN", "ISUZU"))) %>%
  summarise(prop_bad_buy = mean(is_bad_buy))
bad_buy_prop_lt_100_makes <- bad_buy_prop_lt_100_makes[[1]]

make_bad_buy_lt_100 %>%
  ggplot(aes(x = make, y = prop_bad_buy, label = paste(total_cars, "cars"))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_hline(yintercept = bad_buy_prop_lt_100_makes, color = "red", linetype = "dashed") +
  labs(x = "Make", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Make, for Makes with at least 100 cars sold",
       subtitle = paste("Overall Proportion of Bad Buys:", round(bad_buy_prop_lt_100_makes, 5))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

make_bad_buy_lt_30 <- kick_df %>%
  filter(!(make %in% c("PLYMOUTH", "TOYOTA", "MINI", "SUBARU", "LEXUS", "CADILLAC", "ACURA", "VOLVO"))) %>%
  group_by(make) %>%
  summarise(prop_bad_buy = mean(is_bad_buy),
            total_cars = n())

bad_buy_prop_lt_30 <- kick_df %>% 
  filter(!(make %in% c("PLYMOUTH", "TOYOTA", "MINI", "SUBARU", "LEXUS", "CADILLAC", "ACURA", "VOLVO"))) %>%
  summarise(prop_bad_buy = mean(is_bad_buy))
bad_buy_prop_lt_30 <- bad_buy_prop_lt_30[[1]]

make_bad_buy_lt_30 %>%
  ggplot(aes(x = make, y = prop_bad_buy, label = paste(total_cars, "cars"))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_hline(yintercept = bad_buy_prop_lt_30, color = "red", linetype = "dashed") +
  labs(x = "Make", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Make, for Makes with at least 30 cars sold",
       subtitle = paste("Overall Proportion of Bad Buys:", round(bad_buy_prop_lt_30, 5))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example 3: Proportion of Bad Buys by Model
model_bad_buy <- kick_df %>%
  group_by(model) %>%
  summarise(prop_bad_buy = mean(is_bad_buy)) %>%
  top_n(10)

model_bad_buy %>%
  ggplot(aes(x = reorder(model, prop_bad_buy), y = prop_bad_buy)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_hline(yintercept = overall_bad_buy_proportion, color = "red", linetype = "dashed") +
  labs(x = "Model", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Model (Top 10)",
       subtitle = paste("Overall Proportion of Bad Buys:", round(overall_bad_buy_proportion, 3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example 4: Proportion of Bad Buys by Region with Total Car Count Labels
region_car_count <- kick_df %>%
  filter(region != "USA") %>% 
  group_by(region) %>%
  summarise(prop_bad_buy = mean(is_bad_buy),
            total_cars = n())

bad_buy_prop_ex_usa <- kick_df %>% 
  filter(region != "USA") %>% 
  summarise(prop_bad_buy = mean(is_bad_buy))
bad_buy_prop_ex_usa <- bad_buy_prop_ex_usa[[1]]

region_car_count %>%
  ggplot(aes(x = region, y = prop_bad_buy, label = paste(total_cars, "cars"))) +
  geom_bar(stat = "identity", fill = "lightyellow") +
  geom_hline(yintercept = bad_buy_prop_ex_usa, color = "red", linetype = "dashed") +
  geom_text(vjust = -0.5, size = 3) +
  labs(x = "Region", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Region, without USA",
       subtitle = paste("Overall Proportion of Bad Buys:", round(bad_buy_prop_ex_usa, 5))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Example 5: Proportion of Bad Buys by Year with Total Car Count Labels
age_car_count <- kick_df %>%
  group_by(age) %>%
  summarise(prop_bad_buy = mean(is_bad_buy),
            total_cars = n())

age_car_count %>% 
  ggplot(aes(x = age, y = prop_bad_buy, label = paste(total_cars, "cars"))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_hline(yintercept = overall_bad_buy_proportion, color = "red", linetype = "dashed") +
  geom_text(vjust = -0.5, size = 3) +
  labs(x = "Year", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Age of Vehicle",
       subtitle = paste("Overall Proportion of Bad Buys:", round(overall_bad_buy_proportion, 3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

kick_df %>% 
  count(make) %>% 
  arrange(desc(n)) %>% 
  print( n=30)

### PRACTICE with the king Prof Wilson himself

kick_df %>% 
  group_by(year) %>% 
  summarise(total_bad = sum(is_bad_buy == 1),
            total_good = sum(is_bad_buy == 0)) %>% 
  pivot_longer(
    cols = c(total_bad, total_good),
    values_to = "total",
    names_to = "outcome_level"
  ) %>% 
  ggplot(aes(x = year, y = total, fill = outcome_level)) +
  geom_col() +
  theme_bw()


# Potential correlation matrix?
selected_columns <- kick_df %>%
  select(age, odometer, warranty_cost, mmr_acq_auction_clean_price, 
         mmr_acq_retail_clean_price, mmr_curr_auction_clean_price, 
         mmr_curr_retail_clean_price, auction_final_cost)

corr_matrix <- cor(selected_columns)

ggcorrplot(corr_matrix,
           type = "lower",
           outline.color = "white",
           lab = TRUE,
           method = "square",
           title = "Correlation Plot of Numeric Variables",
           ggtheme = theme_minimal())


## somewhat useful charts

# Example 6: Correlation Network Plot
library(igraph)
correlation_graph <- graph_from_adjacency_matrix(correlation_matrix, weighted = TRUE)
plot(correlation_graph, edge.arrow.size = 0.5, edge.curved = TRUE)

# Example 7: Multiple Variable Bubble Chart
kick_df %>% 
  filter(region == "USA") %>% 
  ggplot(aes(x = age, y = odometer, size = mmr_acq_auction_avg_price, color = is_bad_buy)) +
  geom_point(alpha = 0.4) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Age", y = "Odometer", size = "MMR Acquisition Auction Average Price", color = "Is Bad Buy")

# Example 9: Conditional Density Plot
kick_df %>% 
  ggplot(aes(x = age, fill = region)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~is_bad_buy) +
  labs(x = "Age", fill = "Make")

# Example 10: Faceted Boxplot
ggplot(kick_df, aes(x = factor(is_bad_buy), y = mmr_acq_auction_avg_price)) +
  geom_boxplot() +
  facet_wrap(~transmission)
