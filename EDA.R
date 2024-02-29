# THE EDA FILE --------
library(tidyverse)
library(lubridate)
library(janitor)
# library(plot_ly)
library(ggplot2)

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
    )) %>% 
  select(-c(purch_date, veh_year, vehicle_age, veh_odo, wheel_type_id, byrno, vnzip1, vnst, auction, primeunit, mmr_acquisition_auction_average_price, mmr_acquisition_auction_clean_price, mmr_acquisition_retail_average_price, mmr_acquisiton_retail_clean_price, mmr_current_auction_average_price, mmr_current_auction_clean_price, mmr_current_retail_average_price, mmr_current_retail_clean_price)) %>% 
  select(id, is_bad_buy, make, top_three_american_name, parent_company, region, nationality, model, trim, sub_model, size, color, transmission, year, age, odometer, prime_unit, wheel_type, mmr_acq_auction_avg_price, mmr_acq_auction_clean_price, mmr_acq_retail_avg_price, mmr_acq_retail_clean_price, mmr_curr_auction_avg_price, mmr_curr_auction_clean_price, mmr_curr_retail_avg_price, mmr_curr_retail_clean_price, auction_name, vendor_zip, vendor_state, is_online_sale, warranty_cost, buyer_number, auction_final_cost)

kick_df %>% glimpse()

# Example 1: Scatterplot Matrix with Color Encoding
pairs(kick_df[, c("is_bad_buy", "age", "odometer", "mmr_acq_auction_avg_price")], 
      col = ifelse(kick_df$is_bad_buy == 1, "red", "blue"))

# Example 2: Parallel Coordinates Plot
kick_df %>%
  select(-id) %>%
  plotly::plot_ly(type = 'parcoords', line = list(color = ~is_bad_buy,
                                                  colorscale = list(c(0,1), c('blue', 'red'))))

# Example 3: Clustered Heatmap
correlation_matrix <- cor(kick_df[, c("is_bad_buy", "age", "odometer", "mmr_acq_auction_avg_price")])
ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red") +
  labs(x = "Variable", y = "Variable", fill = "Correlation")

# Example 4: Interactive Trellis Plot
ggplot(kick_df, aes(x = factor(is_bad_buy), fill = make)) +
  geom_bar() +
  facet_wrap(~region) +
  labs(x = "Is Bad Buy", y = "Count", fill = "Make")

# Example 5: Hexbin Scatterplot
ggplot(kick_df, aes(x = age, y = mmr_acq_auction_avg_price)) +
  geom_hex(aes(fill = is_bad_buy)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Age", y = "MMR Acquisition Auction Average Price", fill = "Is Bad Buy")

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

# Example 8: Interactive 3D Scatterplot
plot_ly(kick_df, x = ~age, y = ~odometer, z = ~mmr_acq_auction_avg_price, color = ~is_bad_buy, colors = c('blue', 'red'), type = "scatter3d")

# Example 9: Conditional Density Plot
kick_df %>% 
  ggplot(aes(x = age, fill = region)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~is_bad_buy) +
  labs(x = "Age", fill = "Make")

# Example 10: Faceted Boxplot
ggplot(kick_df, aes(x = factor(is_bad_buy), y = mmr_acq_auction_avg_price)) +
  geom_boxplot() +
  facet_wrap(~transmission) +
  lab


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
make_bad_buy <- kick_df %>%
  group_by(make) %>%
  summarise(prop_bad_buy = mean(is_bad_buy))

make_bad_buy %>%
  ggplot(aes(x = make, y = prop_bad_buy)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_hline(yintercept = overall_bad_buy_proportion, color = "red", linetype = "dashed") +
  labs(x = "Make", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Make",
       subtitle = paste("Overall Proportion of Bad Buys:", round(overall_bad_buy_proportion, 3))) +
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

region_car_count %>%
  ggplot(aes(x = region, y = prop_bad_buy, label = paste(total_cars, "cars"))) +
  geom_bar(stat = "identity", fill = "lightyellow") +
  geom_hline(yintercept = overall_bad_buy_proportion_ex_usa, color = "red", linetype = "dashed") +
  geom_text(vjust = -0.5, size = 3) +
  labs(x = "Region", y = "Proportion of Bad Buys",
       title = "Proportion of Bad Buys by Region, without USA",
       subtitle = paste("Overall Proportion of Bad Buys:", round(overall_bad_buy_proportion, 3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

overall_bad_buy_proportion_ex_usa <- kick_df %>% 
  filter(region != "USA") %>% 
  summarise(prop_bad_buy = mean(is_bad_buy))
# format the code above to be a variable
overall_bad_buy_proportion_ex_usa <- overall_bad_buy_proportion_ex_usa[[1]]

### PRACTICE with the king
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


