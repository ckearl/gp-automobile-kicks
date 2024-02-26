# THE EDA FILE --------
library(tidyverse)
library(janitor)
library(readr)
library(dplyr)

# load the kick data
kick_df <- read.csv("clean_kick.csv")

summary(kick_df)

kick_df %>% 
  count(is_bad_buy)
