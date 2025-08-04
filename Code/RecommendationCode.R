library(tidyverse)
library(dplyr)

HousePrice = read_csv("Cleaned Data/cleanedHousePrices.csv")
Performance = read.csv("Cleaned Data/cleanedPerformance.csv")
Crime = read.csv("Cleaned Data/cleanedCrime.csv")
Broadband = read.csv("Cleaned Data/cleanedBroadband.csv")

glimpse(HousePrice)
glimpse(Performance)
glimpse(Crime)
glimpse(Broadband)

top_towns_house <- HousePrice %>%
  group_by(Town, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_price)) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10)

top_towns_house %>% select(rank, Town, County, avg_price)


top_towns_broadband <- Broadband %>%
  group_by(Town, County) %>%
  summarise(avg_download_speed = mean(avg_download, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_download_speed)) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10)

top_towns_broadband %>% select(rank, Town, County, avg_download_speed)


top_towns_performance <- Performance %>%
  filter(!is.na(as.numeric(ATT8SCR))) %>%  # Remove "NE" and other non-numeric
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  group_by(Town, County) %>%
  summarise(avg_att8 = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_att8)) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10)

top_towns_performance %>% select(rank, Town, County, avg_att8)


top_towns_crime <- Crime %>%
  group_by(Town = District, County) %>%  # LSOA doesn’t give Town directly; District is close
  summarise(total_crimes = sum(Crime_Count, na.rm = TRUE), .groups = 'drop') %>%
  arrange(total_crimes) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10)

top_towns_crime %>% select(rank, Town, County, total_crimes)

 