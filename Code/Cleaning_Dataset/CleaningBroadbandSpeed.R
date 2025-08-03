library(tidyverse)

# Import datasets with updated paths
broadband_raw <- read_csv("ObtainedDataset/Broadband Speed/201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)
house_prices_raw <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)

# Extract first four characters of postcode in house prices and tally frequency per area
postcode_freq <- house_prices_raw %>%
  mutate(short_pc = substr(Postcode, 1, 4)) %>%
  group_by(short_pc, Town, District, County) %>%
  tally(name = "count") %>%
  arrange(desc(count)) %>%
  group_by(short_pc) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(short_pc, Town, District, County)

# Clean broadband data: add short postcode and keep necessary columns
broadband_clean <- broadband_raw %>%
  mutate(short_pc = substr(postcode_space, 1, 4)) %>%
  select(postcode, postcode_space, short_pc,
         avg_download = `Average download speed (Mbit/s)`,
         avg_upload = `Average upload speed (Mbit/s)`,
         min_download = `Minimum download speed (Mbit/s)`,
         min_upload = `Minimum upload speed (Mbit/s)`) %>%
  filter(!is.na(short_pc), !is.na(avg_download)) %>%
  distinct()

# Join broadband speeds with location info by short postcode
broadband_with_location <- broadband_clean %>%
  inner_join(postcode_freq, by = "short_pc") %>%
  filter(!is.na(District), !is.na(County))

# Review the combined dataset
View(broadband_with_location)

# Write the cleaned combined data to CSV
write_csv(broadband_with_location, "Cleaned Data/cleanedBroadband.csv")
