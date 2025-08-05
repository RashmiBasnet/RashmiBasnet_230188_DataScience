# Load necessary libraries
library(tidyverse)
library(readr)
library(scales)  # for rescale function

# Load Datasets
HousePrice = read_csv("Cleaned Data/cleanedHousePrices.csv")
Performance = read_csv("Cleaned Data/cleanedPerformance.csv")
Crime = read_csv("Cleaned Data/cleanedCrime.csv")
Broadband = read_csv("Cleaned Data/cleanedBroadband.csv")
LSOA = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# ---------------------
# STEP 1: Clean column names for joining
# ---------------------
# Rename for consistency (remove space in column name)
Crime <- Crime %>% rename(LSOA_code = `LSOA code`)
LSOA <- LSOA %>% rename(LSOA_code = `LSOA code`)

# ---------------------
# STEP 2: Enrich Crime Data with Location Info
# ---------------------
crime_enriched <- Crime %>%
  left_join(
    LSOA %>% select(LSOA_code, Town, District, County),
    by = "LSOA_code"
  ) %>%
  rename(
    District = District.y,
    County = County.y
  ) %>%
  filter(!is.na(County), !is.na(District), !is.na(Town))

# ---------------------
# STEP 3: Summarize Datasets by Town, District, County
# ---------------------
crime_summary <- crime_enriched %>%
  group_by(County, District, Town) %>%
  summarise(TotalCrimeCount = sum(Crime_Count, na.rm = TRUE), .groups = "drop")

house_summary <- HousePrice %>%
  group_by(County, District, Town) %>%
  summarise(AverageHousePrice = mean(Price, na.rm = TRUE), .groups = "drop")

performance_summary <- Performance %>%
  group_by(County, District, Town) %>%
  summarise(Average_Attainment8Score = mean(as.numeric(ATT8SCR), na.rm = TRUE), .groups = "drop")

broadband_summary <- Broadband %>%
  group_by(County, District, Town) %>%
  summarise(
    AvgDownload = mean(avg_download, na.rm = TRUE),
    AvgUpload = mean(avg_upload, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------
# STEP 4: Merge All Summarized Data
# ---------------------
combined_summary <- house_summary %>%
  full_join(performance_summary, by = c("County", "District", "Town")) %>%
  full_join(crime_summary, by = c("County", "District", "Town")) %>%
  full_join(broadband_summary, by = c("County", "District", "Town")) %>%
  filter(complete.cases(.))  # Remove rows with NA for scoring

# ---------------------
# STEP 5: View Final Combined Data
# ---------------------
glimpse(combined_summary)

# ---------------------
# STEP 6: Recommend Top 10 Towns with Scoring
# ---------------------

# Rescale helper function (0 to 1)
rescale_01 <- function(x) scales::rescale(x, to = c(0, 1), na.rm = TRUE)

recommendation_df <- combined_summary %>%
  mutate(
    norm_attainment = rescale_01(Average_Attainment8Score),
    norm_crime = rescale_01(-TotalCrimeCount),         # lower crime better
    norm_broadband = rescale_01(AvgDownload),
    norm_price = rescale_01(-AverageHousePrice)        # lower price better
  ) %>%
  mutate(
    Score = 0.35 * norm_attainment + 0.30 * norm_crime + 0.25 * norm_broadband + 0.10 * norm_price,
    Rating = round(Score * 5, 2)  # Scale score to 0-5 rating and round to 2 decimals
  ) %>%
  arrange(desc(Score)) %>%
  slice_head(n = 10) %>%
  mutate(TownLabel = paste0(Town, " (", District, ")"))

# View top 10 with rating
View(recommendation_df)

# ---------------------
# STEP 7: Visualization of Top 10 Towns with Rating out of 5
# ---------------------

library(ggplot2)

ggplot(recommendation_df, aes(x = fct_reorder(TownLabel, Rating), y = Rating, fill = County)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "Top 10 Recommended Towns to Live In",
    subtitle = "Composite rating out of 5 based on Attainment, Crime, Broadband & House Price",
    x = "Town (District)",
    y = "Rating (out of 5)"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5)

print(recommendation_df)
