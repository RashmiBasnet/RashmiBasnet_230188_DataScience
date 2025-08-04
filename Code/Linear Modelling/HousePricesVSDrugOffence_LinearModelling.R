library(tidyverse)
library(ggpubr)

# --- Load cleaned datasets ---
housing_data <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
crime_data <- read_csv("Cleaned Data/cleanedCrime.csv", show_col_types = FALSE)
postcode_data <- read_csv("Cleaned Data/cleanedPostcode_LSOA.csv", show_col_types = FALSE)

# --- Merge postcode info to crime data ---
crime_with_postcode <- crime_data %>%
  left_join(postcode_data %>% select(`LSOA code`, shortPostcode), by = c("LSOA code" = "LSOA code")) %>%
  drop_na(shortPostcode)

# --- Extract drug-related crime statistics for 2023 ---
drug_cases_2023 <- crime_with_postcode %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  count(shortPostcode, name = "TotalDrugCrimes")

# --- Merge population info and compute offense rate ---
drug_stats <- drug_cases_2023 %>%
  left_join(postcode_data %>% select(shortPostcode, Population2023, County) %>% distinct(), by = "shortPostcode") %>%
  mutate(DrugRatePer10k = (TotalDrugCrimes / Population2023) * 10000) %>%
  drop_na()

# --- Filter and compute mean house prices by postcode ---
price_summary <- housing_data %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(AvgHousePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# --- Combine price and crime stats ---
merged_df <- inner_join(price_summary, drug_stats, by = c("shortPostcode", "County"))

# --- Plot: Drug crime rate vs average house price with solid regression line ---
ggplot(merged_df, aes(x = DrugRatePer10k, y = AvgHousePrice, color = County)) +
  geom_point(size = 3, alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE) +  # Solid regression line
  labs(
    title = "Impact of Drug Offense Rates on Average House Prices (2023)",
    x = "Drug Offenses per 10,000 Residents",
    y = "Average House Price (£)",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  scale_color_brewer(palette = "Dark2")

# --- Fit linear model with interaction between offense rate and county ---
lm_fit <- lm(AvgHousePrice ~ DrugRatePer10k * County, data = merged_df)
summary(lm_fit)

# --- Correlation overall and by each county ---
overall_correlation <- cor(merged_df$AvgHousePrice, merged_df$DrugRatePer10k, use = "complete.obs")
cat("Overall Correlation:", round(overall_correlation, 3), "\n")

county_wise_corr <- merged_df %>%
  group_by(County) %>%
  summarise(Correlation = cor(AvgHousePrice, DrugRatePer10k))
print(county_wise_corr)

