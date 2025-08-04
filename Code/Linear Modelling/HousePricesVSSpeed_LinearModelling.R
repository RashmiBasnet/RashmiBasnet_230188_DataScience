library(tidyverse)
library(dplyr)

# --- Load datasets ---
house_data <- read_csv("Cleaned Data/cleanedHousePrices.csv")
broadband_data <- read_csv("Cleaned Data/cleanedBroadband.csv")

# --- Rename short_pc to shortPostcode for consistency ---
broadband_data <- broadband_data %>%
  rename(shortPostcode = short_pc)

# --- Define target counties ---
target_counties <- c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

# --- Compute average house price per postcode and county ---
house_summary <- house_data %>%
  filter(County %in% target_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# --- Compute average download speed per postcode and county ---
broadband_summary <- broadband_data %>%
  filter(County %in% target_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(AverageSpeed = mean(avg_download, na.rm = TRUE), .groups = "drop")

# --- Merge both datasets ---
merged_stats <- inner_join(house_summary, broadband_summary, by = c("shortPostcode", "County")) %>%
  drop_na()

# --- Plot: House price vs download speed with customized colors ---
ggplot(merged_stats, aes(x = AverageSpeed, y = AveragePrice, color = County)) +
  geom_point(alpha = 0.7, size = 2.8) +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  scale_y_log10() +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#1f77b4",  # deep blue
                                "WEST YORKSHIRE" = "#ff7f0e")) + # orange
  labs(
    title = "Average House Price vs Download Speed (2023)",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  theme_minimal(base_size = 15)

# --- Linear models and correlation per county ---
for (county_name in target_counties) {
  county_data <- merged_stats %>% filter(County == county_name)
  model <- lm(AveragePrice ~ AverageSpeed, data = county_data)
  cat("\nLinear Model Summary for", county_name, ":\n")
  print(summary(model))
  cat("Correlation:", round(cor(county_data$AveragePrice, county_data$AverageSpeed), 3), "\n")
}

