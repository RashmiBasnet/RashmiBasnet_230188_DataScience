# --- Libraries ---
library(tidyverse)
library(ggplot2)
library(dplyr)

# --- Load datasets ---
broadband_data <- read_csv("Cleaned Data/cleanedBroadband.csv")
crime_data <- read_csv("Cleaned Data/cleanedCrime.csv")
postcode_data <- read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# --- Glimpse to confirm structure ---
glimpse(broadband_data)
glimpse(crime_data)
glimpse(postcode_data)

# --- Add postcode info to crime dataset ---
crime_with_pc <- crime_data %>%
  left_join(postcode_data %>% select(`LSOA code`, shortPostcode), by = "LSOA code") %>%
  filter(!is.na(shortPostcode))

# --- Filter for drug-related crimes in 2023 ---
drug_offenses <- crime_with_pc %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  count(shortPostcode, name = "DrugCases")

# --- Extract population and county info ---
postcode_summary <- postcode_data %>%
  select(shortPostcode, Population2023, County) %>%
  distinct()

# --- Compute drug crime rate per 10,000 residents ---
offense_rates <- drug_offenses %>%
  left_join(postcode_summary, by = "shortPostcode") %>%
  filter(!is.na(Population2023)) %>%
  mutate(RatePer10k = (DrugCases / Population2023) * 10000) %>%
  filter(County %in% c("WEST YORKSHIRE", "SOUTH YORKSHIRE"))

# --- Compute average download speed per postcode ---
broadband_summary <- broadband_data %>%
  filter(County %in% c("WEST YORKSHIRE", "SOUTH YORKSHIRE")) %>%
  group_by(short_pc, County) %>%
  summarise(AvgSpeed = mean(avg_download, na.rm = TRUE), .groups = "drop") %>%
  rename(shortPostcode = short_pc)  # 🔧 Rename to match offense_rates

# --- Combine broadband and crime data ---
combined_view <- broadband_summary %>%
  inner_join(offense_rates, by = c("shortPostcode", "County"))

# --- Plotting the relationship ---
crime_plot <- ggplot(combined_view, aes(x = RatePer10k, y = AvgSpeed, color = County)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1, linetype = "solid") +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#E76F51", "WEST YORKSHIRE" = "#2A9D8F")) +
  labs(
    title = "Impact of Drug Offense Rate on Broadband Speed (2023)",
    x = "Drug Offense Rate (per 10,000 People)",
    y = "Average Download Speed (Mbps)",
    color = "Region"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

print(crime_plot)

# --- Linear Regression Model with Interaction ---
model_fit <- lm(AvgSpeed ~ RatePer10k * County, data = combined_view)
cat("\n=== Linear Model Summary ===\n")
print(summary(model_fit))

# --- Overall Correlation ---
overall_corr <- cor(combined_view$AvgSpeed, combined_view$RatePer10k, use = "complete.obs")
cat("\nOverall Correlation:", round(overall_corr, 3), "\n")

# --- County-wise Correlation ---
countywise_corr <- combined_view %>%
  group_by(County) %>%
  summarise(Correlation = cor(AvgSpeed, RatePer10k, use = "complete.obs"), .groups = "drop")

cat("\nCorrelation by County:\n")
print(countywise_corr)

