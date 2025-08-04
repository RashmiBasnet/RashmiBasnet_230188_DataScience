# --- Load Libraries ---
library(tidyverse)
library(ggplot2)

# --- Import Datasets ---
performance_df <- read_csv("Cleaned Data/cleanedPerformance.csv")
crime_df <- read_csv("Cleaned Data/cleanedCrime.csv")
lsoa_df <- read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# --- Preprocess School Data ---
performance_cleaned <- performance_df %>%
  mutate(
    Year = as.numeric(str_extract(Year, "\\d{4}")),
    ATT8 = as.numeric(na_if(ATT8SCR, "NE"))
  ) %>%
  group_by(shortPostcode, Year, County) %>%
  summarise(Avg_ATT8 = mean(ATT8, na.rm = TRUE), .groups = "drop") %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# --- Prepare Crime Data: Drug Offenses (2023) ---
drug_crimes <- crime_df %>%
  filter(`Crime type` == "Drugs", Year == 2023) %>%
  group_by(`LSOA code`, County, District) %>%
  summarise(TotalDrugs = sum(Crime_Count), .groups = "drop")

# --- Merge with Population and Calculate Crime Rate ---
crime_rates <- drug_crimes %>%
  left_join(lsoa_df, by = c("LSOA code", "County", "District")) %>%
  filter(!is.na(Population2023)) %>%
  mutate(DrugRate10k = (TotalDrugs / Population2023) * 10000) %>%
  group_by(shortPostcode, County) %>%
  summarise(DrugRate10k = mean(DrugRate10k, na.rm = TRUE), .groups = "drop") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# --- Combine Performance & Crime Rate Data ---
analysis_data <- performance_cleaned %>%
  inner_join(crime_rates, by = c("shortPostcode", "County"))

# --- Create Plot ---
ggplot(analysis_data, aes(x = Avg_ATT8, y = DrugRate10k, color = County)) +
  geom_point(alpha = 0.75, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, linetype = "solid") +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#F94144", "WEST YORKSHIRE" = "#577590")) +
  labs(
    title = "Correlation Between Performance and Drug Offense Rate (2023)",
    x = "Average Attainment 8 Score",
    y = "Drug Offenses per 10,000 People",
    color = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

# --- Correlation Calculations ---

# Overall
overall_corr <- cor(analysis_data$Avg_ATT8, analysis_data$DrugRate10k, use = "complete.obs")
cat("\nOverall Correlation:", round(overall_corr, 3), "\n")

# South Yorkshire
sy_corr <- analysis_data %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  summarise(Correlation = cor(Avg_ATT8, DrugRate10k, use = "complete.obs")) %>%
  pull(Correlation)

# West Yorkshire
wy_corr <- analysis_data %>%
  filter(County == "WEST YORKSHIRE") %>%
  summarise(Correlation = cor(Avg_ATT8, DrugRate10k, use = "complete.obs")) %>%
  pull(Correlation)

cat("South Yorkshire Correlation:", round(sy_corr, 3), "\n")
cat("West Yorkshire Correlation:", round(wy_corr, 3), "\n")

