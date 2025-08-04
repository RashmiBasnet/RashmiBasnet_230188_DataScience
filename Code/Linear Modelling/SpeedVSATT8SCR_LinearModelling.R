library(tidyverse)
library(ggpubr)

# --- Load cleaned datasets ---
broadband_df <- read_csv("Cleaned Data/cleanedBroadband.csv", show_col_types = FALSE)
performance_df <- read_csv("Cleaned Data/cleanedPerformance.csv", show_col_types = FALSE)
glimpse(broadband_df)
glimpse(performance_df)

# --- Clean year and convert score column ---
performance_df <- performance_df %>%
  mutate(
    Year = as.integer(str_extract(Year, "\\d{4}")),
    AttainmentScore = as.numeric(ATT8SCR)
  )

# --- Check available counties ---
performance_df %>% count(County)

# --- Find the most recent year with data ---
recent_year <- performance_df %>%
  filter(!is.na(Year)) %>%
  arrange(desc(Year)) %>%
  distinct(Year) %>%
  slice(1) %>%
  pull()

cat("Most recent academic year:", recent_year, "\n")

# --- Average Attainment 8 Score per postcode ---
attainment_summary <- performance_df %>%
  filter(Year == recent_year) %>%
  group_by(shortPostcode, County) %>%
  summarise(MeanAttainment = mean(AttainmentScore, na.rm = TRUE), .groups = "drop")

# Rename short_pc to shortPostcode in broadband_df for consistent joining
broadband_df <- broadband_df %>%
  rename(shortPostcode = short_pc)

# Now join works correctly
combined_data <- inner_join(broadband_df, attainment_summary, by = c("shortPostcode", "County"))

cat("Combined dataset size:", dim(combined_data), "\n")

# Plot with correct broadband avg_download column name
edu_vs_net_plot <- ggplot(combined_data, aes(x = MeanAttainment, y = avg_download, color = County)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(
    title = paste("Download Speed vs Attainment 8 Score (", recent_year, ")", sep = ""),
    x = "Average Attainment 8 Score",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

print(edu_vs_net_plot)

# Linear model with interaction (using correct broadband column name)
interaction_model <- lm(avg_download ~ MeanAttainment * County, data = combined_data)
cat("Linear model with interaction:\n")
print(summary(interaction_model))

# Correlation calculations with correct broadband column name
total_correlation <- cor(combined_data$avg_download, combined_data$MeanAttainment, use = "complete.obs")
cat("Overall correlation:", round(total_correlation, 3), "\n")

countywise_correlation <- combined_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avg_download, MeanAttainment, use = "complete.obs")) %>%
  arrange(desc(Correlation))

cat("Correlation per County:\n")
print(countywise_correlation)

