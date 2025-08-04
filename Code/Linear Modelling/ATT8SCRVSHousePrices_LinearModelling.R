# --- Load Required Packages ---
library(tidyverse)
library(ggplot2)

# --- Read Cleaned Datasets ---
house_data <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
performance_data <- read_csv("Cleaned Data/cleanedPerformance.csv", show_col_types = FALSE)

# --- Clean Performance Data ---
performance_filtered <- performance_data %>%
  mutate(
    Year = as.numeric(str_extract(Year, "\\d{4}"))
  ) %>%
  filter(!ATT8SCR %in% c("SUPP", "NE")) %>%
  mutate(ATT8 = as.numeric(ATT8SCR)) %>%
  group_by(shortPostcode, Year) %>%
  summarise(Mean_ATT8 = mean(ATT8, na.rm = TRUE), .groups = "drop")

# --- Clean House Price Data ---
price_summary <- house_data %>%
  group_by(shortPostcode, Year, County) %>%
  summarise(Mean_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# --- Merge DataFrames ---
combined_df <- price_summary %>%
  inner_join(performance_filtered, by = c("shortPostcode", "Year")) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# --- Trim Outliers (Top 5% Prices) ---
clean_df <- combined_df %>%
  filter(Mean_Price <= quantile(Mean_Price, 0.95, na.rm = TRUE))

# --- Custom Color Palette ---
county_colors <- c("SOUTH YORKSHIRE" = "#0081A7", "WEST YORKSHIRE" = "#F07167")

# --- Plot: Attainment Score vs House Price (Log Scale) ---
ggplot(clean_df, aes(x = Mean_ATT8, y = Mean_Price, color = County)) +
  geom_point(alpha = 0.75, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = county_colors) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("B"))) +
  labs(
    title = "House Prices vs Performance Attainment in Yorkshire (2023)",
    x = "Average Attainment 8 Score",
    y = "Mean House Price (log scale)",
    color = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# --- Linear Regression Model: Log(Price) ~ Attainment ---
log_model <- lm(log10(Mean_Price) ~ Mean_ATT8, data = clean_df)
summary(log_model)

# --- Correlation Summary: County-wise and Overall ---
cor_table <- clean_df %>%
  group_by(County) %>%
  summarise(Correlation = cor(Mean_ATT8, Mean_Price, use = "complete.obs")) %>%
  bind_rows(tibble(
    County = "Overall",
    Correlation = cor(clean_df$Mean_ATT8, clean_df$Mean_Price, use = "complete.obs")
  )) %>%
  mutate(Correlation = round(Correlation, 3))

print(cor_table)
