library(tidyverse)

# === Load Cleaned House Price Data ===
house_data <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
glimpse(house_data)

# === Average Price Trends Over Time (Line Plot) ===
avg_price_trend <- house_data %>%
  group_by(District, Year, County) %>%
  summarise(mean_price = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(avg_price_trend, aes(x = District, y = mean_price, color = factor(Year), group = Year)) +
  geom_line(linewidth = 1.1, alpha = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~County, scales = "free_x") +
  labs(
    title = "Trends in Average House Prices (2021–2024)",
    subtitle = "Comparison by District across Yorkshire Counties",
    x = "District",
    y = "Avg. House Price (£)",
    color = "Year"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("2021" = "#a6cee3", "2022" = "#1f78b4", "2023" = "#b2df8a", "2024" = "#33a02c")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# === Bar Plot: Average Prices in 2023 ===
house_2023 <- house_data %>%
  filter(Year == 2023) %>%
  group_by(District, County) %>%
  summarise(avg_2023 = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(house_2023, aes(x = reorder(District, avg_2023), y = avg_2023, fill = County)) +
  geom_col(width = 0.7, position = position_dodge()) +
  coord_flip() +
  labs(
    title = "District-wise Average House Prices (2023)",
    subtitle = "Across South and West Yorkshire",
    x = "District",
    y = "Average Price (£)",
    fill = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("WEST YORKSHIRE" = "#ffcc99", "SOUTH YORKSHIRE" = "#99ccff")) +
  theme_light() +
  theme(legend.position = "top")

# === Boxplot: Distribution of House Prices by District ===
ggplot(house_data, aes(x = District, y = Price, fill = County)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "pink", outlier.color = "gray30", alpha = 0.6) +
  facet_wrap(~County, scales = "free_x") +
  labs(
    title = "Distribution of House Prices by District",
    x = "District",
    y = "Price (£)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("WEST YORKSHIRE" = "#d4a373", "SOUTH YORKSHIRE" = "#8ecae6")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

