library(tidyverse)

# === Load Cleaned House Price Data ===
house_data <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
glimpse(house_data)

# === Average Price Trends Over Time (Line Plot) ===
avg_price_trend <- house_data %>%
  group_by(County, Year) %>%
  summarise(mean_price = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(avg_price_trend, aes(x = Year, y = mean_price, color = County, group = County)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  labs(
    title = "Trends in Average House Prices by County (2021–2024)",
    x = "Year",
    y = "Avg. House Price (£)",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "blue",  # Dark Brown
                                "WEST YORKSHIRE" = "brown"))  # Dark Green
  theme_classic() +
  theme(legend.position = "right")


# === Bar Plot: Average Prices in 2023 ===
  house_2023_county <- house_data %>%
    filter(Year == 2023) %>%
    group_by(County) %>%
    summarise(avg_2023 = mean(Price, na.rm = TRUE), .groups = "drop")
  
  ggplot(house_2023_county, aes(x = reorder(County, avg_2023), y = avg_2023, fill = County)) +
    geom_col(width = 0.6) +
    labs(
      title = "County-wise Average House Prices (2023)",
      subtitle = "Comparison between South and West Yorkshire",
      x = "County",
      y = "Average Price (£)",
      fill = "County"
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("WEST YORKSHIRE" = "#ffcc99", "SOUTH YORKSHIRE" = "#99ccff")) +
    theme_light() +
    theme(legend.position = "none")  # optional: hide legend if counties are already labeled
  

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

