library(tidyverse)
library(ggplot2)

# === Load Broadband Data ===
Broadband <- read_csv("Cleaned Data/cleanedBroadband.csv", show_col_types = FALSE)
glimpse(Broadband)
View(Broadband)

# === Filter by County ===
west_yorkshire <- Broadband %>%
  filter(County == "WEST YORKSHIRE")

south_yorkshire <- Broadband %>%
  filter(County == "SOUTH YORKSHIRE")

# === Boxplot: Download Speed by District ===

# West Yorkshire Plot
plot_west <- ggplot(west_yorkshire, aes(x = District, y = avg_download)) +
  geom_boxplot(fill = "burlywood1", outlier.color = "lightpink") +
  labs(
    title = "Average Download Speed by District in West Yorkshire",
    x = "District", y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# South Yorkshire Plot
plot_south <- ggplot(south_yorkshire, aes(x = District, y = avg_download)) +
  geom_boxplot(fill = "lightblue", outlier.color = "lightpink") +
  labs(
    title = "Average Download Speed by District in South Yorkshire",
    x = "District", y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display boxplots
print(plot_west)
print(plot_south)

# ======================================================================================= #

# === Barcharts: Average Download Speed by Town ===

broadband_town_avg <- Broadband %>%
  group_by(County, Town) %>%
  summarise(meanDownload = mean(avg_download, na.rm = TRUE), .groups = "drop")

# Filter town-wise data by county
west_yorkshire_town <- broadband_town_avg %>%
  filter(County == "WEST YORKSHIRE")

south_yorkshire_town <- broadband_town_avg %>%
  filter(County == "SOUTH YORKSHIRE")

# Bar chart: West Yorkshire
chart_west <- ggplot(west_yorkshire_town, aes(x = reorder(Town, meanDownload), y = meanDownload)) +
  geom_col(fill = "burlywood1") +
  coord_flip() +
  labs(
    title = "Average Download Speed by Town - West Yorkshire",
    x = "Town", y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

# Bar chart: South Yorkshire
chart_south <- ggplot(south_yorkshire_town, aes(x = reorder(Town, meanDownload), y = meanDownload)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Average Download Speed by Town - South Yorkshire",
    x = "Town", y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

# Display barcharts
print(chart_west)
print(chart_south)
