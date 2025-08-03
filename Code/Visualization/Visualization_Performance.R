library(tidyverse)
library(ggplot2)

# Load dataset
performance_data <- read_csv("Cleaned Data/CleanedPerformance.csv")

# View dataset structure
glimpse(performance_data)
View(performance_data)

# ================= SOUTH YORKSHIRE BOXPLOT =================== #
south_data <- performance_data %>%
  filter(County == "SOUTH YORKSHIRE",
         Year == "2021-2022",
         !ATT8SCR %in% c("NE", "SUPP")) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

ggplot(south_data, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "#FF9999", color = "#7D3C98") +
  labs(
    title = "Distribution of Attainment 8 Scores - 2022",
    subtitle = "South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ================= WEST YORKSHIRE BOXPLOT =================== #
west_data <- performance_data %>%
  filter(County == "WEST YORKSHIRE",
         Year == "2021-2022",
         !ATT8SCR %in% c("NE", "SUPP")) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

ggplot(west_data, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "#AED6F1", color = "#2874A6") +
  labs(
    title = "Distribution of Attainment 8 Scores - 2022",
    subtitle = "West Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ================= LINE GRAPH FOR SCORE TRENDS =================== #
trend_data <- performance_data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"),
         !ATT8SCR %in% c("NE", "SUPP")) %>%
  mutate(
    ATT8SCR = as.numeric(ATT8SCR),
    Year = str_extract(Year, "\\d{4}-\\d{4}"),
    District = str_to_title(District),
    County = str_to_title(County),
    Group = paste(District, "(", County, ")")
  )

# Define distinct color palette
palette_colors <- c(
  "#E5989B", "#FFB4A2", "#CDB4DB", "#A2D2FF",
  "#FFCDB2", "#B5EAD7", "#9BF6FF", "#FFADAD", "#CAFFBF"
)
group_labels <- unique(trend_data$Group)
names(palette_colors) <- group_labels[1:length(palette_colors)]

# Plot trend lines
ggplot(trend_data, aes(x = Year, y = ATT8SCR, group = Group, color = Group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  facet_wrap(~ County, scales = "free_x") +
  labs(
    title = "Yearly Trend of Attainment 8 Scores",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "District (County)"
  ) +
  scale_color_manual(values = palette_colors) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )

