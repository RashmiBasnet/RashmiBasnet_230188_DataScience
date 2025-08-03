library(tidyverse)
library(ggplot2)



# Load Data
Crime = read_csv("Cleaned Data/cleanedCrime.csv")
Crime <- Crime %>% mutate(Month = as.integer(Month))
LSOA = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# === Boxplot for Drug Offense Rate by District === #
DrugCrime = Crime %>% filter(`Crime type` == "Drugs")
DrugCrime_Agg = DrugCrime %>%
  group_by(County, District, Year) %>%
  summarise(Total_Drug_Crime = sum(Crime_Count), .groups = "drop")

Population_Long = LSOA %>%
  pivot_longer(cols = starts_with("Population"),
               names_to = "Year", names_prefix = "Population",
               values_to = "Population") %>%
  mutate(Year = as.numeric(Year))

Population_Agg = Population_Long %>%
  group_by(County, District, Year) %>%
  summarise(Avg_Pop = mean(Population, na.rm = TRUE), .groups = "drop")

DrugCrime_with_Rate = DrugCrime_Agg %>%
  left_join(Population_Agg, by = c("County", "District", "Year")) %>%
  filter(!is.na(Avg_Pop), Avg_Pop > 0) %>%
  mutate(Drug_Offense_Rate = (Total_Drug_Crime / Avg_Pop) * 1000)

# South Yorkshire Plot
DrugCrime_with_Rate %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = Drug_Offense_Rate)) +
  geom_boxplot(fill = "#7FB3D5", alpha = 0.8, outlier.color = "#D98880") +
  labs(title = "Drug Offense Rate per District in South Yorkshire",
       x = "District", y = "Rate per 1000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# West Yorkshire Plot
DrugCrime_with_Rate %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = Drug_Offense_Rate)) +
  geom_boxplot(fill = "#F7DC6F", alpha = 0.8, outlier.color = "#D98880") +
  labs(title = "Drug Offense Rate per District in West Yorkshire",
       x = "District", y = "Rate per 1000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === Radar Chart for Vehicle Crime === #
library(fmsb)

vehicle_crime_data = Crime %>%
  filter(`Crime type` == "Vehicle crime", Year == 2023, Month == 2, County == "WEST YORKSHIRE") %>%
  count(District, name = "Count")

max_val = max(vehicle_crime_data$Count)
radar_df = vehicle_crime_data %>%
  column_to_rownames("District") %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()
radar_df = rbind(rep(max_val, ncol(radar_df)), rep(0, ncol(radar_df)), radar_df)

radarchart(radar_df,
           axistype = 1,
           pcol = "#45B39D",
           pfcol = rgb(0.27, 0.71, 0.60, 0.5),
           plwd = 2,
           cglcol = "grey50",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(0, max_val, length.out = 5),
           title = "Vehicle Crime Rate by District (West Yorkshire, Feb 2023)")

# === Pie Chart for Robbery Rate === #
Robbery_data <- Crime %>%
  filter(`Crime type` == "Robbery", County == "SOUTH YORKSHIRE",
         Year == 2023, Month == 5)

Robbery_with_pop <- Robbery_data %>%
  left_join(LSOA %>% select(`LSOA code`, District, Population2023),
            by = "LSOA code") %>%
  filter(!is.na(Population2023))

if ("District.x" %in% colnames(Robbery_with_pop)) {
  Robbery_with_pop <- Robbery_with_pop %>%
    mutate(District = coalesce(District.x, District.y)) %>%
    select(-District.x, -District.y)
}

Robbery_rate_district <- Robbery_with_pop %>%
  group_by(District) %>%
  summarise(
    total_robbery = sum(Crime_Count),
    total_pop = sum(Population2023)
  ) %>%
  mutate(rate_per_1000 = (total_robbery / total_pop) * 1000)

ggplot(Robbery_rate_district, aes(x = "", y = rate_per_1000, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Robbery Rate (May 2023) - South Yorkshire") +
  scale_fill_brewer(palette = "Set3") +
  theme_void()

# === Line Chart for Drug Offense Rate per 10,000 People === #
DrugCrimes <- Crime %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(County, Year) %>%
  summarise(Total_Drug_Crime = sum(Crime_Count), .groups = "drop")

Population_Long <- LSOA %>%
  pivot_longer(cols = starts_with("Population"),
               names_to = "Year", names_prefix = "Population",
               values_to = "Population") %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(County, Year) %>%
  summarise(Total_Pop = sum(Population, na.rm = TRUE), .groups = "drop")

DrugCrime_with_Pop <- DrugCrimes %>%
  left_join(Population_Long, by = c("County", "Year")) %>%
  filter(!is.na(Total_Pop), Total_Pop > 0) %>%
  mutate(Drug_Offense_Rate_per_10000 = (Total_Drug_Crime / Total_Pop) * 10000)

ggplot(DrugCrime_with_Pop, aes(x = Year, y = Drug_Offense_Rate_per_10000, color = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(DrugCrime_with_Pop$Year)) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#5DADE2", "WEST YORKSHIRE" = "#AF7AC5")) +
  labs(
    title = "Drug Offense Rate per 10,000 People by County",
    x = "Year",
    y = "Rate per 10,000 People",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
