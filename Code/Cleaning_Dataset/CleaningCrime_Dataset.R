library(tidyverse)
library(data.table)
library(stringr)

# Load cleaned postcode–LSOA data, focusing on South and West Yorkshire
lsoa_ref <- read_csv("Cleaned Data/cleanedPostcode_LSOA.csv", show_col_types = FALSE) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  distinct(`LSOA code`, .keep_all = TRUE)

# Locate all crime data files recursively from the specified folder
crime_sources <- dir("ObtainedDataset/Crime_Dataset", pattern = "-street\\.csv$", full.names = TRUE, recursive = TRUE)

# Define a function to load and tag each crime file with its year and month
extract_crime_data <- function(filepath) {
  date_info <- str_match(filepath, "(\\d{4})-(\\d{2})")
  year_val <- as.integer(date_info[2])
  month_val <- date_info[3]
  
  fread(filepath, select = c("Crime ID", "LSOA code", "Crime type")) %>%
    mutate(Year = year_val, Month = month_val)
}

# Combine all crime data into one table
crimes_all <- map_dfr(crime_sources, extract_crime_data)

# Summarize number of crimes per LSOA, year, month, and crime type
summary_crimes <- crimes_all %>%
  filter(!is.na(`LSOA code`) & `LSOA code` != "") %>%
  count(`LSOA code`, Year, Month, `Crime type`, name = "Crime_Count")

# Join with reference data to bring in District and County info
cleaned_output <- summary_crimes %>%
  left_join(lsoa_ref, by = "LSOA code") %>%
  filter(!is.na(County)) %>%
  select(`LSOA code`, County, District, Year, Month, `Crime type`, Crime_Count)

# View or write result
View(cleaned_output)
write_csv(cleaned_output, "Cleaned Data/cleanedCrime.csv")
