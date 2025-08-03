library(tidyverse)

# Load datasets
towns_data <- read_csv("Cleaned Data/Towns.csv")
postcode_lsoa <- read_csv("ObtainedDataset/Postcode/Postcode to LSOA.csv") %>%
  select(lsoa11cd, pcds)

# Function to get LSOA code by matching prefix
find_lsoa_code <- function(short_pc) {
  matches <- postcode_lsoa$lsoa11cd[str_starts(postcode_lsoa$pcds, short_pc)]
  if(length(matches) == 0) return(NA_character_)
  return(matches[1])
}

# Apply function to each shortPostcode in towns_data
towns_with_lsoa <- towns_data %>%
  mutate(
    LSOA_code = map_chr(shortPostcode, find_lsoa_code)
  ) %>%
  filter(!is.na(LSOA_code)) %>%
  select(`LSOA code` = LSOA_code, shortPostcode, Town, District, County,
         Population2020, Population2021, Population2022, Population2023, Population2024)

# Preview and save
View(towns_with_lsoa)

write_csv(towns_with_lsoa, "Cleaned Data/cleanedPostcode_LSOA.csv")
