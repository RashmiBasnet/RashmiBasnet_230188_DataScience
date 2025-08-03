library(tidyverse)

# Step 1: Prepare years and folder
years <- 2021:2024
folder <- "ObtainedDataset/HousePrice"
column_names <- c("TransactionID", "Price", "DateOfTransfer", "Postcode", "PropertyType",
                  "OldOrNew", "Duration", "PAON", "SAON", "Street", "Locality",
                  "Town", "District", "County", "PPDCategoryType", "RecordStatus")

# Step 2: Initialize empty tibble
combined_data <- tibble()

# Step 3: Read and merge files one by one
for (yr in years) {
  path <- file.path(folder, paste0("HousePrice", yr, ".csv"))
  
  if (file.exists(path)) {
    data <- read_csv(path, show_col_types = FALSE)
    colnames(data) <- column_names
    combined_data <- bind_rows(combined_data, data)
  }
}

# Step 4: Filter, mutate and clean
clean_house_prices <- combined_data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = str_sub(DateOfTransfer, 1, 4),
    Price = as.numeric(Price)
  ) %>%
  select(Postcode, shortPostcode, Year, PAON, Price, Town, District, County) %>%
  distinct() %>%
  drop_na()

# Step 5: Save cleaned data
dir.create("Cleaned Data", showWarnings = FALSE, recursive = TRUE)
write_csv(clean_house_prices, "Cleaned Data/cleanedHousePrices.csv")

# View result
View(clean_house_prices)
