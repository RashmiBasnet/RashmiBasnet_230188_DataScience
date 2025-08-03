library(tidyverse)

# === Set working directories and year-specific folders ===
performance_dirs <- list(
  "2021-2022" = "ObtainedDataset/Performance/Performance2021-2022/2021-2022",
  "2022-2023" = "ObtainedDataset/Performance/Performance2022-2023/2022-2023",
  "2023-2024" = "ObtainedDataset/Performance/Performance2023-2024/2023-2024"
)

postcode_prefixes <- c("BD", "DN", "HD", "HG", "HU", "HX", "LS", "S", "WF", "YO")
valid_counties <- c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

# === Load reference town data ===
town_reference <- read_csv("Cleaned Data/Towns.csv", show_col_types = FALSE) %>%
  mutate(
    shortPostcode = toupper(str_trim(shortPostcode)),
    County = toupper(str_trim(County))
  ) %>%
  filter(County %in% valid_counties)

# === Define a function to extract relevant school data from a folder ===
extract_performance_data <- function(folder, label) {
  file_path <- list.files(folder, pattern = "(?i)england_ks4final.*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(file_path) == 0) return(tibble())
  
  read_csv(file_path[1], show_col_types = FALSE) %>%
    select(any_of(c("LEA", "URN", "SCHNAME", "TOWN", "PCODE", "ATT8SCR"))) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    mutate(
      shortPostcode = str_extract(PCODE, "^[A-Z0-9]+") %>% str_trim() %>% toupper(),
      Year = label
    ) %>%
    filter(shortPostcode %in% postcode_prefixes | str_starts(shortPostcode, paste(postcode_prefixes, collapse = "|")))
}

# === Process all years' data and combine ===
performance_data <- imap_dfr(performance_dirs, extract_performance_data)

# === Join with town info and finalize ===
performance_with_location <- performance_data %>%
  inner_join(town_reference, by = "shortPostcode") %>%
  select(LEA, URN, SCHNAME, PCODE, ATT8SCR, Year, shortPostcode, Town, District, County) %>%
  filter(County %in% valid_counties)

# === Export result ===
if (nrow(performance_with_location) > 0) {
  write_csv(performance_with_location, "Cleaned Data/cleanedPerformance.csv")
  View(performance_with_location)
} else {
  message("🚫 No matching performance data after filtering.")
}
