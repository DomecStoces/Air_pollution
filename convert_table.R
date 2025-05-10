library(tidyverse)

# Check which columns are metadata and which are species
meta_cols <- c("Date", "Time.period", "Year", "Woody.species", "Locality",
               "Week.period", "Temperature", "Immission", "T", "Precipitation", "Wind")
meta_cols <- as.character(meta_cols)

# Reshape the data: gather species columns into key-value pairs
df_long <- final_R %>%
  pivot_longer(cols = -all_of(meta_cols),
               names_to = "Species",
               values_to = "Number") %>%
  filter(Number > 0)

# Preview result
print(head(df_long))
