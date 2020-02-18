library(tidyverse)
library(sf)
source("./data-raw/cleaning_scripts/utils.R")

# Create package data using a series of scripts ---------------------------

# Create country level data
source("./data-raw/cleaning_scripts/data_country.R")
# Create province level data
source("./data-raw/cleaning_scripts/data_province.R")
# Create district level data
source("./data-raw/cleaning_scripts/data_district.R")

# Create geodata
source("./data-raw/cleaning_scripts/geodata.R")


# Add labels --------------------------------------------------------------

# Read Indicator Description df
df <- readxl::read_excel("./data-raw/data_input/pak_indicator_descriptions/pak_indicator_descriptions.xlsx")
df <- janitor::clean_names(df)
df <- janitor::remove_empty(df, which = "rows")

# Select relevant columns & remove white spaces
df <- df %>%
  select(variable_name, indicator_definition, label) %>%
  mutate(
    variable_name = trimws(variable_name)
  )


# Add relevant descriptions to country df
pakeduc_country           <- pakeduc_country %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_country_weighted  <- pakeduc_country_weighted %>% left_join(df, by = c("indicator" = "variable_name"))

# Add relevant descriptions to district df
pakeduc_district           <- pakeduc_district %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_district_weighted  <- pakeduc_district_weighted %>% left_join(df, by = c("indicator" = "variable_name"))

# Add relevant descriptions to province df
pakeduc_province           <- pakeduc_province %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_province_weighted  <- pakeduc_province_weighted %>% left_join(df, by = c("indicator" = "variable_name"))





usethis::use_data(pakeduc_country,
                  pakeduc_country_weighted,
                  indicator_choices_country,
                  pakeduc_province,
                  pakeduc_province_weighted,
                  indicator_choices_province,
                  pakeduc_district,
                  pakeduc_district_weighted,
                  indicator_choices_district,
                  indicator_choices_country_inv,
                  pakgeo_district,
                  pakgeo_province,
                  overwrite = TRUE)
