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
