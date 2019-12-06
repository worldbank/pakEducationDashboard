library(tidyverse)
library(sf)


# Distrist level shapefiles -----------------------------------------------

pakgeo_district <- sf::read_sf("data-raw/pak_district_boundaries")

distkey_lkup <- read_csv("data-raw/distkey_lkup.csv") %>%
  mutate(
    province = str_to_lower(province),
    province = str_trim(province, side = "both"),
    district = str_to_lower(district),
    district = str_trim(district, side = "both"),
  )

pakgeo_district <- pakgeo_district %>%
  mutate(
    PROVINCE = str_to_lower(PROVINCE),
    DISTRICT = str_to_lower(DISTRICT)
  ) %>%
  left_join(distkey_lkup, by = c("PROVINCE" = "province", "DISTRICT" = "district"))


# Save data ---------------------------------------------------------------

usethis::use_data(pakgeo_district,
                  overwrite = TRUE)