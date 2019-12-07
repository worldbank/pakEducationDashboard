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


# Province level shapefiles -----------------------------------------------
pakgeo_province <- sf::read_sf("data-raw/pak_province_boundaries")

provincekey_lkup    <- read_csv("data-raw/admin_level_lkup.csv") %>% 
  mutate(
    province = str_to_lower(province),
    province = str_trim(province, side = "both"),
    district = str_to_lower(district),
    district = str_trim(district, side = "both"),
  )

pakgeo_province <- pakgeo_province %>%
  mutate(
    NAME_1     = str_to_lower(NAME_1),
    # TODO: Mention to Tony that we can't simply do a join as the province names in provincekey_lkup
    ## are different than the names in the shapes files
    ### (i.e khyber pakhtunkhwa in shape file, while "kp" in provincekey_lkup)
    province_id  = case_when(str_to_lower(NAME_1) == "northern areas" ~ 0,
                             str_to_lower(NAME_1) == "azad kashmir" ~ 0,
                             str_to_lower(NAME_1) == "f.c.t." ~ 0,
                             str_to_lower(NAME_1) == "punjab" ~ 1,
                             str_to_lower(NAME_1) == "sind" ~  2,
                             str_to_lower(NAME_1) == "baluchistan" ~ 3,
                             str_to_lower(NAME_1) == "n.w.f.p" ~ 4,
                             str_to_lower(NAME_1) == "f.a.t.a." ~ 4))%>%
  
  left_join(provincekey_lkup, by = "province_id") %>%
  # TODO: Consult with Tony,
  ## removing NAME_1 AND NAME_0 which were present in Shape files
  select(-NAME_1, -NAME_0)
# Save data ---------------------------------------------------------------

usethis::use_data(pakgeo_district,
                  pakgeo_province,
                  overwrite = TRUE)
