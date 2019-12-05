## code to prepare `data` dataset goes here
library(tidyverse)


# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/admin_level_lkup.csv")


# Clean country level data ------------------------------------------------

pakeduc_country <- haven::read_stata("data-raw/pakeduc_data_country.dta")  %>%
  pivot_longer(cols = -c("country", "year", "dataset"), 
               names_to = "names", 
               values_to = "values") %>%
  mutate(
    indicator = str_replace_all(names, "_se$", ""),
    indicator = str_replace_all(indicator, "boys_|girls_", ""),
    gender = if_else(!str_detect(names, "boys|girls"), "Both", NA_character_),
    gender = if_else(str_detect(names, "boys"), "Boy", gender),
    gender = if_else(str_detect(names, "girls"), "Girl", gender),
    measurement = if_else(str_detect(names, "_se$"), "standard_error", "point_estimate")
  ) %>%
  select(-names) %>%
  distinct() %>%
  pivot_wider(names_from = "measurement", values_from = "values")


# Clean province level data -----------------------------------------------
province_lkup <- admin_level_lkup %>%
  select(-district) %>%
  distinct()

pakeduc_province <- haven::read_stata("data-raw/pakeduc_data_province.dta") %>%
  filter(!is.na(province)) %>%
  mutate(
    province_id = as.numeric(province)
    ) %>%
  select(-province) %>%
  left_join(province_lkup) %>%
  mutate(
    province = if_else(province_id == 5, "Unnamed", province)
  ) %>%
  pivot_longer(cols = -c("province_id", "province", "year", "dataset"), 
               names_to = "names", 
               values_to = "values") %>%
  mutate(
    indicator = str_replace_all(names, "_se$", ""),
    indicator = str_replace_all(indicator, "boys_|girls_", ""),
    gender = if_else(!str_detect(names, "boys|girls"), "Both", NA_character_),
    gender = if_else(str_detect(names, "boys"), "Boy", gender),
    gender = if_else(str_detect(names, "girls"), "Girl", gender),
    measurement = if_else(str_detect(names, "_se$"), "standard_error", "point_estimate")
  ) %>%
  select(-names) %>%
  distinct() %>%
  pivot_wider(names_from = "measurement", values_from = "values")

  

# Clean district level data -----------------------------------------------
pakeduc_district <- haven::read_stata("data-raw/pakeduc_data_district.dta") 

pakeduc_district <- pakeduc_district %>%
  mutate(
    dist_nm = stringr::str_to_title(dist_nm)
  ) %>%
  left_join(admin_level_lkup, by = c("dist_nm" = "district")) %>%
  pivot_longer(cols = -c("province_id", "province", "dist_key", "dist_nm", "year", "dataset"), 
               names_to = "names", 
               values_to = "values") %>%
  mutate(
    indicator = str_replace_all(names, "_se$", ""),
    indicator = str_replace_all(indicator, "boys_|girls_", ""),
    gender = if_else(!str_detect(names, "boys|girls"), "Both", NA_character_),
    gender = if_else(str_detect(names, "boys"), "Boy", gender),
    gender = if_else(str_detect(names, "girls"), "Girl", gender),
    measurement = if_else(str_detect(names, "_se$"), "standard_error", "point_estimate")
    ) %>%
  select(-names) %>%
  distinct() %>%
  pivot_wider(names_from = "measurement", values_from = "values")


# Save data ---------------------------------------------------------------

usethis::use_data(pakeduc_district,
                  pakeduc_province,
                  pakeduc_country,
                  overwrite = TRUE)


# # Code to generate the admin_level_lkup
# aser <- haven::read_stata("../Platform - Pak/ASER/aser_panel.dta")        
# admin_level_lkup <- aser %>%
#   select(district = dist_nm, province = province) %>%
#   distinct() %>%
#   mutate(
#     province_id = as.numeric(province),
#     province = as.character(province),
#     province = recode(province,
#                       "0" = "Other areas", 
#                       "1" = "Punjab", 
#                       "2" = "Sindh", 
#                       "3" = "Balochistan",
#                       "4" = "KP"),
#     district = stringr::str_to_title(district)
#   )
# 
# readr::write_csv(admin_level_lkup, "data-raw/admin_level_lkup.csv")
