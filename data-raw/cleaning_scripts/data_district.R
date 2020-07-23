
# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv") %>%
  mutate(
    district_merge = stringr::str_to_lower(district),
    district_merge = stringr::str_squish(district_merge)
  ) %>%
  select(-district)
  


# Clean district level data -----------------------------------------------
district_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_district_version1.dta"

pakeduc_district <- haven::read_stata(district_link) %>%
  mutate(
    district = stringr::str_to_title(as.character(forcats::as_factor(district))),
    district_merge = stringr::str_to_lower(district),
    district_merge = stringr::str_squish(district_merge)
  ) %>%
  clean_raw_data(cols_to_remove = c( "country_tag",
                                     "province_tag",
                                     "district_tag",
                                     "country",
                                     "province"),
                 unique_keys    = c("identifier",
                                    "district",
                                    "district_merge",
                                    "year",
                                    "dataset"),
                 dimensions     = c("gender", 
                                    "wealth quintile", 
                                    "urban-rural"),
                 patterns       = c("_boys|boys_|_boys_|_girls|girls_|_girls_", 
                                    "_wq[1-5]|wq[1-5]_|_wq[1-5]_",
                                    "_urban|urban_|_urban_|_rural|rural_|_rural_")) %>%
  dplyr::left_join(admin_level_lkup, by = c("district_merge" = "district_merge")) %>%
  select(-district_merge)


# Create weighted dataset -------------------------------------------------


pakeduc_district_weighted <- pakeduc_district %>%
  create_weighted(selection = c("year",
                                "province_id",
                                "province",
                                "identifier",
                                "district", 
                                "indicator",
                                "age_range",
                                "dimensions",
                                "dimension_levels",
                                "point_estimate",
                                "dataset"),
                  weighted_mix = "Weighted mix (Moving average, window = 3)",
                  year, province, district, indicator, dimensions, dimension_levels) %>%
  group_by(province, district, indicator, dimensions, dimension_levels) %>%
  arrange(indicator, province, district, dimensions, dimension_levels, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct() %>%
  mutate(
    identifier = as.character(identifier)
  )
