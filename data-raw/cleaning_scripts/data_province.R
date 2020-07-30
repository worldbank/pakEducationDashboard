
# Load admin_level_lkup ---------------------------------------------------

province_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv") %>%
  select(-district) %>%
  distinct()

# Clean province level data -----------------------------------------------
province_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_province_version1.dta" 

pakeduc_province <- haven::read_stata(province_link) %>%
  mutate(
    province = stringr::str_to_sentence(as.character(forcats::as_factor(province_tag)))
  ) %>%
  clean_raw_data(cols_to_remove = c( "country_tag",
                                     "province_tag",
                                     "district_tag",
                                     "country",
                                     "district"),
                 unique_keys    = c("identifier",
                                    "province",
                                    "year",
                                    "dataset"),
                 dimensions     = c("gender", 
                                    "wealth quintile", 
                                    "urban-rural"),
                 patterns       = c("_boys|boys_|_boys_|_girls|girls_|_girls_", 
                                    "_wq[1-5]|wq[1-5]_|_wq[1-5]_",
                                    "_urban|urban_|_urban_|_rural|rural_|_rural_")) %>%
  mutate(
    province = as.factor(province),
    province = forcats::fct_relevel(province, "Punjab", "Sindh", "Kp", "Balochistan", "Other areas")
  ) %>%
  dplyr::left_join(province_lkup)
                                                                         
# Create weighted dataset -------------------------------------------------


pakeduc_province_weighted <- pakeduc_province %>%
  create_weighted(selection = c("year",
                                "province_id",
                                "province",
                                "indicator",
                                "age_range",
                                "dimensions",
                                "dimension_levels",
                                "point_estimate",
                                "dataset"),
                  weighted_mix = "Weighted mix (Moving average, window = 3)",
                  year, province, indicator, dimensions, dimension_levels) %>%
  group_by(province, indicator, dimensions, dimension_levels) %>%
  arrange(indicator, province, dimensions, dimension_levels, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()
