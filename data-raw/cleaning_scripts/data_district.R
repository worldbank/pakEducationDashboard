source("./data-raw/cleaning_scripts/clean_district_level.R")

# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv") %>%
  mutate(
    district_merge = stringr::str_to_lower(district),
    district_merge = stringr::str_squish(district_merge)
  ) %>%
  select(-district)
  


# Clean district level data -----------------------------------------------
# district_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_district_version1.dta"
district_link <- "data-raw/data_input/DD_Pak_district_level_version2.dta"

pakeduc_district <- haven::read_stata(district_link) %>%
  mutate(
    district = stringr::str_to_title(as.character(forcats::as_factor(district))),
    district_merge = stringr::str_to_lower(district),
    district_merge = stringr::str_squish(district_merge)
  ) %>%
  clean_district_level2() %>%
  dplyr::left_join(admin_level_lkup, by = c("district_merge" = "district_merge")) %>%
  select(-district_merge)


# indicator_choices_district <- sort(unique(pakeduc_district$indicator)) 
# indicator_choices_district <- prepare_indicator_choices(indicator_choices_district,
#                                                         expected_choices = expected_choices_district,
#                                                         choices_labels = choice_labels_district,
#                                                         labels_order = labels_order_district)
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
