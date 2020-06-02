source("./data-raw/cleaning_scripts/clean_district_level.R")

# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv")


# Clean district level data -----------------------------------------------
district_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_district_version1.dta"

pakeduc_district <- haven::read_stata(district_link) %>%
  clean_district_level(admin_lkup = admin_level_lkup)


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
                                "dist_key",
                                "dist_nm", 
                                "indicator",
                                "gender",
                                "point_estimate",
                                "dataset"),
                  year, province, dist_nm, indicator, gender) %>%
  group_by(province, dist_nm, indicator, gender) %>%
  arrange(indicator, province, dist_nm, gender, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()