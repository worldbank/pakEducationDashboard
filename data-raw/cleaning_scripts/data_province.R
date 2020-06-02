source("./data-raw/cleaning_scripts/clean_province_level.R")

# Load admin_level_lkup ---------------------------------------------------

province_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv") %>%
  select(-district) %>%
  distinct()

# Clean province level data -----------------------------------------------
province_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_province_version1.dta" 

pakeduc_province <- haven::read_stata(province_link) %>%
  clean_province_level(province_lkup = province_lkup)

# indicator_choices_province <- sort(unique(pakeduc_province$indicator)) 
# indicator_choices_province <- prepare_indicator_choices(indicator_choices_province,
#                                                         expected_choices = expected_choices,
#                                                         choices_labels = choice_labels,
#                                                         labels_order = labels_order)
                                                                         
# Create weighted dataset -------------------------------------------------


pakeduc_province_weighted <- pakeduc_province %>%
  create_weighted(selection = c("year",
                                "province_id",
                                "province",
                                "indicator",
                                "gender",
                                "point_estimate",
                                "dataset"),
                  year, province, indicator, gender) %>%
  group_by(province, indicator, gender) %>%
  arrange(indicator, province, gender, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()
