source("./data-raw/cleaning_scripts/clean_country_level.R")

# Clean country level data ------------------------------------------------
country_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_country_version1.dta"

pakeduc_country <- haven::read_stata(country_link)  %>%
  clean_country_level()
        
indicator_choices_country_inv <- names(indicator_choices_country)
names(indicator_choices_country_inv) <- unname(indicator_choices_country)

assertthat::assert_that(all(sort(unique(pakeduc_country$indicator)) %in% sort(indicator_choices_country) ))

# Create weighted dataset -------------------------------------------------

pakeduc_country_weighted <- pakeduc_country %>%
  create_weighted(selection = c("year",
                                "country",
                                "indicator",
                                "gender",
                                "point_estimate",
                                "dataset"),
                  year, indicator, gender) %>%
  group_by(country, indicator, gender) %>%
  arrange(indicator, country, gender, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()
  
  

