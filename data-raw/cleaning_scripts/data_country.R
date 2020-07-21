source("./data-raw/cleaning_scripts/clean_country_level.R")

# Clean country level data ------------------------------------------------
#country_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_country_version1.dta"
country_link <- "data-raw/data_input/DD_Pak_country_level_version2.dta"

pakeduc_country <- haven::read_stata(country_link)  %>%
  clean_country_level2()
        
indicator_choices_country_inv <- names(indicator_choices_country)
names(indicator_choices_country_inv) <- unname(indicator_choices_country)

assertthat::assert_that(all(sort(unique(pakeduc_country$indicator)) %in% sort(unname(indicator_choices_country)) ))

# Create weighted dataset -------------------------------------------------

pakeduc_country_weighted <- pakeduc_country %>%
  create_weighted(selection = c("year",
                                "indicator",
                                "age_range",
                                "dimensions",
                                "dimension_levels",
                                "point_estimate",
                                "dataset"),
                  weighted_mix = "Weighted mix (Moving average, window = 3)",
                  year, indicator, dimensions, dimension_levels) %>%
  group_by(indicator, dimensions, dimension_levels) %>%
  arrange(indicator, dimensions, dimension_levels, year) %>%
  mutate(
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='right', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()
  
  

