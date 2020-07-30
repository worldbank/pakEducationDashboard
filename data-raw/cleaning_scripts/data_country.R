
# Clean country level data ------------------------------------------------
country_link <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_pe_country_version1.dta"

pakeduc_country <- haven::read_stata(country_link)  %>%
  clean_raw_data(cols_to_remove = c( "country_tag",
                                     "province_tag",
                                     "district_tag",
                                     "country",
                                     "province", 
                                     "district"),
                 unique_keys    = c("identifier",
                                    "year",
                                    "dataset"),
                 dimensions     = c("gender", 
                                    "wealth quintile", 
                                    "urban-rural"),
                 patterns       = c("_boys|boys_|_boys_|_girls|girls_|_girls_", 
                                    "_wq[1-5]|wq[1-5]_|_wq[1-5]_",
                                    "_urban|urban_|_urban_|_rural|rural_|_rural_"))
        
indicator_choices_country_inv <- names(indicator_choices_country)
names(indicator_choices_country_inv) <- unname(indicator_choices_country)

assertthat::assert_that(all(sort(unique(pakeduc_country$indicator)) %in% sort(unname(unlist(indicator_choices_country)))))

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
  
  

