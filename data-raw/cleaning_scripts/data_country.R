
# Clean country level data ------------------------------------------------

pakeduc_country <- haven::read_stata("data-raw/data_input/pakeduc_data_country.dta")  %>%
  pivot_longer(cols = -c("country", "year", "dataset"), 
               names_to = "names", 
               values_to = "values") %>%
  # filter(!str_detect(names, "^egra_")) %>%
  mutate(
    indicator = str_replace_all(names, "_se$", ""),
    indicator = str_replace_all(indicator, "_boys|_girls", ""),
    gender = if_else(!str_detect(names, "boys|girls"), "Both", NA_character_),
    gender = if_else(str_detect(names, "boys"), "Boy", gender),
    gender = if_else(str_detect(names, "girls"), "Girl", gender),
    measurement = if_else(str_detect(names, "_se$"), "standard_error", "point_estimate")
  ) %>%
  select(-names) %>%
  distinct() %>%
  pivot_wider(names_from = "measurement", values_from = "values")

indicator_choices_country <- sort(unique(pakeduc_country$indicator)) 
indicator_choices_country <- prepare_indicator_choices(indicator_choices_country,
                                                       expected_choices = expected_choices,
                                                       choices_labels = choices_labels,
                                                       labels_order = labels_order)
        
indicator_choices_country_inv <- names(indicator_choices_country)
names(indicator_choices_country_inv) <- unname(indicator_choices_country)


# Create weighted dataset -------------------------------------------------

pakeduc_country_weighted <- pakeduc_country %>%
  filter(!is.na(point_estimate)) %>%
  mutate(
    inv_se = 1 / standard_error
  ) %>%
  group_by(year, indicator, gender) %>%
  mutate(
    inv_se_sum = sum(inv_se)
  ) %>%
  ungroup() %>%
  mutate(
    weight_var = inv_se / inv_se_sum,
    point_estimate_weighted = point_estimate * weight_var 
  ) %>%
  group_by(year, indicator, gender) %>%
  mutate(
    point_estimate_weighted = sum(point_estimate_weighted)
  ) %>%
  ungroup() %>%
  select(
    year,
    country,
    indicator,
    gender,
    point_estimate_weighted
  ) %>%
  distinct()
  

