
# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv")


# Clean province level data -----------------------------------------------
province_lkup <- admin_level_lkup %>%
  select(-district) %>%
  distinct()

pakeduc_province <- haven::read_stata("data-raw/data_input/pakeduc_data_province.dta") %>%
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
  #filter(!str_detect(names, "^egra_")) %>%
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

indicator_choices_province <- sort(unique(pakeduc_province$indicator)) 
indicator_choices_province <- prepare_indicator_choices(indicator_choices_province,
                                                        expected_choices = expected_choices,
                                                        choices_labels = choices_labels,
                                                        labels_order = labels_order)
                                                                         
# Create weighted dataset -------------------------------------------------

pakeduc_province_weighted <- pakeduc_province %>%
  filter(!is.na(point_estimate)) %>%
  mutate(
    inv_se = 1 / standard_error
  ) %>%
  group_by(year, province, indicator, gender) %>%
  mutate(
    inv_se_sum = sum(inv_se)
  ) %>%
  ungroup() %>%
  mutate(
    weight_var = inv_se / inv_se_sum,
    point_estimate_weighted = point_estimate * weight_var 
  ) %>%
  group_by(year, province, indicator, gender) %>%
  mutate(
    point_estimate_weighted = sum(point_estimate_weighted)
  ) %>%
  ungroup() %>%
  select(
    year,
    province_id,
    province,
    indicator,
    gender,
    point_estimate_weighted
  ) %>%
  distinct()

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
