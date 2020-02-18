
# Load admin_level_lkup ---------------------------------------------------

admin_level_lkup <- readr::read_csv("data-raw/data_input/admin_level_lkup.csv")


# Clean district level data -----------------------------------------------
pakeduc_district <- haven::read_stata("data-raw/data_input/pakeduc_data_district.dta") 

pakeduc_district <- pakeduc_district %>%
  mutate(
    dist_nm = stringr::str_to_title(dist_nm)
  ) %>%
  left_join(admin_level_lkup, by = c("dist_nm" = "district")) %>%
  pivot_longer(cols = -c("province_id", "province", "dist_key", "dist_nm", "year", "dataset"), 
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

indicator_choices_district <- sort(unique(pakeduc_district$indicator)) 
indicator_choices_district <- prepare_indicator_choices(indicator_choices_district,
                                                        expected_choices = expected_choices_district,
                                                        choices_labels = choice_labels_district,
                                                        labels_order = labels_order_district)
# Create weighted dataset -------------------------------------------------

pakeduc_district_weighted <- pakeduc_district %>%
  filter(!is.na(point_estimate)) %>%
  filter(!(dataset == "aser" & (str_detect(indicator, "^in_school") | str_detect(indicator, "^share_private")))) %>%
  mutate(
    inv_se = 1 / standard_error
  ) %>%
  group_by(year, province, dist_nm, indicator, gender) %>%
  mutate(
    inv_se_sum = sum(inv_se)
  ) %>%
  ungroup() %>%
  mutate(
    weight_var = inv_se / inv_se_sum,
    point_estimate = point_estimate * weight_var 
  ) %>%
  group_by(year, province, dist_nm, indicator, gender) %>%
  mutate(
    point_estimate = sum(point_estimate)
  ) %>%
  ungroup() %>%
  mutate(
    dataset = "Weighted mix"
  ) %>%
  select(
    year,
    province_id,
    province,
    dist_key,
    dist_nm, 
    indicator,
    gender,
    point_estimate,
    dataset
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
