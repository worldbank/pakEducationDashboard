
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
  pivot_wider(names_from = "measurement", values_from = "values") %>%  
  mutate(
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100)),
    province = as.factor(province),
    province = forcats::fct_relevel(province, "Punjab", "Sindh", "KP", "Balochistan", "Other areas")
    
  )

indicator_choices_province <- sort(unique(pakeduc_province$indicator)) 
indicator_choices_province <- prepare_indicator_choices(indicator_choices_province,
                                                        expected_choices = expected_choices,
                                                        choices_labels = choice_labels,
                                                        labels_order = labels_order)
                                                                         
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
    point_estimate = zoo::rollapply(point_estimate, 3, mean, align='center', fill=NA),
    pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
                         as.character(round(point_estimate, 1)),
                         sprintf("%.1f%%", point_estimate * 100))
  ) %>%
  ungroup() %>%
  filter(!is.na(point_estimate)) %>%
  distinct()

# 
# pakeduc_province_weighted <- pakeduc_province %>%
#   filter(!is.na(point_estimate)) %>%
#   filter(!(dataset == "aser" & (str_detect(indicator, "^in_school") | str_detect(indicator, "^share_private")))) %>%
#   mutate(
#     inv_se = 1 / standard_error
#   ) %>%
#   group_by(year, province, indicator, gender) %>%
#   mutate(
#     inv_se_sum = sum(inv_se)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     weight_var = inv_se / inv_se_sum,
#     point_estimate = point_estimate * weight_var 
#   ) %>%
#   group_by(year, province, indicator, gender) %>%
#   mutate(
#     point_estimate = sum(point_estimate)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     dataset = "Weighted mix"
#   ) %>%
#   select(
#     year,
#     province_id,
#     province,
#     indicator,
#     gender,
#     point_estimate,
#     dataset
#   ) %>%
#   distinct() %>%  
#   mutate(
#     pe_percent = if_else(stringr::str_detect(indicator, "^egra"), 
#                          as.character(round(point_estimate, 1)),
#                          sprintf("%.1f%%", point_estimate * 100))
#   )

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
