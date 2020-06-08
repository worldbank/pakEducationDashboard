

# Distrist level shapefiles -----------------------------------------------

pakgeo_district <- sf::read_sf("data-raw/data_input/pak_district_boundaries")

distkey_lkup <- read_csv("data-raw/data_input/distkey_lkup.csv") %>%
  mutate(
    province = str_to_lower(province),
    province = str_trim(province, side = "both"),
    district = str_to_lower(district),
    district = str_trim(district, side = "both"),
  )

pakgeo_district <- pakgeo_district %>%
  mutate(
    PROVINCE = str_to_lower(PROVINCE),
    DISTRICT = str_to_lower(DISTRICT)
  ) %>%
  left_join(distkey_lkup, by = c("PROVINCE" = "province", "DISTRICT" = "district"))

pakgeo_district$dist_key <- as.character(pakgeo_district$dist_key)

# Province level shapefiles -----------------------------------------------
pakgeo_province <- sf::read_sf("data-raw/data_input/pak_province_boundaries")

# Have to union the geometries of KP & Other areas
## other areas
pakgeo_province <- combine_sf(input_df = pakgeo_province[,c("NAME_1", "geometry")], 
                              column_name = dplyr::quo(NAME_1), 
                              row_values = c("Azad Kashmir", "F.C.T.","Northern Areas"),
                              output_value = "other areas")

## kp
pakgeo_province <- combine_sf(input_df = pakgeo_province, 
                              column_name = dplyr::quo(NAME_1), 
                              row_values = c("F.A.T.A.", "N.W.F.P.","Northern Areas"),
                              output_value = "kp")

provincekey_lkup    <- read_csv("data-raw/data_input/admin_level_lkup.csv") %>% 
  mutate(
    province = str_to_lower(province),
    province = str_trim(province, side = "both")
  ) %>%
  # Remove districts
  select(-district) %>%
  distinct()

pakgeo_province <- pakgeo_province %>%
  mutate(
    NAME_1     = str_to_lower(NAME_1),
    # TODO: Mention to Tony that we can't simply do a join as the province names in provincekey_lkup
    ## are different than the names in the shapes files
    ### (i.e khyber pakhtunkhwa in shape file, while "kp" in provincekey_lkup)
    province_id  = case_when(str_to_lower(NAME_1) == "other areas" ~ 0,
                             str_to_lower(NAME_1) == "punjab" ~ 1,
                             str_to_lower(NAME_1) == "sind" ~  2,
                             str_to_lower(NAME_1) == "baluchistan" ~ 3,
                             str_to_lower(NAME_1) == "kp" ~ 4)) %>%
  
  left_join(provincekey_lkup, by = "province_id") %>%
  select(-NAME_1, -province)




# Extract outer border ----------------------------------------------------
# Extract region ID 0
rg0 <- pakgeo_province %>%
  filter(province_id == 0) %>%
  st_cast("POLYGON") %>%
  slice(2)
# Extract region ID 1 & 4
rg14 <- pakgeo_province %>%
  filter(province_id %in% c(1, 4))

# Extract intersection
outer_border <- st_intersection(rg0, rg14) %>%
  st_geometry() 

# Remove disputed area ----------------------------------------------------
to_keep <- pakgeo_province %>%
  filter(province_id == 0) %>%
  st_cast("POLYGON") %>%
  slice(1)

pakgeo_province <- pakgeo_province %>%
  filter(province_id != 0) %>%
  bind_rows(to_keep)

# Save data ---------------------------------------------------------------

usethis::use_data(pakgeo_district,
                  pakgeo_province,
                  outer_border,
                  overwrite = TRUE)
