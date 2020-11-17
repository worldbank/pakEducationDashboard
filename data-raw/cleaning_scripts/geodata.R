

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
provincekey_lkup    <- read_csv("data-raw/data_input/admin_level_lkup.csv") %>% 
  mutate(
    province = str_to_lower(province),
    province = str_trim(province, side = "both")
  ) %>%
  # Remove districts
  select(-district) %>%
  distinct()

# Shapefile and dataset regions are different
# Some shapefile regions need to be combined (sf::st_union) 
pakgeo_province <- pakgeo_province <- sf::read_sf("data-raw/data_input/pak_province_boundaries") %>%
  mutate(
    # "Azad Kashmir", "F.C.T.","Northern Areas" are mapped to "other areas"
    # "F.A.T.A.", "N.W.F.P." are mapped to "kp"
    province_id  = case_when(HASC_1 == "PK.JK" ~ 0,
                             HASC_1 == "PK.BA" ~ 3,
                             HASC_1 == "PK.TA" ~ 4,
                             HASC_1 == "PK.IS" ~ 0,
                             HASC_1 == "PK.NW" ~ 4,
                             HASC_1 == "PK.NA" ~ 0,
                             HASC_1 == "PK.PB" ~ 1,
                             HASC_1 == "PK.SD" ~ 2)
  ) %>%
  left_join(provincekey_lkup, by = "province_id") %>%
  group_by(province_id) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  ungroup()

# Extract dashed border ----------------------------------------------------
# Extract region ID 0
# This region contains 2 polygons
# I'm only interested in the larger one to identify the common border
# with other regions
rg0 <- pakgeo_province %>%
  filter(province_id == 0) %>%
  st_cast("POLYGON") %>%
  mutate(
    area = st_area(geometry)
  ) %>%
  filter(
    area == max(area, na.rm = TRUE) # Keep only larger polygon
  ) %>%
  select(-area) %>%
  st_cast("LINESTRING") %>%
  pull(geometry)
 
# Extract region ID 1 & 4
rg14 <- pakgeo_province %>%
  filter(province_id %in% c(1, 4)) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  mutate(
    province_id = 0
  ) %>%
  st_cast("LINESTRING") %>%
  slice(1) %>% # List filtering should also be based on polygon area to avoid silent failing due to the list order
  pull(geometry)

# Extract intersection
tmp <- st_difference(rg14, rg0)
dashed_border <- st_difference(rg14, tmp)

# Extract dotted border
tmp <- st_difference(rg0, rg14)
my_bbox <- st_bbox(tmp)
my_bbox[["ymax"]] <- 35.469
dotted_border <- tmp %>%
  st_crop(my_bbox)

# Extract plain border
tmp <- st_difference(rg0, rg14)
my_bbox <- st_bbox(tmp)
my_bbox[["ymin"]] <- 35.469
plain_border <- tmp %>%
  st_crop(my_bbox)

# Remove disputed area ----------------------------------------------------
# Islamabad area: Will be colored on the map
to_keep <- pakgeo_province %>%
  filter(province_id == 0) %>%
  st_cast("POLYGON") %>%
  slice(1)
# Other area (non-Islamabad): Will not be colored on the map
# to_modify <- pakgeo_province %>%
#   filter(province_id == 0) %>%
#   mutate(
#     province_id = 6
#   ) %>%
#   st_cast("POLYGON") %>%
#   slice(2)

pakgeo_province <- pakgeo_province %>%
  filter(province_id != 0) %>%
  bind_rows(to_keep) #%>%
  #bind_rows(to_modify)


# Handle projection -------------------------------------------------------
# Reference: https://stackoverflow.com/questions/61286108
st_crs(pakgeo_district) <- 4326
st_crs(pakgeo_province) <- 4326
st_crs(dashed_border) <- 4326
st_crs(dotted_border) <- 4326
st_crs(plain_border) <- 4326

# Save data ---------------------------------------------------------------

usethis::use_data(pakgeo_district,
                  pakgeo_province,
                  dashed_border,
                  dotted_border,
                  plain_border,
                  overwrite = TRUE)
