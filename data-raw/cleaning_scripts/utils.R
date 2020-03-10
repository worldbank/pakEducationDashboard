

# Label and order indicators ----------------------------------------------
prepare_indicator_choices <- function(choices,
                                      expected_choices,
                                      choices_labels,
                                      labels_order) 
{
  assertthat::assert_that(all.equal(sort(choices), sort(expected_choices)))
  
  choices <- expected_choices
  names(choices) <- choices_labels
  
  choices <- choices[labels_order]
  
  return(choices)
}


# Combine sf geometries ---------------------------------------------------
combine_sf <- function(input_df, column_name, row_values, output_value){
  
  # Subset the rows according column and values
  temp_df <- purrr::map(row_values, function(x)
              {
                    dplyr::filter(input_df, !!column_name == x)
                             
              }) 

  # Merge to single sf data.frame
  temp_df <- sf::st_as_sf(plyr::ldply(temp_df, data.frame))
  
  # Create combined geometry
  combined_geometry <- sf::st_union(temp_df)
  
  # Account for KP
  if(output_value == "kp"){
    combined_geometry <- sf::st_cast(combined_geometry, "MULTIPOLYGON")
  }
  
  # Create combined geom data.frame
  combined_df <- data.frame(output_value, combined_geometry, stringsAsFactors = FALSE)
  colnames(combined_df)[1] <- "NAME_1"
  
  # Filter out rows to replace with combined row
  output_df <- dplyr::filter(input_df, !(!!column_name %in% row_values))

  return(rbind(output_df, sf::st_as_sf(combined_df)))
}

# List of indicators and labels -------------------------------------------
# Read Indicator Description df
df <- readxl::read_excel("./data-raw/data_input/pak_indicator_descriptions/pak_indicator_descriptions.xlsx")
df <- janitor::clean_names(df)
df <- janitor::remove_empty(df, which = "rows")

# Select relevant columns & remove white spaces
df <- df %>%
  dplyr::select(variable_name, indicator_definition, label) %>%
  dplyr::mutate(
    variable_name = trimws(variable_name)
  )

# Handle country / province level indicators
expected_choices <- c("reading_9_11",
                      "in_school_6_10",
                      "in_school_11_16",
                      "division_9_11",
                      "literacy_12_18",
                      "numeracy_12_18",
                      "share_private_6_10",
                      "share_private_11_16",
                      "egra_clpm",
                      "egra_clpm_g3",
                      "egra_clpm_g5",
                      "egra_orf",
                      "egra_orf_g3",
                      "egra_orf_g5")

tmp <- df %>%
  dplyr::filter(variable_name %in% expected_choices)
assertthat::are_equal(length(tmp$variable_name), length(expected_choices))

labels_lkup <- tmp$label
names(labels_lkup) <- tmp$variable_name

choice_labels <- unname(labels_lkup[expected_choices])

labels_order <- choice_labels # Order is defined by expected_choices vector

# Handle district level indicators
expected_choices_district = c("reading_9_11",
                              "in_school_6_10",
                              "in_school_11_16",
                              "division_9_11",
                              "literacy_12_18",
                              "numeracy_12_18",
                              "share_private_6_10", 
                              "share_private_11_16")

tmp <- df %>%
  dplyr::filter(variable_name %in% expected_choices_district)
assertthat::are_equal(length(tmp$variable_name), length(expected_choices_district))

labels_lkup <- tmp$label
names(labels_lkup) <- tmp$variable_name

choice_labels_district <- unname(labels_lkup[expected_choices_district])

labels_order_district <- choice_labels_district # Order is defined by expected_choices_district vector

create_weighted <- function(df,
                            selection = c("year",
                                          "country",
                                          "indicator",
                                          "gender",
                                          "point_estimate",
                                          "dataset"),
                            ...
                            )
{
  
  by <- enquos(...)
  
  out <- df %>%
    filter(!is.na(point_estimate)) %>%
    # aser data not to be used for these two indicators
    # considered unreliable
    filter(!(dataset == "aser" & (str_detect(indicator, "^in_school") | 
                                    str_detect(indicator, "^share_private")))) %>%
    mutate(
      inv_se = 1 / standard_error
    ) %>%
    group_by(!!!by) %>%
    mutate(
      inv_se_sum = sum(inv_se)
    ) %>%
    ungroup() %>%
    mutate(
      weight_var = inv_se / inv_se_sum,
      point_estimate = point_estimate * weight_var 
    ) %>%
    group_by(!!!by) %>%
    mutate(
      point_estimate = sum(point_estimate)
    ) %>%
    ungroup() %>%
    mutate(
      dataset = "Weighted mix (Moving average, window = 3)"
    ) %>%
    select(all_of(selection)) %>%
    distinct()
  
  return(out)
}