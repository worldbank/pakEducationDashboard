
#' clean_province_level
#' Helper function to clean province level dataset
#' @param df data.frame: Province level data.frame from DDH
#' @param province_lkup data.frame: Province lkup table (name / id)
#'
#' @return data.frame
#' @export
#'

clean_province_level <- function(df, province_lkup) {
  
  # CHECK input
  assertthat::assert_that(all(c("year", "province", "dataset") %in% colnames(df)))
  # The "Country" column has been included inconsistently in different versions
  # of the file. This handles it:
  if ("country" %in% colnames(df)) {
    df[["country"]] <- NULL
  }
  
  out <- df %>%
    dplyr::filter(!is.na(province)) %>%
    dplyr::mutate(
      province_id = as.numeric(province)
    ) %>%
    dplyr::select(-province) %>%
    dplyr::left_join(province_lkup) %>%
    dplyr::mutate(
      province = dplyr::if_else(province_id == 5, "Unnamed", province)
    ) %>%
    tidyr::pivot_longer(cols = -c("province_id", "province", "year", "dataset"), 
                        names_to = "names", 
                        values_to = "values") %>%
    #filter(!str_detect(names, "^egra_")) %>%
    dplyr::mutate(
      indicator = stringr::str_replace_all(names, "_se$", ""),
      indicator = stringr::str_replace_all(indicator, "_boys|_girls", ""),
      gender = dplyr::if_else(!stringr::str_detect(names, "boys|girls"), "Both", NA_character_),
      gender = dplyr::if_else(stringr::str_detect(names, "boys"), "Boy", gender),
      gender = dplyr::if_else(stringr::str_detect(names, "girls"), "Girl", gender),
      measurement = dplyr::if_else(stringr::str_detect(names, "_se$"), "standard_error", "point_estimate")
    ) %>%
    dplyr::select(-names) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "measurement", values_from = "values") %>%  
    dplyr::mutate(
      pe_percent = dplyr::if_else(stringr::str_detect(indicator, "^egra"), 
                                  as.character(round(point_estimate, 1)),
                                  sprintf("%.1f%%", point_estimate * 100)),
      province = as.factor(province),
      province = forcats::fct_relevel(province, "Punjab", "Sindh", "KP", "Balochistan", "Other areas")
      
    )
  
  return(out)
  
}

clean_province_level2 <- function(df,
                                  cols_to_remove = c( "country_tag",
                                                      "province_tag",
                                                      "district_tag",
                                                      "country",
                                                      "district"),
                                  unique_keys = c("identifier",
                                                  "province",
                                                  "year",
                                                  "dataset"),
                                  dimensions = c("gender", 
                                                 "wealth quintile", 
                                                 "urban-rural"),
                                  patterns = c("_boys|boys_|_boys_|_girls|girls_|_girls_", 
                                               "_wq[1-5]|wq[1-5]_|_wq[1-5]_",
                                               "_urban|urban_|_urban_|_rural|rural_|_rural_")) {
  
  # CHECK input
  assertthat::assert_that(all(cols_to_remove %in% colnames(df)))
  assertthat::assert_that(all(unique_keys %in% colnames(df)))
  
  
  out <- df %>%
    dplyr::select(-c(tidyselect::all_of(cols_to_remove))) %>%
    tidyr::pivot_longer(cols = -c(tidyselect::all_of(unique_keys)), 
                        names_to = "names", 
                        values_to = "values") %>%
    dplyr::mutate(
      dimensions       = extract_dimensions(col = names, 
                                            pattern_list = patterns, 
                                            dimension_list = dimensions),
      dimension_levels = extract_dimension_levels(col = names,
                                                  dim_col = dimensions,
                                                  dimensions = dimensions),
      indicator        = extract_indicator_name(col = names,
                                                to_remove = patterns),
      age_range        = extract_age_range(col = names),
      measurement      = extract_measurement(names)
    ) %>%
    dplyr::select(-names) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "measurement", values_from = "values") %>%
    dplyr::filter(!is.na(point_estimate)) %>%
    dplyr::mutate(
      pe_percent = dplyr::if_else(stringr::str_detect(indicator, "^egra"),
                                  as.character(round(point_estimate, 1)),
                                  sprintf("%.1f%%", point_estimate * 100))
    )
  
  return(out)
  
}

