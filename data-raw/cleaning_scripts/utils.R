#' clean_raw_data
#' Helper function to clean data stored on DDH for easy consumption by the
#' Shiny app
#' @param df data.frame: Data from DDH (Country, Province, or District level)
#' @param cols_to_remove character: Columns to be removed from the data frame
#' @param unique_keys character: Columns to be used as unique keys to pivot data frame
#' @param dimensions character: Dimensions to be used for indicator decomposition
#' @param patterns character: Patterns to be matched for identification of 
#' dimensions in column names
#'
#' @return data.frame
#' @export
#'
clean_raw_data <- function(df,
                           cols_to_remove = c( "country_tag",
                                               "province_tag",
                                               "district_tag",
                                               "country",
                                               "province", 
                                               "district"),
                           unique_keys = c("identifier",
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


#' read_variable_map
#'
#' @param ddh_link character: Link to variable map file on DDH
#'
#' @return data.frame
#' @export
#'
read_variable_map <- function(ddh_link = "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_varmap.xlsx") {
  httr::GET(ddh_link, httr::write_disk(tmp <- tempfile(fileext = ".xlsx")))
  
  df <- readxl::read_excel(tmp, sheet = "Varmap", skip = 2)
  df <- janitor::clean_names(df)
  df <- janitor::remove_empty(df, which = "rows")
  df <- janitor::remove_empty(df, which = "cols")
}


#' create_indicator_choices
#' Create a vector of valid choices from the Variable map file stored in DDH
#' 
#' @param df data.frame: Varmap sheet of the DDH file
#' @param level character: Regional level for which to extract the list of indicators
#'
#' @return named character vector
#' @export
#'

create_indicator_choices <- function(df, 
                                     level = c("country", "province","district"),
                                     to_remove = c("year", 
                                                   "dataset", 
                                                   "country", 
                                                   "province", 
                                                   "dist_key", 
                                                   "dist_nm",
                                                   "district",
                                                   "country_tag",
                                                   "province_tag",
                                                   "district_tag"),
                                     pattern_to_remove = c("_boy|boy_|_boy_|_girl|girl_|_girl_", 
                                                           "_wq[1-5]|wq[1-5]_|_wq[1-5]_",
                                                           "_urban|urban_|_urban_|_rural|rural_|_rural_"),
                                     labels_order = c("reading_6_8",
                                                      "reading_9_11",
                                                      "reading_12_14",
                                                      "in_school_5_10",
                                                      "in_school_5_16",
                                                      "in_school_11_16",
                                                      "division_6_8",
                                                      "division_9_11",
                                                      "division_12_14",
                                                      "literacy_12_18",
                                                      "literacy_19_25",
                                                      "literacy_26_32",
                                                      "numeracy_12_18",
                                                      "numeracy_19_25",
                                                      "numeracy_26_32",
                                                      "share_private_5_10",
                                                      "share_private_5_16", 
                                                      "share_private_11_16",
                                                      "egra_clpm",
                                                      "egra_clpm_g3",
                                                      "egra_clpm_g5",
                                                      "egra_orf",
                                                      "egra_orf_g3",
                                                      "egra_orf_g5")) 
{
  pattern_to_remove <- paste(pattern_to_remove, collapse = "|")
  
  values <- df[df[[level]] == 1, c("variable_name", "label")]
  values <- values[!is.na(values$variable_name), ]
  values <- values[!values$variable_name %in% to_remove, ]
  values[] <- lapply(values, stringr::str_squish) 
  values <- values[!stringr::str_detect(tolower(values$variable_name), pattern = pattern_to_remove),]
  values <- values[!stringr::str_detect(tolower(values$variable_name), "_se$"),]
  values <- values[!stringr::str_detect(tolower(values$variable_name), "_n$"),]
  values <- unique(values)
  #values <- values[order(values$variable_name),]
  out <- values$variable_name
  assertthat::assert_that(all.equal(sort(out), sort(labels_order))) # CHECk that all expected indicators are here
  names(out) <- values$label
  
  out <- out[order(match(out, labels_order))]
  
  # Create a list of options with subsections
  df$age_range <- extract_age_range(df$variable_name)
  age_ranges <- unique(df$age_range[!is.na(df$age_range)])
  # reorder vector of age_ranges
  tmp <- stringr::str_replace(age_ranges, " to ", ".")
  tmp <- readr::parse_number(tmp)
  names(tmp) <- age_ranges
  tmp <- sort(tmp)
  age_ranges <- names(tmp)
  
  out_list <- vector(mode = "list", length = length(age_ranges))
  age_patterns <- stringr::str_replace(age_ranges, " to ", "_")
  age_names <- paste0("Age range: ", age_ranges)
  
  for (i in seq_along(out_list)) {
    tmp <- out[str_detect(out, age_patterns[i])]
    out_list[[i]] <- tmp
    names(out_list)[i] <- age_names[i]
  }
  
  return(out_list)
}


#' created_weighted
#' Created weighted average
#' 
#' @param df 
#' @param selection 
#' @param ... 
#'
#' @return
#' @export
#'
create_weighted <- function(df,
                            selection = c("year",
                                          "indicator",
                                          "age_range",
                                          "dimensions",
                                          "dimension_levels",
                                          "point_estimate",
                                          "dataset"),
                            weighted_mix = "Weighted mix (Moving average, window = 3)",
                            ...
                            )
{
  
  by <- dplyr::enquos(...)
  weighted_mix <- dplyr::enquo(weighted_mix)
  
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
      dataset = !!weighted_mix
    ) %>%
    select(all_of(selection)) %>%
    distinct()
  
  return(out)
}

#' extract_dimension
#' 
#' @param col character: Character vector of column names from which dimensions need to be extracted
#' @param pattern character: Regular expression to match specific patterns in \code{col}
#' @param dimension character: Dimension names corresponding to \code{pattern}
#'
#' @return character
#' @export
#'
extract_dimension <- function(col, pattern, dimension) {
  # CHECK inputs
  assertthat::assert_that(class(col) == "character")
  assertthat::assert_that(class(pattern) == "character")
  assertthat::assert_that(class(dimension) == "character")
  assertthat::assert_that(length(pattern) == 1)
  assertthat::assert_that(length(dimension) == 1)
  
  out <- stringr::str_detect(col, pattern = pattern)
  col[out == TRUE] <- dimension
  
  return(col)
}

#' extract_dimensions
#' vectorized version fo extract_dimension
#' @param col character: Character vector of column names from which dimensions need to be extracted
#' @param pattern_list character: Regular expressions to match specific patterns in \code{col}
#' @param dimension_list character: Dimension names corresponding to \code{pattern}
#'
#' @return character
#' @export
#'
extract_dimensions <- function(col, pattern_list, dimension_list) {
  # CHECK that lists are of the same length
  assertthat::assert_that(length(pattern_list) == length(dimension_list))
  
  for (i in seq_along(pattern_list)) {
    col <- extract_dimension(col, 
                             pattern = pattern_list[[i]],
                             dimension = dimension_list[[i]]
                             )
  }
  
  col[!col %in% dimension_list] <- "aggregate"
  
  return(col)
}

#' extract_indicator_name
#' Retrieve indicator name from column names
#' @param col character: Character vector of column names from which indicators need to be extracted
#' @param to_remove character: Regular expressions of patterns to be removed from name
#'
#' @return character
#' @export
#'
extract_indicator_name <- function(col, to_remove) {
  # CHECK inputs
  assertthat::assert_that(class(col) == "character")
  assertthat::assert_that(class(to_remove) == "character")
  to_remove <- paste(to_remove, collapse = "|")
  
  col <- stringr::str_remove(col, pattern = to_remove) 
  col <- stringr::str_remove(col, "_se$|_n$")
  
  return(col)
}

#' extract_age_range
#' Retrieve age_range name from column names
#' @param col character: Character vector of column names from which age ranges need to be extracted
#'
#' @return character
#' @export
#'
extract_age_range <- function(col) {
  # CHECK inputs
  assertthat::assert_that(class(col) == "character")
  age_range_pattern <- "_[0-9]{1,2}_[0-9]{1,2}"
  
  col <- stringr::str_extract(col, pattern = age_range_pattern) 
  col <- stringr::str_remove(col, "^_")
  col <- stringr::str_replace(col, "_", " to ")
  
  return(col)
}

#' extract_measurement
#' Retrieve measurement type from column names
#' @param col character: Character vector of column names from which measurements need to be extracted
#'
#' @return character
#' @export
#'
extract_measurement <- function(col) {
  # CHECK inputs
  assertthat::assert_that(class(col) == "character")
  
  sd <- "standard_error"
  ss <- "sample_size"
  
  out <- col
  out[stringr::str_detect(col, "_se$")] <- sd 
  out[stringr::str_detect(col, "_n$")] <- ss
  out[!out %in% c(sd, ss)] <- "point_estimate" 

  return(out)
}

#' extract_dimension_levels
#' extract_dimension_levels from column names
#' @param col character: Column names from which dimension levels need to be extracted
#' @param dim_col character: Column pre-identified dimensions
#' @param dimensions character: Valid dimensions
#' 
#' @return character
#' @export
#'
extract_dimension_levels <- function(col, dim_col, dimensions) {
  # CHECK that lists are of the same length
  assertthat::assert_that(class(col) == "character")
  assertthat::assert_that(length(col) == length(dim_col))
  assertthat::assert_that(all(dimensions %in% unique(dim_col)))
  
  gender_dim <- "gender"
  wq_dim <- "wealth quintile"
  ur_dim <- "urban-rural"
  wq <- c("wq1", "wq2", "wq3", "wq4", "wq5")
  # CHECK that hard coded variable are valid
  # Not great. Think about refactoring this
  assertthat::assert_that(gender_dim %in% dimensions)
  assertthat::assert_that(wq_dim %in% dimensions)
  assertthat::assert_that(ur_dim %in% dimensions)
  
  out <- rep("Combined", length(col))
  # Handle gender
  out[stringr::str_detect(col, "boys")] <- "Boy"
  out[stringr::str_detect(col, "girls")] <- "Girl"
  # out[(!stringr::str_detect(col, "boys|girls")) & 
  #       stringr::str_detect(dim_col, gender_dim)] <- "Gender combined"
  # Handle wealth quintile
  for (i in seq_along(wq)) {
    out[stringr::str_detect(col, wq[i])] <- wq[i]
  }
  # out[(!stringr::str_detect(col, paste(wq, collapse = "|"))) & 
  #       stringr::str_detect(dim_col, wq_dim)] <- "All quintiles"
  # Handle urban-rural
  out[stringr::str_detect(col, "urban")] <- "Urban"
  out[stringr::str_detect(col, "rural")] <- "Rural"
  # out[(!stringr::str_detect(col, "urnam|rural")) & 
  #       stringr::str_detect(dim_col, ur_dim)] <- "Urban and Rural combined"
  
  return(out)
}
