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

#' read_variable_map
#'
#' @param ddh_link character: Link to variable map file on DDH
#'
#' @return data.frame
#' @export
#'
read_variable_map <- function(ddh_link = "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936441/dd_pak_varmap.xlsx") {
  httr::GET(ddh_link, httr::write_disk(tmp <- tempfile(fileext = ".xlsx")))
  
  df <- readxl::read_excel(tmp, sheet = "Varmap", skip = 1)
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
                                     labels_order = c("reading_9_11",
                                                      "in_school_6_10",
                                                      "in_school_6_15",
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
                                                      "egra_orf_g5")) 
{
  values <- df[df[[level]] == 1, c("variable_name", "label")]
  values <- values[!is.na(values$variable_name), ]
  values <- values[!values$variable_name %in% c("year", "dataset", "country", "province", "dist_key", "dist_nm"), ]
  values[] <- lapply(values, stringr::str_squish) 
  values <- values[!stringr::str_detect(tolower(values$variable_name), "_se$"),]
  values <- values[!stringr::str_detect(tolower(values$variable_name), "_boys|_girls"),]
  values <- unique(values)
  #values <- values[order(values$variable_name),]
  out <- values$variable_name
  assertthat::assert_that(all.equal(sort(out), sort(labels_order))) # CHECk that all expected indicators are here
  names(out) <- values$label
  
  out <- out[order(match(out, labels_order))]
  
  return(out)
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