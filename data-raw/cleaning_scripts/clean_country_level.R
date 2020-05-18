
#' clean_country_level
#' Helper function to clean country level dataset
#' @param df data.frame: Country level data.frame from DDH
#'
#' @return data.frame
#' @export
#'

clean_country_level <- function(df) {
  
  # CHECK input
  assertthat::assert_that(all(c("year", "country", "dataset") %in% colnames(df)))
  
  
  out <- df %>%
    tidyr::pivot_longer(cols = -c("country", "year", "dataset"), 
                        names_to = "names", 
                        values_to = "values") %>%
    # filter(!str_detect(names, "^egra_")) %>%
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
                                  sprintf("%.1f%%", point_estimate * 100))
    ) %>%
    dplyr::filter(!is.na(point_estimate))
  
  return(out)
  
}
