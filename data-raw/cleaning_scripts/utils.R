

# Label and order indicators ----------------------------------------------
prepare_indicator_choices <- function(choices,
                                      expected_choices,
                                      choices_labels,
                                      labels_order) 
{
  assertthat::assert_that(all.equal(choices, expected_choices))
  
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
expected_choices <- c("division_9_11",
                      "egra_clpm",
                      "egra_clpm_g3",
                      "egra_clpm_g5",
                      "egra_orf",
                      "egra_orf_g3",
                      "egra_orf_g5",
                      "in_school_11_16",
                      "in_school_6_10",
                      "literacy_12_18",
                      "numeracy_12_18",
                      "reading_9_11",
                      "share_private_11_16",
                      "share_private_6_10" )

choices_labels <- c("Share of kids who can do division,  aged 9-11",
                    "EGRA: correct letters per minute",
                    "EGRA: correct letters per minute - Grade 3",
                    "EGRA: correct letters per minute - Grade 5",
                    "EGRA: Oral reading fluency",
                    "EGRA: Oral reading fluency - Grade 3",
                    "EGRA: Oral reading fluency - Grade 5",
                    "Share of kids curently in school,  aged 11-16",
                    "Share of kids curently in school,  aged 6-10",
                    "Share of adolescent who are literate (self-reported),  aged 12-18",
                    "Share of adolescent who are numerate (self-reported),  aged 12-18",
                    "Share of kids who can read a basic paragraph,  aged 9-11",
                    "Share of kids in private schools, among those enrolled,  aged 11-16",
                    "Share of kids in private schools, among those enrolled,  aged 6-10")

labels_order <- c("Share of kids who can do division,  aged 9-11",
                  "Share of kids curently in school,  aged 6-10",
                  "Share of kids curently in school,  aged 11-16",
                  "Share of kids who can read a basic paragraph,  aged 9-11",
                  "Share of kids in private schools, among those enrolled,  aged 6-10",
                  "Share of kids in private schools, among those enrolled,  aged 11-16",
                  "Share of adolescent who are literate (self-reported),  aged 12-18",
                  "Share of adolescent who are numerate (self-reported),  aged 12-18",
                  "EGRA: correct letters per minute",
                  "EGRA: correct letters per minute - Grade 3",
                  "EGRA: correct letters per minute - Grade 5",
                  "EGRA: Oral reading fluency",
                  "EGRA: Oral reading fluency - Grade 3",
                  "EGRA: Oral reading fluency - Grade 5")

expected_choices_district = c("division_9_11",
                              "in_school_11_16",
                              "in_school_6_10",
                              "literacy_12_18",
                              "numeracy_12_18",
                              "reading_9_11",
                              "share_private_11_16",
                              "share_private_6_10" )
choices_labels_district <- c("Share of kids who can do division,  aged 9-11",
                             "Share of kids curently in school,  aged 11-16",
                             "Share of kids curently in school,  aged 6-10",
                             "Share of adolescent who are literate (self-reported),  aged 12-18",
                             "Share of adolescent who are numerate (self-reported),  aged 12-18",
                             "Share of kids who can read a basic paragraph,  aged 9-11",
                             "Share of kids in private schools, among those enrolled,  aged 11-16",
                             "Share of kids in private schools, among those enrolled,  aged 6-10")
labels_order_district <- c("Share of kids who can do division,  aged 9-11",
                           "Share of kids curently in school,  aged 6-10",
                           "Share of kids curently in school,  aged 11-16",
                           "Share of kids who can read a basic paragraph,  aged 9-11",
                           "Share of kids in private schools, among those enrolled,  aged 6-10",
                           "Share of kids in private schools, among those enrolled,  aged 11-16",
                           "Share of adolescent who are literate (self-reported),  aged 12-18",
                           "Share of adolescent who are numerate (self-reported),  aged 12-18")