prepare_indicator_choices <- function(choices,
                                      expected_choices = c("division_9_11",
                                                           "in_school_11_16",
                                                           "in_school_6_10",
                                                           "literacy_12_18",
                                                           "numeracy_12_18",
                                                           "reading_9_11",
                                                           "share_private_11_16",
                                                           "share_private_6_10" ),
                                      choices_labels = c("Share of kids who can do division,  aged 9-11",
                                                         "Share of kids curently in school,  aged 11-16",
                                                         "Share of kids curently in school,  aged 6-10",
                                                         "Share of adolescent who are literate (self-reported),  aged 12-18",
                                                         "Share of adolescent who are numerate (self-reported),  aged 12-18",
                                                         "Share of kids who can read a basic paragraph,  aged 9-11",
                                                         "Share of kids in private schools, among those enrolled,  aged 11-16",
                                                         "Share of kids in private schools, among those enrolled,  aged 6-10"),
                                      labels_order = c("Share of kids who can do division,  aged 9-11",
                                                       "Share of kids curently in school,  aged 6-10",
                                                       "Share of kids curently in school,  aged 11-16",
                                                       "Share of kids who can read a basic paragraph,  aged 9-11",
                                                       "Share of kids in private schools, among those enrolled,  aged 6-10",
                                                       "Share of kids in private schools, among those enrolled,  aged 11-16",
                                                       "Share of adolescent who are literate (self-reported),  aged 12-18",
                                                       "Share of adolescent who are numerate (self-reported),  aged 12-18")) 
{
  assertthat::assert_that(all.equal(choices, expected_choices))
  
  names(choices) <- choices_labels
  
  choices <- choices[labels_order]
  
  return(choices)
}
