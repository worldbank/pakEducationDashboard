library(tidyverse)
library(sf)
source("./data-raw/cleaning_scripts/utils.R")


# Create indicators -------------------------------------------------------
varmap <- read_variable_map()

indicator_choices_country <- create_indicator_choices(df = varmap, level = "country")
indicator_choices_province <- create_indicator_choices(df = varmap, level = "province")
indicator_choices_district <- create_indicator_choices(df = varmap, level = "district",
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
                                                                        "share_private_11_16"
                                                       ))


# Create package data using a series of scripts ---------------------------

# Create country level data
source("./data-raw/cleaning_scripts/data_country.R")
# Create province level data
source("./data-raw/cleaning_scripts/data_province.R")
# Create district level data
source("./data-raw/cleaning_scripts/data_district.R")
# Create geodata
source("./data-raw/cleaning_scripts/geodata.R")


# Add labels --------------------------------------------------------------

# Read Indicator Description df
df <- varmap %>%
  janitor::clean_names() %>%
  janitor::remove_empty(which = "rows")

# Select relevant columns & remove white spaces
df <- df %>%
  select(variable_name, indicator_definition, label) %>%
  mutate(
    variable_name = stringr::str_squish(variable_name)
  )


# Add relevant descriptions to country df
pakeduc_country           <- pakeduc_country %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_country_weighted  <- pakeduc_country_weighted %>% left_join(df, by = c("indicator" = "variable_name"))

# Add relevant descriptions to province df
pakeduc_province           <- pakeduc_province %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_province_weighted  <- pakeduc_province_weighted %>% left_join(df, by = c("indicator" = "variable_name"))

# Add relevant descriptions to district df
pakeduc_district           <- pakeduc_district %>% left_join(df, by = c("indicator" = "variable_name"))
pakeduc_district_weighted  <- pakeduc_district_weighted %>% left_join(df, by = c("indicator" = "variable_name"))


# Data sources descrition
library(shiny)
data_sources_description <- tagList(
  tags$h4("Data sources"),
  tags$ul(
    tags$li(tags$a("ASER", href = "http://aserpakistan.org/index.php", target="_blank"),
            ": The Annual Status of Education Report is a citizen-led; household-based survey, led by ITA. "),
    tags$li(tags$a("DHS",  href ="https://dhsprogram.com/what-we-do/survey/survey-display-523.cfm", target="_blank"),
            ": The Pakistan Demographic and Health Survey is a household survey implemented by the National Institute of Population Studies."),
    tags$li(tags$a("EGRA", href ="https://www.usaid.gov/news-information/videos/early-grade-reading-assessment-egra-extra-mile-better-journey", target="_blank"),
            ": EGRA is an individually administered assessment tool that measures foundational literacy skills, which has been administered as part of USAID’s Pakistan Reading Project."),
    tags$li(tags$a("HIES", href = "http://www.pbs.gov.pk/content/household-integrated-economic-survey-hies-2015-16", target="_blank"), 
            ": The Household Integrated Economic Survey is a household survey, representative at the provincial level, and led by the Pakistan Bureau of Statistics."),
    tags$li(tags$a("MICS", href ="http://www.bos.gop.pk/mics", target="_blank"),
            ": The Multiple Indicator Cluster Survey is a multipurpose household survey implemented on a provincial level basis, and is representative at district level."),
    tags$li(tags$a("PSLM", href ="http://www.pbs.gov.pk/content/pakistan-social-and-living-standards-measurement", target="_blank"),
            ": The Pakistan Social and Living Standards Measurement (PSLM) is another household survey, representative at the district level, and led by the Pakistan Bureau of Statistics.")
  )
)



usethis::use_data(pakeduc_country,
                  pakeduc_country_weighted,
                  indicator_choices_country,
                  pakeduc_province,
                  pakeduc_province_weighted,
                  indicator_choices_province,
                  pakeduc_district,
                  pakeduc_district_weighted,
                  indicator_choices_district,
                  indicator_choices_country_inv,
                  pakgeo_district,
                  pakgeo_province,
                  data_sources_description,
                  overwrite = TRUE)
