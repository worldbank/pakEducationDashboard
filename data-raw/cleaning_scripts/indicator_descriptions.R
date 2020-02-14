library(dplyr)

# Add indicator descriptions ------------------------------------------------

# Read Indicator Description df
df <- openxlsx::read.xlsx("data-raw\\data_input\\pak_indicator_descriptions\\pak_indicator_descriptions.xlsx")

# Select relevant columns
df <- df %>%
        select(Variable.Name, Indicator.definition, Label)

# Remove whitespaces
df$Variable.Name <- df$Variable.Name %>% trimws()


# Add relevant descriptions to country df
pakeduc_country           <- pakeduc_country %>% left_join(df, by = c("indicator" = "Variable.Name"))
pakeduc_country_weighted  <- pakeduc_country_weighted %>% left_join(df, by = c("indicator" = "Variable.Name"))

# Add relevant descriptions to district df
pakeduc_district           <- pakeduc_district %>% left_join(df, by = c("indicator" = "Variable.Name"))
pakeduc_district_weighted  <- pakeduc_district_weighted %>% left_join(df, by = c("indicator" = "Variable.Name"))

# Add relevant descriptions to province df
pakeduc_province           <- pakeduc_province %>% left_join(df, by = c("indicator" = "Variable.Name"))
pakeduc_province_weighted  <- pakeduc_province_weighted %>% left_join(df, by = c("indicator" = "Variable.Name"))



# Save data ---------------------------------------------------------------

usethis::use_data(pakeduc_country,
                  pakeduc_country_weighted,
                  pakeduc_district,
                  pakeduc_district_weighted,
                  pakeduc_province,
                  pakeduc_province_weighted,
                  overwrite = TRUE)
