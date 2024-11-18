# based on https://github.com/langcog/wordbank-book/blob/master/104-appendix-aoa.Rmd

library(dplyr)
load("raw/eng_ws_raw_data.Rds")

ms <- eng_ws %>%
    group_by(definition, age, category) %>%
    summarise(prop = mean(value == "produces", na.rm = TRUE), 
              num_true = sum(value == "produces", na.rm = TRUE), 
              num_false = sum(value != "produces", na.rm = TRUE), 
              n = sum(c(num_true, num_false))) %>%
    filter(!is.na(category))