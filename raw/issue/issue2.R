library(dplyr)

# Raw data at https://github.com/langcog/wordbank/tree/master/raw_data/Mandarin_Taiwanese_WG
d_raw = readr::read_csv('MandarinTaiwaneseWG_Liu_data.csv')
for ( c_ in names(d_raw)[17:724] )  # Recode NA responses as -1
    d_raw[[c_]] = ifelse(is.na(d_raw[[c_]]), -1L, d_raw[[c_]])
str(d_raw)

# Wordbank Child-by-item data
d_wb = readr::read_csv("wordbank_instrument_data_MandarinWG.csv") %>% 
    select(child_id, item_definition, english_gloss, age, value, everything())
str(d_wb)

# "dog" results (Wordbank processed data)
dog.wb = d_wb %>% filter(english_gloss == "dog")
table(dog.wb$value)

# "dog" results (raw data, -1s are NAs)
table(understands=d_raw$d01_01u, produces=d_raw$d01_01p)


# Wordbank API
library(dplyr)
d_m = wordbankr::get_instrument_data(language = "Mandarin (Taiwanese)",
                                     form = "WG", item_info = TRUE)
d.dog = d_m |>
    filter(english_gloss == "dog")
