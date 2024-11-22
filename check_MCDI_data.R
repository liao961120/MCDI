d = readr::read_csv("raw/China_WG.csv")


table(d$`26_dian4shi4u`, d$`26_dian4shi4p`)


d1 = readr::read_csv("raw/MandarinTaiwaneseWG_Liu_data.csv")

table(d1$d01_01u, d1$d01_01p)


library(dplyr)
d1 = readr::read_csv("raw/wordbank_instrument_data_MandarinWG.csv")


d1 %>% 
    filter(item_id == "item_61") %>% 
    select(child_id, value) %>% 
    arrange(child_id) -> d2

p = d2 %>% filter(value == "")






d1 = readr::read_csv("raw/wordbank_instrument_data_MandarinWS.csv")


d1 %>% 
    filter(item_id == "item_61") %>% 
    select(child_id, value) %>% 
    arrange(child_id) -> d2

p = d2 %>% filter(value == "")


###########
library(dplyr)
dG = readr::read_csv("raw/wordbank_instrument_data_MandarinWG.csv")
dS = readr::read_csv("raw/wordbank_instrument_data_MandarinWS.csv")
dm = readr::read_csv("raw/wordbank_administration_data_Mandarin.csv")


dG %>% 
    select(item_id, item_definition) %>% 
    distinct() %>% 
    arrange(item_id) -> item_dG


dS %>% 
    select(item_id, item_definition) %>% 
    distinct() %>% 
    arrange(item_id) %>% 
    full_join(item_dG, by=c("item_definition"="item_definition")) %>% 
    View()