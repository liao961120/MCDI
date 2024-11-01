library(dplyr)

d0 = readr::read_csv("raw/[Mandarin_Taiwanese_WG].csv") %>% 
    select(item, definition) %>% 
    distinct()
d1 = readr::read_csv("raw/[Mandarin_Taiwanese_WS].csv") %>% 
    select(item, category, definition) %>% 
    distinct()


d = full_join(d0, d1, 
              by = c("definition"="definition"), 
              suffix = c(".WG", ".WS")) %>% 
    mutate(ID = seq_along(definition)) %>% 
    select(ID, item.WG, item.WS, everything())


readr::write_csv(d, "made/word_id.csv")
# writexl::write_xlsx(d, "word_id.xlsx")
