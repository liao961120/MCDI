library(dplyr)
dG = readr::read_csv("raw/wordbank_instrument_data_MandarinWG.csv")
dS = readr::read_csv("raw/wordbank_instrument_data_MandarinWS.csv")

categories = c(
    animals           = "動物",
    body_parts        = "身體部位",
    vehicles          = "交通工具",
    food_drink        = "食物/飲料",
    clothing          = "衣物",
    toys              = "玩具",
    household         = "家庭用品",
    furniture_rooms   = "家具/房間",
    outside           = "戶外",
    places            = "地點",
    games_routines    = "遊戲/日常生活",
    people            = "人物",
    time_words        = "時間詞",
    locations         = "位置",
    action_words      = "動作詞",
    connecting_words  = "連接詞",
    descriptive_words = "描述詞",
    quantifiers       = "數量詞",
    question_words    = "疑問詞"
)


dG = dG %>% 
    select(item_definition, category) %>% 
    distinct() %>% 
    arrange()


dm = dS %>% 
    select(item_definition, category) %>% 
    distinct() %>% 
    full_join(dG, by=c("item_definition"="item_definition")) %>% 
    select(1:2) %>% 
    rename(category = category.x) %>% 
    filter(!is.na(category)) %>% 
    mutate(category_zh = categories[category])

readr::write_csv(dm, "made/word_id.csv")
