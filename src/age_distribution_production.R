library(dplyr)

dm = readr::read_csv("made/word_id.csv")

# Production only
dWG = readr::read_csv("raw/wordbank_instrument_data_MandarinWG.csv") %>% 
    select(child_id, item_definition, age, value) %>% 
    mutate(value = case_when(
        is.na(value)           ~ 0L,
        value == "understands" ~ 0L,
        value == "produces"    ~ 1L,
        TRUE                   ~ 0L
    ))
dWS = readr::read_csv("raw/wordbank_instrument_data_MandarinWS.csv") %>% 
    select(child_id, item_definition, age, value)  %>% 
    mutate(value = case_when(
        is.na(value)           ~ 0L,
        value == "understands" ~ 0L,
        value == "produces"    ~ 1L,
        TRUE                   ~ 0L
    ))
child_ages = bind_rows(
    dWG %>% select(child_id, age) %>% distinct(),
    dWS %>% select(child_id, age) %>% distinct(),
)

base_age = table(child_ages$age)
base_age_levels = names(base_age)

dm$age_distr_base = sapply(1:nrow(dm), \(i) paste(base_age, collapse="|"))
dm$age_distr = sapply(1:nrow(dm), \(i) {
    item = dm$item_definition[i]
    
    dg = dWG %>% filter(item_definition == {{ item }}, value == 1)
    ds = dWS %>% filter(item_definition == {{ item }}, value == 1)
    
    ages = c( dg$age, ds$age ) |> 
        as.character() |>
        ordered(levels = base_age_levels) |>
        table()
    # ages = ages / base_age  # use probability
    return(paste(ages, collapse="|"))
})

dm$age_distr_prob = sapply(dm$age_distr, \(x) {
    freq = strsplit(x, "|", fixed=T)[[1]] |> as.numeric()
    prob = freq / base_age
    return(paste(round(100*prob), collapse="|"))
})

word_age = function(age_distr, cuttoff=.5, Max=40) {
    sapply(age_distr, \(x) {
        freq = strsplit(x, "|", fixed=T)[[1]] |> as.numeric()
        prob = freq / base_age
        idx = which(prob >= cuttoff)
        if (length(idx) == 0) return(Max)
        idx = idx[1]
        if (idx == 1) return(base_age_levels[idx[1]] |> as.integer())
        
        # interpolate
        p1 = prob[idx-1]
        p2 = prob[idx]
        a1 = base_age_levels[idx-1] |> as.integer()
        a2 = base_age_levels[idx]   |> as.integer()
        aM = a1 + (cuttoff-p1)/(p2-p1) * (a2 - a1)
        return(aM)
    }, USE.NAMES = F)
}
dm$age.75 = word_age(dm$age_distr, cuttoff=.75, Max=40)
dm$age.50 = word_age(dm$age_distr, cuttoff=.50, Max=40)
dm$age.25 = word_age(dm$age_distr, cuttoff=.25, Max=40)

dm = dm %>% 
    rename(definition = item_definition) %>% 
    mutate(n = sapply(strsplit(age_distr, "|", fixed=T), \(x) sum(as.integer(x))) ) %>% 
    filter(n > 50) %>%   # ID:189,182,185 samp_size <= 8
    mutate(category = factor(category, levels=c('people', 
                                                'games_routines', 'action_words', 
                                                'food_drink', 'animals', 'outside',  
                                                'vehicles', 'toys', 
                                                'clothing', 'body_parts', 
                                                'furniture_rooms', 'household', 
                                                'descriptive_words', 
                                                'quantifiers', 
                                                'locations',  'places', 
                                                'time_words', 'question_words', 'connecting_words')  
                             )) %>% 
    arrange(category, age.50, age.75, age.25) %>% 
    select(everything(), -age_distr_base, -age_distr, -age_distr_prob, 
           age_distr_base, age_distr, age_distr_prob) %>% 
    mutate(age_rng = paste(range(as.integer(base_age_levels)), collapse="|"))

readr::write_csv(dm, "made/MCDI.age-production.csv")
writexl::write_xlsx(dm, "MCDI.age.xlsx")
