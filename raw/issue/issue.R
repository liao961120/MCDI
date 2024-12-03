library(dplyr)
library(tidyr)

get_age_distr_dat = function(fp, values=c("understands","produces") ) {
    d = readr::read_csv(fp) %>% 
        filter(item_kind == "word") %>% 
        select(child_id, item_definition, english_gloss, age, value)
    base_age_num = d %>% select(child_id, age) %>% distinct() %>% .$age %>% table()
    d %>% 
        filter(value %in% {{ values }}) %>% 
        ungroup() %>% 
        group_by(age, english_gloss, item_definition) %>% 
        summarise(n = n()) %>% 
        ungroup() %>%
        mutate(prop = n / base_age_num[as.character(age)]) %>% 
        arrange(age, desc(prop))
}
plot_word = function(d, english_gloss, ...) {
    dat = d %>% 
        filter(english_gloss == {{ english_gloss }}) %>% 
        arrange(age)
    
    plot( 1, type="n", xlim=c(0,nrow(dat)), ylim=c(0,1), xaxt="n", 
          xlab="Age", ylab="Proportion", ... )
    for (i in 1:nrow(dat)) {
        x_ = rep(i,2)
        y_ = c(0, dat$prop[i])
        lines( x_, y_, lwd=7, col=2)
    }
    axis(1, at=1:nrow(dat), labels=dat$age)
}


# CDI data retrieved from <https://wordbank.stanford.edu/data/?name=instrument_data> (Full Child-by-Word Data)
url = "https://raw.githubusercontent.com/liao961120/MCDI/refs/heads/main/raw/issue"
src = c(
    "Mandarin (Taiwan)"  = file.path(url, "wordbank_instrument_data_MandarinTW_WG.csv"),
    "English (American)" = file.path(url, "wordbank_instrument_data_Eng_WG.csv")
)

# English (American) WG data
get_age_distr_dat(src["English (American)"])
# Mandarin (Taiwan) WG data
get_age_distr_dat(src["Mandarin (Taiwan)"])


#### Plot words ####
for (nm in names(src)) {
    dat = get_age_distr_dat(src[nm], values=c("understands", "produces"))
    par(mfrow=c(2,2))
    for (word in c("ball", "mouth", "dog", "nose") )
        plot_word(dat, word, main=paste0(nm, ": ", word) )
}
