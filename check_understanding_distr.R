library(dplyr)

# Understanding only
dWG = readr::read_csv("raw/wordbank_instrument_data_MandarinWG.csv") %>% 
    filter(item_kind == "word") %>% 
    select(child_id, category, item_definition, age, value) %>% 
    mutate(value = case_when(
        is.na(value)           ~ 0L,
        value == "understands" ~ 1L,
        value == "produces"    ~ 1L
    ))

child_ages = dWG %>% select(child_id, age) %>% distinct()
base_age = table(child_ages$age)
base_age_levels = names(base_age)


dm = dWG %>% select(item_definition, category) %>% distinct()


dm$age_distr_base = sapply(1:nrow(dm), \(i) paste(base_age, collapse="|"))
dm$age_distr = sapply(1:nrow(dm), \(i) {
    item = dm$item_definition[i]
    
    dg = dWG %>% filter(item_definition == {{ item }}, value == 1)
    ages = as.character(dg$age) |>
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
dm$age.75 = word_age(dm$age_distr, cuttoff=.75, Max=16)
dm$age.50 = word_age(dm$age_distr, cuttoff=.50, Max=16)
dm$age.25 = word_age(dm$age_distr, cuttoff=.25, Max=16)

dm = dm %>% 
    rename(definition = item_definition) %>% 
    mutate(n = sapply(strsplit(age_distr, "|", fixed=T), \(x) sum(as.integer(x))) ) %>% 
    arrange(age.50, age.75, age.25) %>% 
    select(everything(), -age_distr_base, -age_distr, -age_distr_prob, 
           age_distr_base, age_distr, age_distr_prob)  %>% 
    mutate(age_rng = paste(range(as.integer(base_age_levels)), collapse="|")) %>%   # ID:189,182,185 samp_size <= 8
    filter(!is.na(category)) %>% 
    mutate(category = factor(category, levels= c('people', 'body_parts', 'food_drink', 'animals', 'furniture_rooms', 'games_routines', 
                                                 'clothing', 'household', 'action_words', 
                                                 'toys', 'descriptive_words', 'vehicles')
    ))


writexl::write_xlsx(dm, "china.xlsx")


### Plot word #############
library(dplyr)
library(showtext)
library(progress)
f = glue::glue
YLAB = "%"


plot_word = function(idx, base_col=1, test=F) {
    if (test) pdf("test.pdf")
    word  = d$definition[idx]
    distr = strsplit(d$age_distr_prob[idx], "|", fixed=T)[[1]] |> as.numeric()
    distr = c( distr, rep(0, length(8:36)-length(distr)) )
    x_axis_lab = 8:36
    highlight_idx = which(x_axis_lab == ceiling(d$age.50[idx]) )
    
    showtext_begin()
    plot(distr, xaxt="n", col="white", xlab="", ylab="", ylim=c(0,100),
         axes = FALSE)
    abline(h=50, col=stom::col.alpha("grey",.7), lty="dashed")
    # lines(distr+1, col="grey", lwd=2)
    for (i in seq(distr)) {
        x_ = rep(i,2)
        y_ = c(0,distr[i])
        c_ = stom::col.alpha(base_col,.55)
        if (base_col %% 8 == 2) c_ = stom::col.alpha(base_col,.35)
        if (base_col %% 8 == 6) c_ = stom::col.alpha(base_col,.25)
        if (base_col %% 8 == 8) c_ = stom::col.alpha(base_col, 1)
        if ( length(highlight_idx) == 1 && i == highlight_idx && any(distr >= 50) ) 
            c_ = 2
        if (y_[2] > 0)
            lines(x_, y_, lwd=6, col=c_)
    }
    
    axis(1, at=seq(x_axis_lab), labels = x_axis_lab, family="gochi", 
         cex.axis=1.4, padj=-1.05, col.ticks = "white")
    axis(2, las=1, family="gochi", cex.axis=1.5)
    box(bty='L')
    
    nchr = nchar(word)
    if (nchr > 10) {
        title(main=word, cex.main=4, line=1.5, family="zh")
    } 
    else if (nchr <= 2)
        title(main=word, cex.main=5.5, line=1.5, family="zh")
    else {
        title(main=word, cex.main=5, line=1.5, family="zh")
    }
    
    title(xlab="月齡", cex.lab = 2, line=2.8, family="zh")
    title(ylab=YLAB, cex.lab = 2, line=3.5, family="zh")
    showtext_end()
    
    if (test) dev.off()
}

OUTPUT = "test.pdf"
# Testing
# d = d[680:696, ]
categories = levels(dm$category) #[17:19]
d2 = lapply(categories, \(x) dm %>% filter(category == {{x}})) 
names(d2) = categories

nc = 4
nr = 4
pdf(OUTPUT, width = 5.2 * nc, height = 3.5 * nr)
pb <- progress_bar$new(total = length(categories))
base_cols = seq(categories)
base_cols[seq(1,19,by=8)] = 2  # Switch order of col=1,2
base_cols[seq(2,19,by=8)] = 1  # Switch order of col=1,2
for (j in seq(categories)) {
    category = categories[j]
    d = d2[[category]]
    
    # pdf(f("made/distribution_per_word/{category}.pdf"), width = 5.2 * nc, height = 3.5 * nr)
    mar = c(5, 4.9, 5.5, 2.8)
    par(mfrow=c(nr, nc), oma=c(0,1,0,0), mar=mar)
    for (i in 1:nrow(d)) {
        plot_word(i, base_col = base_cols[j])
    }
    pb$tick()
    # dev.off()
}
dev.off()

