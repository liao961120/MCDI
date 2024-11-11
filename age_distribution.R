library(dplyr)

dm = readr::read_csv("made/word_id.csv")

# Production only
dWG = readr::read_csv("raw/MandarinTaiwaneseWG_Liu_data.csv") %>% 
    filter(dis == 0) %>% 
    select(sub, age, sex, d01_01p:d17_06p)
names(dWG)[-(1:3)] = names(dWG)[-(1:3)] |> substr(1,6)
dWS = readr::read_csv("raw/MandarinTaiwaneseWS_Liu_data.csv") %>% 
    filter(dis == 0) %>% 
    select(sub, age, sex, a01_01:a19_13)
base_age = c( dWS$age, dWG$age ) |> table()
base_age_levels = names(base_age)

dm$age_distr = sapply(1:nrow(dm), \(i) {
    G = dm$item.WG[i]
    S = dm$item.WS[i]
    idx_produce_G = c()
    idx_produce_S = c()
    if (!is.na(G)) idx_produce_G = which(dWG[[G]] == 1)
    if (!is.na(S)) idx_produce_S = which(dWS[[S]] == 1)
    ages = c( dWG$age[idx_produce_G],
              dWS$age[idx_produce_S] ) |> 
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
dm$age.70 = word_age(dm$age_distr, cuttoff=.7, Max=40)
dm$age.50 = word_age(dm$age_distr, cuttoff=.5, Max=40)
dm$age.30 = word_age(dm$age_distr, cuttoff=.3, Max=40)

dm = dm %>% 
    mutate(n = sapply(strsplit(age_distr, "|", fixed=T), \(x) sum(as.integer(x))) ) %>% 
    filter(n > 50) %>%   # ID:189,182,185 samp_size <= 8
    select(everything(), -age_distr, -age_distr_prob, 
           age_distr, age_distr_prob) %>% 
    arrange(age.50, age.70, age.30)






plot_AoA = function(d) {
    plot(1, type="n", xlim=range(d$age.30-1, d$age.70+1), ylim=c(1,nrow(d)), yaxt='n', 
         xaxt="n", xlab="Age (month)", ylab="")
    abline(v = seq(12, 36, by=1), col="lightgrey" )
    for (i in 1:nrow(d)) {
        y_ = c(i,i)
        x_ = c(d$age.30[i], d$age.70[i])
        m_ = d$age.50[i]
        lines(x_, y_, lwd=1, col=stom::col.alpha(2,.4))
        points(m_, y_[1], pch=19, col=2, cex=.5)
    }
    axis(1, at=seq(12,36,by=2), labels=seq(12,36,by=2))
    axis(3, at=seq(12,36,by=2), labels=seq(12,36,by=2), padj = 1.5, cex.axis=1, , lwd.ticks = 0)
    axis(2, at=(1:nrow(d)), labels=d$definition, las=1, cex.axis=.8, 
         lwd.ticks = 0, hadj = 1)
}

svglite::svglite("made/AoA-all.svg", width = 9, height = 10)
plot_AoA(dm)
title(main = "Age of Acquisition of 696 Words Derived from MCDI")
title(sub = "Dots are the age groups at which 50% (interpolated) of the children produced the words.", 
      adj=0, cex.sub=.8)
dev.off()

dp = list(
    dm[1:233,],
    dm[234:464,],
    dm[465:696,]
)
svglite::svglite("made/AoA.svg", width = 12, height = 6)
par(mfrow=c(1,3))

for (j in 1:3) {
    plot_AoA(dp[[j]])
}
dev.off()


readr::write_csv(dm, "made/MCDI.age.csv")
writexl::write_xlsx(dm, "MCDI.age.xlsx")
