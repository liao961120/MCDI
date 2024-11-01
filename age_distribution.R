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


dm$age_distr = sapply(1:nrow(dm), \(i) {
    G = dm$item.WG[i]
    S = dm$item.WS[i]
    idx_produce_G = c()
    idx_produce_S = c()
    if (!is.na(G)) idx_produce_G = which(dWG[[G]] == 1)
    if (!is.na(S)) idx_produce_S = which(dWS[[S]] == 1)
    ages = c( dWG$age[idx_produce_G],
              dWS$age[idx_produce_S] ) |> sort()
    return(paste(ages, collapse="|"))
})


dm$n = sapply(1:nrow(dm), \(i) {
    strsplit(dm$age_distr[i], "|", fixed=T)[[1]] |> length()
})
dm$age.m = sapply(1:nrow(dm), \(i) {
    strsplit(dm$age_distr[i], "|", fixed=T)[[1]] |> as.numeric() |> mean()
})
dm$age.50 = sapply(1:nrow(dm), \(i) {
    strsplit(dm$age_distr[i], "|", fixed=T)[[1]] |> as.numeric()  |> median()
})
dm$age.80 = sapply(1:nrow(dm), \(i) {
    strsplit(dm$age_distr[i], "|", fixed=T)[[1]] |> as.numeric()  |> quantile(.8)
})
dm$age.20 = sapply(1:nrow(dm), \(i) {
    strsplit(dm$age_distr[i], "|", fixed=T)[[1]] |> as.numeric()  |> quantile(.2)
})

dm = dm %>% 
    filter(n > 50) %>%   # ID:189,182,185 samp_size <= 8
    select(everything(), -age_distr, age_distr)

dm = dm %>% 
    arrange(age.m, age.80)


dp = list(
    dm[1:232,],
    dm[233:464,],
    dm[465:696,]
)

plot_AoA = function(d) {
    plot(1, type="n", xlim=range(d$age.20-1, d$age.80+1), ylim=c(1,nrow(d)), yaxt='n', 
         xaxt="n", xlab="Age (month)", ylab="")
    abline(v = seq(15, 36, by=1), col="lightgrey" )
    for (i in 1:nrow(d)) {
        y_ = c(i,i)
        x_ = c(d$age.20[i], d$age.80[i])
        m_ = d$age.m[i]
        lines(x_, y_, lwd=1, col=stom::col.alpha(2,.4))
        points(m_, y_[1], pch=19, col=2, cex=.5)
    }
    axis(1, at=seq(16,36,by=2), labels=seq(16,36,by=2))
    axis(3, at=seq(16,36,by=2), labels=seq(16,36,by=2), padj = 1.5, cex.axis=1, , lwd.ticks = 0)
    axis(2, at=(1:nrow(d)), labels=d$definition, las=1, cex.axis=.8, 
         lwd.ticks = 0, hadj = 1)
}

svglite::svglite("made/AoA-all.svg", width = 9, height = 10)
plot_AoA(dm)
title(main = "Age of Acquisition of 696 Words Derived from MCDI")
title(sub = "Bars cover 20%-80% age distributions. Dots are age averages.", adj=0, cex.sub=.8)
dev.off()

svglite::svglite("made/AoA.svg", width = 12, height = 6)
par(mfrow=c(1,3))

for (j in 1:3) {
    plot_AoA(dp[[j]])
}
dev.off()


readr::write_csv(dm, "made/MCDI.age.csv")
writexl::write_xlsx(dm, "MCDI.age.xlsx")
