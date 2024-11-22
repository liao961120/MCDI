library(stom)
library(dplyr)

d0 = readr::read_csv("made/MCDI.age-production.csv")
fit = readRDS("made/model-production.RDS")

####################
##### Diagnose #####
####################
# library(cmdstanr)
# ds = precis(fit, pars = c("D", "AoA"), depth = 3)
# plot(ds$rhat)

AoA.25 = precis(fit, pars="AoA_25")
AoA.75 = precis(fit, pars="AoA_75")
AoA = precis(fit, pars="AoA")
D   = precis(fit, pars="D")

# plot(1, type="n", xlab = "Empirical", ylab="Model-based Estimate",
#      xlim=c(8,40), ylim=c(8,40))
# abline(0,1, col="grey")
# abline(v=36, col="grey", lty="dashed")
# abline(h=36, col="grey", lty="dashed")
# for (i in 1:nrow(AoA)) {
#     emp = d0$age.50[i]
#     est = AoA$mean[i]
#     est.q5 = AoA$q5[i]
#     est.q95 = AoA$q95[i]
#     lines( rep(emp,2), c(est.q5,est.q95), lwd=3, col=col.alpha(2) )
#     points(emp, est, pch=19, col=2)
# }

d1 = d0 %>% 
    select(-starts_with("age_distr"), -n) %>% 
    mutate(
        age.75.m = AoA.75$mean,
        age.50.m = AoA$mean,
        age.25.m = AoA.25$mean,
        D        = D$mean
    ) %>% 
    mutate(bin = case_when(
        age.50.m < 18                  ~  "12~18",
        18 <= age.50.m & age.50.m < 24 ~  "18~24",
        24 <= age.50.m & age.50.m < 30 ~  "24~30",
        30 <= age.50.m & age.50.m < 36 ~  "30~36",
        TRUE                           ~  "大於36"
    )) %>%
    mutate(bin = ordered(bin, levels = c("12~18","18~24","24~30","30~36","大於36") )) %>%
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
    arrange(bin, category, age.50.m, D) %>% 
    select(-bin)

readr::write_csv(d1, "made/MCDI.age.m-produciton.csv")
writexl::write_xlsx(d1, "made/MCDI.age.m-produciton.xlsx")

# plot(d1$age.50, d1$age.50.m, pch=19, cex=.8,col=col.alpha(2,.6), xlab="Empirical", 
#      ylab="Model-based Estimate", main="AoA of Words from MCDI"); abline(0,1, h=36,v=36,col="grey")

d1 = d1 %>% arrange(age.50.m)
## Model-based AoA
plot_AoA = function(d) {
    hl = c(10, 46)
    plot(1, type="n", xlim=range(d$age.25.m-1, d$age.75.m+1), ylim=c(1,nrow(d)), 
         yaxt='n', xaxt="n", xlab="Age (month)", ylab="")
    abline(v = hl[1]:hl[2], col="lightgrey" )
    for (i in 1:nrow(d)) {
        y_ = c(i,i)
        x_ = c(d$age.25.m[i], d$age.75.m[i])
        m_ = d$age.50.m[i]
        lines(x_, y_, lwd=1, col=stom::col.alpha(2,.4))
        points(m_, y_[1], pch=19, col=2, cex=.5)
    }
    axis(1, at=seq(hl[1],hl[2],by=2), labels=seq(hl[1],hl[2],by=2))
    axis(3, at=seq(hl[1],hl[2],by=2), labels=seq(hl[1],hl[2],by=2), padj = 1.5, cex.axis=1, , lwd.ticks = 0)
    axis(2, at=(1:nrow(d)), labels=d$definition, las=1, cex.axis=.8, 
         lwd.ticks = 0, hadj = 1)
}

svglite::svglite("made/AoA-all.svg", width = 9, height = 10)
par(mai = c(1.25,0.7965714, 0.7965714, 0.4080000))
plot_AoA(d1)
title(main = "Age of Acquisition of 696 Words Derived from MCDI")
subs = c(
    "Dots are model-based estimates of the ages at which there is a 50% probability producing the words.",
    "The left and right ends of the bars represent the corresponding estimates for probabilities of 25% and 75%, respectively."
)
title(sub = subs[1], cex.sub=.8, adj=1)
title(sub = subs[2], cex.sub=.8, adj=1, line=4.8)
dev.off()

dp = list(
    d1[1:233,],
    d1[234:464,],
    d1[465:696,]
)
svglite::svglite("made/AoA.svg", width = 12, height = 6)
par(mfrow=c(1,3))

for (j in 1:3) {
    plot_AoA(dp[[j]])
}
dev.off()

