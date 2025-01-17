library(stom)
library(dplyr)

fit = readRDS("made/model-production.RDS")

D = precis(fit, depth=2, pars="D")
aoa = precis(fit, depth=2, pars="AoA")





nw = 696

plot(1, type="n", xlim=c(0,697), ylim=range(D$q5,D$q95),
     xlab="Word ID", ylab="D")
for (i in 1:nw) {
    x_ = rep(i,2)
    y_ = c(D$q5[i], D$q95[i])
    points(x_[1], D$mean[i], col=2)
    lines(x_, y_, col=col.alpha(2), lwd=5)
}


plot(1, type="n", xlim=c(0,697), ylim=range(aoa$q5,aoa$q95),
     xlab="Word ID", ylab="AoA")
for (i in 1:nw) {
    x_ = rep(i,2)
    y_ = c(aoa$q5[i], aoa$q95[i])
    points(x_[1], aoa$mean[i], col=2)
    lines(x_, y_, col=col.alpha(2), lwd=3)
}


plot(aoa$mean, aoa$sd)
