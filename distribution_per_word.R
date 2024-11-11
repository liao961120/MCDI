library(dplyr)
library(showtext)
library(progress)

font_paths("./raw/JasonHandwriting-master/")
font_add("zh", regular = "raw/JasonHandwriting-master/JasonHandwriting3p.ttf")
font_add_google("Gochi Hand", "gochi")

d = readr::read_csv("made/MCDI.age.csv")

plot_word = function(idx) {
    word  = d$definition[idx]
    distr = strsplit(d$age_distr_prob[idx], "|", fixed=T)[[1]] |> as.numeric()
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
        c_ = stom::col.alpha(1,.65)
        if (length(highlight_idx) == 1 && i == highlight_idx) 
            c_ = stom::col.alpha(2,.8)
        lines(x_, y_, lwd=6, col=c_)
    }
    
    axis(1, at=seq(x_axis_lab), labels = x_axis_lab, family="gochi", 
         cex.axis=1.4, padj=-1.05, col.ticks = "white")
    axis(2, las=1, family="gochi", cex.axis=1.5)
    box(bty='L')
    title(main=word, cex.main=3, line=2, family="zh")
    title(xlab="月齡", cex.lab = 2, line=2.8, family="zh")
    title(ylab=" 會說的比例 (%)", cex.lab = 2, line=3.5, family="zh")
    showtext_end()
}

# Testing
# d = d[680:696, ]

nc = 4
nr = 3
pb <- progress_bar$new(total = nrow(d))

pdf("distribution_per_word.pdf", width = 5.2 * nc, height = 3.5 * nr)
mar = c(5, 4.9, 5.5, 2.8)

par(mfrow=c(nr, nc), oma=c(0,1,0,0), mar=mar)
for (i in 1:nrow(d)) {
    plot_word(i)
    pb$tick()
}
    
dev.off()
