INPUT = c(
    "made/MCDI.age-production.csv"
)
OUTDIR = c(
    "made/distr"
)
YLAB = c(
    " 會說的比例(%)"
)

###########################################



library(dplyr)
library(showtext)
library(progress)
f = glue::glue

font_paths("./raw/JasonHandwriting-master/")
font_add("zh", regular = "raw/JasonHandwriting-master/JasonHandwriting3p.ttf")
font_add_google("Gochi Hand", "gochi")

d = readr::read_csv(INPUT) %>%
    mutate(category = factor(category, levels=c('people', 'games_routines', 
                                                'food_drink', 'action_words', 
                                                'animals', 'vehicles', 'toys', 
                                                'clothing', 'outside', 
                                                'body_parts', 'household', 
                                                'descriptive_words', 
                                                'furniture_rooms', 
                                                'quantifiers', 'locations', 
                                                'places', 'time_words', 
                                                'question_words', 
                                                'connecting_words')  )) %>% 
    arrange(category, age.50) %>% 
    mutate(wid = 1:nrow(.))

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
            lines(x_, y_, lwd=10, col=c_)
    }
    
    axis(1, at=seq(x_axis_lab), labels = x_axis_lab, family="gochi", 
         cex.axis=1.4, padj=-1.05, col.ticks = "white")
    axis(2, las=1, family="gochi", cex.axis=1.5)
    box(bty='L')
    
    nchr = nchar(word)
    if (nchr > 10) {
        title(main=word, cex.main=2.5, line=1.5, family="zh")
    } 
    else if (nchr <= 2)
        title(main=word, cex.main=4, line=1.5, family="zh")
    else {
        title(main=word, cex.main=4, line=1.5, family="zh")
    }
        
    title(xlab="月齡", cex.lab = 2, line=2.8, family="zh")
    title(ylab=YLAB, cex.lab = 2, line=3.5, family="zh")
    showtext_end()
    
    if (test) dev.off()
}



d %>% 
    select(wid, definition, age_distr_prob) %>% 
    readr::write_csv("made/distr/map.csv")

# Testing
# d = d[680:696, ]
categories = levels(d$category) #[17:19]
d2 = lapply(categories, \(x) d %>% filter(category == {{x}})) 
names(d2) = categories

nc = 4
nr = 4

pb <- progress_bar$new(total = length(categories))
base_cols = seq(categories)
base_cols[seq(1,19,by=8)] = 2  # Switch order of col=1,2
base_cols[seq(2,19,by=8)] = 1  # Switch order of col=1,2
fct = 2
fct_pt = 1.4
for (j in seq(categories)) {
    category = categories[j]
    d = d2[[category]]
    if (nrow(d) == 0) next

    # # pdf(f("made/distribution_per_word/{category}.pdf"), width = 5.2 * nc, height = 3.5 * nr)
    # mar = c(5, 4.9, 5.5, 2.8)
    for (i in 1:nrow(d)) {
        wid = d$wid[i]
        fp = file.path(OUTDIR, f("{wid}.png"))
        # svglite::svglite(fp, width = 5.2 , height = 3.5)
        png(fp, width = 480*fct , height = 280*fct, pointsize=12*fct_pt)
        par(mai=stom::as_vec("1.12 1.5 1.32 0.42"), oma=c(0,.3,0,0))
        plot_word(i, base_col = base_cols[j])
        dev.off()
    }
    pb$tick()
}

# png("test.png", width = 500*fct , height = 300*fct, pointsize=12*fct_pt)
# 
# plot_word(i, base_col = base_cols[j])
# dev.off()

