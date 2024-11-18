library(dplyr)
d = readr::read_csv("made/MCDI.age.csv")

Age_rng = 8:36
dat = list(
    # Metadata
    # words    = d$definition,
    # category = d$category,
    # age.50   = d$age.50,
    # Data
    nw    = nrow(d),
    n_obs = nrow(d) * length(Age_rng),
    Y     = lapply(1:nrow(d), \(i) strsplit(d$age_distr[i], "|", fixed=T)[[1]]) |> unlist() |> as.integer(),
    N     = lapply(1:nrow(d), \(i) strsplit(d$age_distr_base[i], "|", fixed=T)[[1]]) |> unlist() |> as.integer(),
    wid   = lapply(1:nrow(d), \(i) rep(i, length(8:36)) ) |> unlist(),
    Age   = lapply(1:nrow(d), \(i) 8:36 ) |> unlist()
)
str(dat)
saveRDS(dat, "made/dat.fit.RDS")

nw = dat$nw
write_init_files = function(dir="init", chains=1:4) {
    sapply( chains, function(chain) {
        init = tibble::lst(
            D       = runif(nw, .1, 1),
            AoA_raw = runif(nw,  .1, 1)
        )
        dir.create(dir, showWarnings = F)
        fp = file.path(dir, paste0(chain,".json") )
        cmdstanr::write_stan_json(init, fp)
        fp
    })
}


set.seed(50)
m = cmdstanr::cmdstan_model("model.stan")
fit = m$sample(data = dat, chains = 4, parallel_chains = 4,
               iter_warmup = 500, iter_sampling = 300,
               save_warmup = TRUE, refresh = 200,
               init = write_init_files())
fit$save_object("made/model.RDS")
