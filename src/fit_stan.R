# Rscript this.R INPUT OUTPUT1 OUTPUT2 MAX_AoA
args = commandArgs(TRUE)
INPUT = "made/MCDI.age-understanding.csv"
OUTPUT = c(
    "made/dat.fit.RDS",
    "made/model.RDS"
)
MAX_AoA = 48
if (length(args) > 0) {
    if (length(args) != 4) stop("Should have 3 argument: INPUT OUTPUT1 OUTPUT2 MAX_AoA")
    INPUT = args[1]
    OUTPUT = args[2:3]
    MAX_AoA = as.numeric(args[4])
}
########################################################

library(dplyr)
d = readr::read_csv(INPUT)

Age_rng = strsplit(d$age_rng[1], "|", fixed=T)[[1]] |> as.integer()
Age_rng = Age_rng[1]:Age_rng[2]
dat = list(
    # Metadata
    # words    = d$definition,
    # category = d$category,
    # age.50   = d$age.50,
    # Data
    nw      = nrow(d),
    n_obs   = nrow(d) * length(Age_rng),
    Y       = lapply(1:nrow(d), \(i) strsplit(d$age_distr[i], "|", fixed=T)[[1]]) |> unlist() |> as.integer(),
    N       = lapply(1:nrow(d), \(i) strsplit(d$age_distr_base[i], "|", fixed=T)[[1]]) |> unlist() |> as.integer(),
    wid     = lapply(1:nrow(d), \(i) rep(i, length(Age_rng)) ) |> unlist(),
    Age     = lapply(1:nrow(d), \(i) Age_rng ) |> unlist(),
    max_AoA = MAX_AoA
)
str(dat)
saveRDS(dat, OUTPUT[1])

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
m = cmdstanr::cmdstan_model("src/model.stan")
fit = m$sample(data = dat, chains = 4, parallel_chains = 4,
               iter_warmup = 500, iter_sampling = 300,
               save_warmup = TRUE, refresh = 200,
               init = write_init_files())
fit$save_object(OUTPUT[2])
