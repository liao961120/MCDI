data {
    int nw;     // num of words
    int n_obs;  // num of observations
    array[n_obs] int<lower=0> Y;    // produced counts under N trials
    array[n_obs] int<lower=1> N;    // num of trials
    array[n_obs] int<lower=1> wid;  // word id
    array[n_obs] real<lower=0> Age; // child age
    real<lower=0> max_AoA;
}
parameters {
    vector<lower=0>[nw] D;  // Discrimination of acquisition
    vector<lower=0,upper=1>[nw] AoA_raw; 
}
transformed parameters {
    vector[nw] AoA = AoA_raw * max_AoA;  // Age of acquisition of words
}
model {
    D ~ exponential(1);
    AoA_raw ~ beta(1.5,1.5);

    for (i in 1:n_obs) {
        int w = wid[i];
        Y[i] ~ binomial_logit(N[i], D[w] * (Age[i] - AoA[w]));
    }
}
generated quantities {
    vector[nw] AoA_75 = logit(.75) / D + AoA;
    vector[nw] AoA_25 = logit(.25) / D + AoA;
}
