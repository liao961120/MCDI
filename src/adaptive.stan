data {
    int nw;        // num of words
    int n_obs;     // num of observations
    vector<lower=0>[nw] D;   // Discrimination of acquisition
    vector<lower=0>[nw] AoA; // Age of acquisition of word
    array[n_obs] int<lower=1> wid;  // word id
    array[n_obs] int<lower=0,upper=1> Y;  // Response
    real<lower=0> age_init;
}
parameters {
    real<lower=0> Age;
}
model {
    Age ~ normal(age_init, 8);

    for (i in 1:n_obs) {
        int w = wid[i];
        Y[i] ~ bernoulli_logit(D[w] * (Age - AoA[w]));
    }
}
generated quantities {
    // Compute item information based on posterior estimate of Age
    {   
        vector[nw] P = D * inv_logit(Age - AoA);
    }
    vector[nw] I = exp( 2*log(D) + log(P) + log1m(P) );
}
