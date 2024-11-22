#import "@local/yongfu:1.0.0": *
#show: Math

$
up(Y)_i                &tilde "Bernoulli"(p) \ 
"logit"(p)         &= D_(w[i]) ("Age"_i - "AoA"_(w[i])) \ 
$

$ "[Priors]" $

$
"AoA"_w/48 &tilde "Beta"(1.5,1.5) #h(8pt) script(("AoA"_w in "[0,48]")) \ 
D_(w[i])           &tilde "Exponential"(1)
$


// GLM-based AoA estimates (Frank et al., 2021)
// $
// & Y_i        tilde "Bernoulli"(p) \ 
// & "logit"(p) = beta "Age"_i + k \ \
// & arrow.r.double underbrace("logit"(.5), 0) = beta "AoA"_w + k \ 
// & arrow.r.double "AoA"_w       = (-k)/beta
// $

