#import "@local/yongfu:1.0.0": *
#show: Math

$
up(Y)_i                &tilde "Bernoulli"(p) \ 
"logit"(p)         &= D_(w[i]) ("Age"_i - "AoA"_(w[i])) \ 
$

$ "[Priors]" $

$
48 #h(2pt) "AoA"_w &tilde "Beta"(1.5,1.5) #h(8pt) script(("AoA"_w in "[0,48]")) \ 
D_(w[i])           &tilde "Exponential"(1)
$
