# função massa de probabilidade da dist binom negativa
fmp <- function(x, k, p){
  return(choose(x-1, k-1) * p^k * (1-p)^(x-k))
}

# P[X <= x] da binom negativa
pnegbinom <- function(q, k, p){
  sum(fmp(k:q, k, p))
}
