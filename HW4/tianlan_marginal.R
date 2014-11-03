#R code for 1.5 Pset4

data0 = read.table('waterbuck.txt', header=T)$waterbuck
data1 = read.table('impala.txt', header=T)$impala

prob <- function(N, data, log=F){
  result = 0
  S = sum(data)
  n = length(data)
  for (i in 1:n){
    result = result + lchoose(N, data[i])
  }
  #result = result/N*beta(S+1, n*N-S+1)
  result = result - log(N) + lbeta(S+1, n*N-S+1)
  if (log){
    return result
  }
  exp(result)
}

margin <- function(data, log=F){
  diff = 100
  result = 0
  N = max(data)
  count = 0
  while(diff > 1e-18){
    diff = prob(N, data)
    result = result + diff
    count = count + 1
    N = N+1
  }
  result
}

prob.theory <- function(data, margin){
  N = max(data)
  result = 0
  for (n in N:100){
    result = result + prob(n, data)/margin
  }
  return (1 - result)
}

margin.waterbuck = margin(data0)
margin.impala = margin(data1)

mcmc.waterbuck = mcmc.chain[(nrow(mcmc.chain)*0.3):nrow(mcmc.chain), ]
p.mcmc.waterbuck = sum(mcmc.waterbuck>100)/nrow(mcmc.waterbuck)
p.theory.waterbuck = prob.theory(data0, margin.waterbuck)

mcmc.impala = mcmc.chain[(nrow(mcmc.chain)*0.3):nrow(mcmc.chain), ]
p.mcmc.impala = sum(mcmc.impala>100)/nrow(mcmc.impala)
p.theory.impala = prob.theory(data1, margin.impala)


