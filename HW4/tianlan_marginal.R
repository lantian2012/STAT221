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
    return (result)
  }
  exp(result)
}

margin <- function(data, log=F){
  diff = 100
  result = 0
  N = max(data)
  count = 0
  last = 0
  while(diff > 1e-5){
    result = result + prob(N, data)
    N = N+1
    diff = 1/last - 1/result
    last = result
    count = count + 1
  }
  print(count)
  1/result
}

prob.theory <- function(data, margin){
  N = max(data)
  result = 0
  for (n in N:100){
    result = result + prob(n, data)*margin
  }
  return (1 - result)
}

mcmc.waterbuck = matrix(ncol=1, nrow=10)
accept.waterbuck = matrix(ncol=1, nrow=10)
for (j in 1:10){
  name = paste('mcmc_job_', j, '.rda',sep="")
  load(name)
  temp = mcmc.chain[(nrow(mcmc.chain)*0.3):(nrow(mcmc.chain)), 1]
  temp = sum(temp>100)/length(temp)
  mcmc.waterbuck[j] = temp
  accept.waterbuck[j] = accept
}
p.mcmc.waterbuck = mean(mcmc.waterbuck)
p.mcmc.waterbuck.std = sd(mcmc.waterbuck)

mcmc.impala = matrix(ncol=1, nrow=10)
accept.impala = matrix(ncol=1, nrow=10)
for (j in 11:20){
  name = paste('mcmc_job_', j, '.rda',sep="")
  load(name)
  temp = mcmc.chain[(nrow(mcmc.chain)*0.3):(nrow(mcmc.chain)), 1]
  temp = sum(temp>100)/length(temp)
  mcmc.impala[j-10] = temp
  accept.impala[j-10] = accept
}
p.mcmc.impala = mean(mcmc.impala)
p.mcmc.impala.std = sd(mcmc.impala)

margin.waterbuck = margin(data0)
margin.impala = margin(data1)

p.theory.waterbuck = prob.theory(data0, margin.waterbuck)
p.theory.impala = prob.theory(data1, margin.impala)


