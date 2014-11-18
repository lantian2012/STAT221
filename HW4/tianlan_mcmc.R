#MCMC for Stat221 Pset4

#select the dataset to run
args <- as.numeric(commandArgs(trailingOnly = TRUE))

if(length(args) != 1) {
  args[1] = 1
}
job.id = args[1]
select = 0
if (job.id > 10){
  select = 1
}


library(MASS)
library(scales)
sample.data <- function(n, N, theta) {
  rbinom(n, N, theta)
}

get.data <- function(n){
  #get waterbucks data if n==0
  #get impala data if n==1
  if (n==0){
    return (read.table('waterbuck.txt', header=T)$waterbuck)
  }
  if (n==1){
    return(read.table('impala.txt', header=T)$impala)
  }
}

log.lik <- function(N, theta, Y) {
  # Log-likelihood of the data
  sum(dbinom(Y, N, theta, log = T))
}

log.prior <- function(N, theta) {
  log(1/N)
}

log.posterior <- function(N, theta, Y) {
  log.lik(N, theta, Y) + log.prior(N, theta)
}

rpropose <- function(N.old, theta.old, y){
  S.old = N.old * theta.old
  S.new = rbeta(1, theta.old*c.s, c.s-theta.old*c.s)*N.old
  theta.new = rbeta(1, theta.old*c.t, c.t-theta.old*c.t)
  #theta.new = rbeta(1, 6, 6)
  N.new = ceiling(S.new/theta.new)
  while(N.new <= max(y) || N.new > 10000){
    theta.new = rbeta(1, theta.old*c.t, c.t-theta.old*c.t)
    #theta.new = rbeta(1, 6, 6)
    N.new = round(S.new/theta.new)
  }
  c(N.new, theta.new)
}
log.dpropose <- function(N.old, theta.old, N.new, theta.new){
  dbeta(theta.new, theta.old*c.t, c.t-theta.old*c.t, log=T)+
    dbeta(N.new*theta.new/N.old, theta.old*c.s, c.s-theta.old*c.s, log=T)
  #dbeta(theta.new, 6, 6, log=T)+
   # dbeta(N.new*theta.new/N.old, theta.old*c.s, c.s-theta.old*c.s, log=T)
}

rpropose1 <- function(N.old, theta.old, y){
  S.old = N.old * theta.old
  S.new = rbeta(1, theta.old*c.s, c.s-theta.old*c.s)*N.old
  N.new = rpois(1, N.old)
  #N.new = round(rnorm(1, N.old, 3))
  theta.new = S.new/N.new
  while(N.new <= max(y) || theta.new >= 1){
    N.new = rpois(1, N.old)
    #N.new = round(rnorm(1, N.old, 3))
    theta.new = S.new/N.new
  }
  #theta.new = min(1-1e-10, theta.new)
  c(N.new, theta.new)
}
log.dpropose1 <- function(N.old, theta.old, N.new, theta.new){
  dpois(N.new, N.old, log=T)+
    dbeta(N.new*theta.new/N.old, theta.old*c.s, c.s-theta.old*c.s, log=T)
  #dnorm(N.new, N.old, 3, log=T)+
    dbeta(N.new*theta.new/N.old, theta.old*c.s, c.s-theta.old*c.s, log=T)
}

plot.chain2 <- function(mcmc.chain){
  mcmc.niters = nrow(mcmc.chain)
  burnin = 0.3 * mcmc.niters
  mcmc.chain = mcmc.chain[burnin:mcmc.niters, ]
  cutoff = quantile(mcmc.chain, 0.90)
  mcmc.chain = data.frame(mcmc.chain)
  mcmc.chain = mcmc.chain[which(mcmc.chain$X1 < cutoff),]
  f = kde2d(x=mcmc.chain[, 1], y=mcmc.chain[, 2], n=100)
  plot(mcmc.chain$X1, mcmc.chain$X2, col = alpha('black', 0.005), xlab='N', ylab='theta')
  contour(f, col='red', lwd=2.5, add=TRUE)
}

mcmc <- function(y, mcmc.niters=1e5, rpropose, dpropose) {
  # Complete with MH.
  S = sum(y)
  n = length(y)
  y.max = max(y)
  mcmc.chain <- matrix(nrow=mcmc.niters, ncol=2)
  mcmc.chain[1, ] = c(max(ceiling(S/n*2), y.max), 0.5)
  nacc <- 0
  for(i in 2:mcmc.niters) {
    # 1. Current state
    N.old = mcmc.chain[i-1, 1]
    theta.old = mcmc.chain[i-1, 2]
    # 2. Propose new state
    param.new = rpropose(N.old, theta.old, y)
    N.new = param.new[1]
    theta.new = param.new[2]
    # 3. Ratio
    mh.ratio = min(0, log.posterior(N.new, theta.new, y) - 
                     log.posterior(N.old, theta.old, y) +
                     log.dpropose(N.new, theta.new, N.old, theta.old) -
                     log.dpropose(N.old, theta.old, N.new, theta.new))
    if(runif(1) < exp(mh.ratio)) {
      # Accept 
      mcmc.chain[i, ] <- c(N.new, theta.new)
      nacc <- nacc + 1
    } else {
      mcmc.chain[i, ] <- c(N.old, theta.old)
    }
  }
  # Cut the burnin period.
  print(sprintf("Acceptance ratio %.2f%%", 100 * nacc / mcmc.niters))
  #plot.chain2(mcmc.chain)
  return(list(mcmc.chain, 100 * nacc / mcmc.niters))
}



c.s = 1000
c.t = 400

data = get.data(select)
mcmc.chain = mcmc(data,mcmc.niters=1e6,rpropose = rpropose, dpropose = log.dpropose)
jpeg(filename=sprintf("mcmc_job_%d.jpg", job.id), width=900, height=600)
plot.chain2(mcmc.chain[[1]])
dev.off()
accept = mcmc.chain[[2]]
mcmc.chain = mcmc.chain[[1]]
save(accept, mcmc.chain, file=sprintf("mcmc_job_%d.rda", job.id))

