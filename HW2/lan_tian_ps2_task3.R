#PSet 2  Task 3
args <- as.numeric(commandArgs(trailingOnly = TRUE))

if(length(args) != 1) {
  stop("Not correct no. of args")
}
job.id = args[1]

source("poissonLogN_MCMC.R")
source("Lan_Tian_ps2_functions.R")

mu = c(1.6, 2.5, 5.2, 4.9)
std = c(0.7, 1.3, 1.3, 1.6)
Ntheta = 2 #number of theta drawn from each (mu,std) pair  4
Ny = 25   #number of Y drawn from each theta  25
J = 1000
w = rep(1, J)
logthetaList = list()
coverage95List = list()
coverage68List = list()
meansList = list()
stdsList = list()
for (i in 1:length(mu)){
  coverage95 = matrix(nrow = J, ncol = Ntheta)
  coverage68 = matrix(nrow = J, ncol = Ntheta)
  logtheta = matrix(nrow = J, ncol = Ntheta)
  means = matrix(nrow = J, ncol = (Ntheta*Ny))
  stds = matrix(nrow = J, ncol = (Ntheta*Ny))
  for (nt in 1:Ntheta){
    logtheta0 = rnorm(J, mean = mu[i], sd = std[i])
    Covered95 = matrix(nrow=J, ncol=Ny)
    Covered68 = matrix(nrow=J, ncol=Ny)
    for (ny in 1:Ny){
      theta0 = exp(logtheta0)
      Y = simYgivenTheta(theta0, w, 2)
      postlogtheta = poisson.logn.mcmc(Y, w)[["logTheta"]]
      Covered95[,ny] = isCovered95(postlogtheta, logtheta0)
      Covered68[,ny] = isCovered68(postlogtheta, logtheta0)
      means[, (nt-1)*Ny+ny] = rowMeans(postlogtheta)
      stds[, (nt-1)*Ny+ny] = apply(postlogtheta, 1, sd)
    }
    coverage95[,nt] = rowSums(Covered95)/Ny
    coverage68[,nt] = rowSums(Covered68)/Ny
    logtheta[,nt] = logtheta0
  }
  logthetaList[[i]] = logtheta
  coverage95List[[i]] = coverage95
  coverage68List[[i]] = coverage68
  meansList[[i]] = means
  stdsList[[i]] = stds
}
#logthetaList:each element is a matrix of logtheta(1 (mu, sigma) pair), each column 25 sim combined
#coverage95List:each element is a matrix(1 (mu, sigma) pair), each column the coverage by 25 sim combined
save(logthetaList, coverage95List, coverage68List, meansList, stdsList, file=sprintf("out/coverage_%d.rda", job.id))