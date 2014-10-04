#PSet 2  Task 4
args <- as.numeric(commandArgs(trailingOnly = TRUE))

if(length(args) != 1) {
  stop("Not correct no. of args")
}
job.id = args[1]

source("poissonLogN_MCMC.R")
source("Lan_Tian_ps2_functions.R")
w = read.table('weights.txt')[[1]]

select = job.id%%4
if (select == 0)
  select = 4
mu = c(1.6, 2.5, 5.2, 4.9)
std = c(0.7, 1.3, 1.3, 1.6)
mu = mu[select]
std = std[select]

Ntheta = 3 #number of theta drawn
Ny = 40   #number of Y drawn from each theta
J = 1000

coverage95 = matrix(nrow = J, ncol = Ntheta)
coverage68 = matrix(nrow = J, ncol = Ntheta)
logtheta = matrix(nrow = J, ncol = Ntheta)
means = matrix(nrow = J, ncol = (Ntheta*Ny))
stds = matrix(nrow = J, ncol = (Ntheta*Ny))
for (nt in 1:Ntheta){
  logtheta0 = rnorm(J, mean = mu, sd = std)
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
#logtheta a matrix of logtheta, each column 25 sim combined
#coverage95 a matrix, each column the coverage by 25 sim combined
save(job.id, logtheta, coverage95, coverage68, means, stds, file=sprintf("out/task4_coverage_%d.rda", job.id))
