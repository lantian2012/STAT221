source("poissonLogN_MCMC.R")
source("Lan_Tian_ps2_functions.R")
library(compiler)
w = 1:1000
simYgivenTheta = cmpfun(simYgivenTheta)
logtheta0 = rnorm(1000, 1, 1)
theta0 = exp(logtheta0)
Y = simYgivenTheta(theta0, w, 2)
library(microbenchmark)
compare <- microbenchmark(simYgivenTheta(theta0, w, 2), times = 1000)
compare

test = poisson.logn.mcmc
thetaInit = cmpfun(thetaInit)
thetaMHStep = cmpfun(thetaMHStep)
test = cmpfun(poisson.logn.mcmc)
thetaPost = cmpfun(thetaPost)
dp.dtheta = cmpfun(dp.dtheta)
d2p.dtheta2 = cmpfun(d2p.dtheta2)
d3p.dtheta3 = cmpfun(d3p.dtheta3)
simYgivenTheta = cmpfun(simYgivenTheta)
poisson.logn.mcmc = cmpfun(poisson.logn.mcmc)

compare <- microbenchmark(poisson.logn.mcmc(Y,w), times = 5)
compare
