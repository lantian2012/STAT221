library(mvtnorm)
source('lan_tian_ps3_functions.R')

sample.data <- function(dim.n, dim.p, model="gaussian") {
  # Samples the covariates as normal with the specific correlation
  
  # Create A matrix (variance of the covariates xn)
  Q = random.orthogonal(p=dim.p)
  lambdas = seq(0.01, 1, length.out=dim.p)
  A = Q %*% diag(lambdas) %*% t(Q)
  
  X = rmvnorm(dim.n, mean=rep(0, dim.p), sigma=A)
  theta = matrix(1, ncol=1, nrow=dim.p)
  epsilon = rnorm(dim.n, mean=0, sd=1)
  # Data generation
  y = X %*% theta  + epsilon
  
  return(list(Y=y, X=X, A=A, theta=theta))
}

check.data <- function(data) {
  # Do this to check the data object.
  # 
  nx = nrow(data$X)
  ny = length(data$Y)
  p = ncol(data$X)
  stopifnot(nx==ny, p==length(data$theta))
  lambdas = eigen(cov(data$X))$values
  print(lambdas)
  print(mean(data$Y))
  print(var(data$Y))
  print(1 + sum(cov(data$X)))
}

plot.risk <- function(data, est) {
  # est = p x niters 
  est.bias = apply(est, 2, function(colum) 
    log(t(colum-data$theta) %*% data$A %*% (colum-data$theta)))
  
  plot(est.bias, type="l", lty=3)
}

sgd <- function(data, plot=T) {
  # check.data(data)
  n = nrow(data$X)
  p = ncol(data$X)
  # matrix of estimates of SGD (p x iters)
  theta.sgd = matrix(0, nrow = p, ncol = n)
  # params for the learning rate seq.
  gamma0 = 1 / (sum(seq(0.01, 1, length.out=p)))
  lambda0 = 0.01
  
  for(i in 1:(n-1)) {
    xi = data$X[i, ]
    theta.old = theta.sgd[, i]
    ai = gamma0 / (1 + gamma0 * lambda0 * i)
    # make computations easier.
    lpred = sum(theta.old * xi)
    theta.new = (theta.old - ai * lpred * xi) + ai * data$Y[i] * xi
    theta.sgd[, i+1] = theta.new
  }
  if(plot) {
    plot.risk(data, theta.sgd)
  } else {
    return(theta.sgd)
  }
}

sgd.im <- function(data, plot=T) {
  # check.data(data)
  n = nrow(data$X)
  p = ncol(data$X)
  # matrix of estimates of SGD (p x iters)
  theta.sgd = matrix(0, nrow = p, ncol = n)
  # params for the learning rate seq.
  gamma0 = 1 / (sum(seq(0.01, 1, length.out=p)))
  lambda0 = 0.01
  
  for(i in 1:(n-1)) {
    xi = data$X[i, ]
    theta.old = theta.sgd[, i]
    ai = 1 / ( lambda0 + lambda0 * i)
    # make computations easier.
    inner = sum(xi^2)
    ratio = 1/(1+inner*ai)
    lpred = sum(theta.old * xi)
    theta.new = theta.old + ai*(data$Y[i]-lpred*ratio-ai*data$Y[i]*ratio*inner)*xi
    theta.sgd[, i+1] = theta.new
  }
  if(plot) {
    plot.risk(data, theta.sgd)
  } else {
    return(theta.sgd)
  }
}


asgd <- function(data, plot=T) {
  # check.data(data)
  n = nrow(data$X)
  p = ncol(data$X)
  # matrix of estimates of SGD (p x iters)
  theta.asgd = matrix(0, nrow = p, ncol = n)
  theta.sgd = matrix(0, nrow = p, ncol = 1)
  # params for the learning rate seq.
  gamma0 = 1 / (sum(seq(0.01, 1, length.out=p)))
  lambda0 = 0.01  
  for(i in 1:(n-1)) {
    xi = data$X[i, ]
    theta.old = theta.sgd[, 1]
    ai = gamma0 / (1 + gamma0 * lambda0 * i)
    # make computations easier.
    lpred = sum(theta.old * xi)
    theta.new = (theta.old - ai * lpred * xi) + ai * data$Y[i] * xi
    theta.sgd[, 1] = theta.new
    theta.asgd[, i+1] = (1-(1/(i+1)))*theta.asgd[, i] + 1/(i+1)*theta.new
  }
  if(plot) {
    plot.risk(data, theta.asgd)
  } else {
    return(theta.asgd)
  }
}

batch <- function(data, plot=T) {
  # check.data(data)
  n = nrow(data$X)
  p = ncol(data$X)
  # matrix of estimates of SGD (p x iters)
  theta.batch = matrix(0, nrow = p, ncol = n)
  for(i in 1:n) {
    if (i > 100){
      if (i %% 1000 == 0){
        fit = lm(data$Y[1:i] ~ data$X[1:i, ])
        theta.batch[, i] = coef(fit)[2:(p+1)]
      }
      else{
        theta.batch[, i] = theta.batch[, i-1]
      }
    }
  }
  if(plot) {
    plot.risk(data, theta.batch)
  } else {
    return(theta.batch)
  }
}

d = sample.data(1e5, 100)
theta.sgd = sgd(d, F)
theta.sgd.im = sgd.im(d, F)
theta.asgd = asgd(d, F)
theta.batch = batch(d, F)
plot.all(d, list('sgd'=theta.sgd, 'sgd.im'=theta.sgd.im, 'asgd'=theta.asgd, 'batch'=theta.batch))


