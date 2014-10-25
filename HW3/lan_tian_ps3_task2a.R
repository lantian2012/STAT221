source('lan_tian_ps3_functions.R')
#library(ggplot2)
sample.data2a <-function(dim.n){
  Q = random.orthogonal(100)
  lambdas = c(rep(1, 3), rep(0.02, 97))
  A = Q %*% diag(lambdas) %*% t(Q)
  X = matrix(rnorm(100*dim.n), nrow = dim.n)
  return (list(A=A, X=X))
}

plot.risk2a <- function(data, est) {
  # est = p x niters 
  est.bias = apply(est, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  x = 1:(length(est.bias))
  plot(x, est.bias, type="l", lty=3)
}


sgd<-function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.sgd = matrix(1, nrow = p, ncol = n+1)
  #params for learning rate
  gamma0 = 1
  lambda0 = 0.02
  
  for (i in 1:n){
    xi = data$X[i, ]
    #learning rate
    ai = gamma0 / (1 + gamma0 * lambda0 * i)
    theta.old = theta.sgd[, i]
    #update
    theta.new = theta.old + ai*(2*data$A %*% (xi-theta.old))
    theta.sgd[, i+1] = theta.new
  }
  if (plot){
    plot.risk2a(data, theta.sgd)
  }
  else{
    return (theta.sgd)
  }
}

sgd.im<-function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.sgd = matrix(1, nrow = p, ncol = n+1)
  gamma0 = 1
  lambda0 = 0.02
  for (i in 1:n){
    xi = data$X[i, ]
    theta.old = theta.sgd[, i]
    ai = gamma0 / (1 + gamma0 * lambda0 * i)
    theta.new = solve(diag(p) + ai*2*data$A)%*%(theta.old + ai*(2*data$A %*% xi))
    theta.sgd[, i+1] = theta.new
  }
  if (plot){
    plot.risk2a(data, theta.sgd)
  }
  else{
    return (theta.sgd)
  }
}

asgd<-function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.asgd = matrix(1, nrow = p, ncol = n+1)
  theta.sgd = matrix(0, nrow = p, ncol = 1)
  a = (1+0.02*seq(1, n))^(-2/3)
  for (i in 1:n){
    xi = data$X[i, ]
    theta.old = theta.sgd[, 1]
    theta.new = theta.old + a[i]*(2*data$A %*% (xi - theta.old))
    theta.sgd[, 1] = theta.new
    #weigh the current update with previous ones
    theta.new = (1-1/(i+1))*theta.asgd[, i] + 1/(i+1)*theta.new
    theta.asgd[, i+1] = theta.new
  }
  if (plot){
    plot.risk2a(data, theta.asgd)
  }
  else{
    return (theta.asgd)
  }
}

asgd.bad<-function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.asgd = matrix(1, nrow = p, ncol = n+1)
  theta.sgd = matrix(0, nrow = p, ncol = 1)
  a = (1+seq(1, n))^(-0.5)
  for (i in 1:n){
    xi = data$X[i, ]
    theta.old = theta.sgd[, 1]
    theta.new = theta.old + a[i]*(2*data$A %*% (xi - theta.old))
    theta.sgd[, 1] = theta.new
    theta.new = (1-1/(i+1))*theta.asgd[, i] + 1/(i+1)*theta.new
    theta.asgd[, i+1] = theta.new
  }
  if (plot){
    plot.risk2a(data, theta.asgd)
  }
  else{
    return (theta.asgd)
  }
}

batch <- function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.batch = matrix(0, nrow = p, ncol = n+1)
  for (i in 1:n){
    xi = data$X[i, ]
    if (i == 1){
      theta.new = xi
    }
    else{
      theta.old = theta.batch[, i-1]
      theta.new = (1-1/i)*theta.old + 1/i*xi
    }
    theta.batch[, i] = theta.new
  }
  if (plot){
    plot.risk2a(data, theta.batch)
  }
  else{
    return (theta.batch)
  }
}

select = seq(1, 1e6, 50)
d = sample.data2a(1e6)
theta.sgd = sgd(d, F)[, select]
theta.sgd.im = sgd.im(d, F)[, select]
theta.asgd = asgd(d, F)[, select]
theta.asgd.bad = asgd.bad(d, F)[, select]
theta.batch = batch(d, F)[, select]
d = list(d$A)
save(d, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im, file="out/task2a.rda")
