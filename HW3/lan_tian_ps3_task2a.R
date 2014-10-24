source('lan_tian_ps3_functions.R')
library(ggplot2)
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

plot.all <-function(data, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im){
  theta.sgd  = apply(theta.sgd, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  theta.asgd  = apply(theta.asgd, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  theta.asgd.bad  = apply(theta.asgd.bad, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  theta.batch  = apply(theta.batch, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  theta.sgd.im = apply(theta.sgd.im, 2, function(colum) 
    log(t(colum) %*% data$A %*% (colum)))
  x = seq(1, length(theta.batch))
  agg = data.frame(x, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im)
  select = c(floor(1.1^(1:72)), 1e4)
  agg = agg[select, ]
  agg = melt(agg, id = 'x')
  ggplot(agg, aes(x = x, y = value, color = variable)) + geom_line()+scale_x_log10()+
    xlab('Iterations')+ylab('excess risk')
  ggsave(file='fig/lan_tian_ps3_task2a.png', width=8, heigh=5, dpi=150)
}


sgd<-function(data, plot = T){
  n = nrow(data$X)
  p = ncol(data$X)
  theta.sgd = matrix(1, nrow = p, ncol = n+1)
  #a = 0.01/(0.01/sum(diag(data$A))+seq(1, n))
  gamma0 = 1
  lambda0 = 0.02
  
  for (i in 1:n){
    xi = data$X[i, ]
    ai = gamma0 / (1 + gamma0 * lambda0 * i)
    theta.old = theta.sgd[, i]
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


d = sample.data2a(1e6)
theta.sgd = sgd(d, F)
theta.sgd.im = sgd.im(d, F)
theta.asgd = asgd(d, F)
theta.asgd.bad = asgd.bad(d, F)
theta.batch = batch(d, F)
plot.all(d, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im)
save(d, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im, file="out/task2a.rda")
