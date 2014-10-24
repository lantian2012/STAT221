library(ggplot2)
library(reshape2)

var.agg = matrix(ncol=201, nrow=10)
for (j in 1:10){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  theta.var = matrix(nrow = 1, ncol = N+1)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = M, nrow = p)
    for (m in 1:M){
      theta.n[, m] = theta.list[[m]][ , n]
    }
    theta.var[n+1] = sum(diag(cov(theta.n)))
  }
  var.agg[j, ]= theta.var
}

data.sgd = data.frame(t(var.agg))
colnames(data.sgd) = data.sgd[1, ]
data.sgd = data.sgd[-1,]
data.sgd['X'] = seq(1, 10000, 50)
data.sgd = melt(data.sgd, id='X')
colnames(data.sgd)[2] = 'alpha'
ggplot(data.sgd, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Trace of Variance')+ xlab('Iterations')
ggsave(file='fig/lan_tian_ps2_task2d_sgd.png', width=5, heigh=5, dpi=120)

var.agg = matrix(ncol=201, nrow=10)
for (j in 11:20){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  theta.var = matrix(nrow = 1, ncol = N+1)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = M, nrow = p)
    for (m in 1:M){
      theta.n[, m] = theta.list[[m]][ , n]
    }
    theta.var[n+1] = sum(diag(cov(theta.n)))
  }
  var.agg[j-10, ]= theta.var
}

data.sgd.im = data.frame(t(var.agg))
colnames(data.sgd.im) = data.sgd.im[1, ]
data.sgd.im = data.sgd.im[-1,]
data.sgd.im['X'] = seq(1, 10000, 50)
data.sgd.im = melt(data.sgd.im, id='X')
colnames(data.sgd.im)[2] = 'alpha'
ggplot(data.sgd.im, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Trace of Variance')+ xlab('Iterations')
ggsave(file='fig/lan_tian_ps2_task2d_im.png', width=5, heigh=5, dpi=120)


var.agg = matrix(ncol=201, nrow=10)
for (j in 21:30){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  theta.var = matrix(nrow = 1, ncol = N+1)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = M, nrow = p)
    for (m in 1:M){
      theta.n[, m] = theta.list[[m]][ , n]
    }
    theta.var[n+1] = sum(diag(cov(theta.n)))
  }
  var.agg[j-20, ]= theta.var
}

data.asgd = data.frame(t(var.agg))
colnames(data.asgd) = data.asgd[1, ]
data.asgd = data.asgd[-1,]
data.asgd['X'] = seq(1, 10000, 50)
data.asgd = melt(data.asgd, id='X')
colnames(data.asgd)[2] = 'alpha'
ggplot(data.asgd, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Trace of Variance')+ xlab('Iterations')
ggsave(file='fig/lan_tian_ps2_task2d_asgd.png', width=5, heigh=5, dpi=120)


data.asgd = matrix(ncol=201, nrow=10)
for (j in 21:30){
  name = paste('out/Task2c_job_', j, '.rda',sep="")
  load(name)
  data.temp = matrix(ncol=200, nrow=500)
  for (i in 1:length(theta.list)){
    est.bias = apply(theta.list[[i]], 2, function(colum) 
      sum(diag((colum-theta)%*%t(colum-theta))))
    data.temp[i, ] = est.var
  }
  data.asgd[j-20, ] = c(a, apply(data.temp, 2, mean))
}

data.asgd = data.frame(t(data.asgd))
colnames(data.asgd) = data.asgd[1, ]
data.asgd = data.asgd[-1,]
data.asgd['X'] = seq(1, 10000, 50)
data.asgd = melt(data.asgd, id='X')
colnames(data.asgd)[2] = 'alpha'
ggplot(data.asgd, aes(x = X, y = value, color = alpha)) + geom_line()
ggsave(file='fig/lan_tian_ps2_task2d_asgd.png', width=5, heigh=5, dpi=120)


