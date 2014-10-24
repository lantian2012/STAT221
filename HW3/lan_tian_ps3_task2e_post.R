library(ggplot2)
library(reshape2)

var.agg = matrix(ncol=201, nrow=10)

sqrt.norm <- function(X) {
  sqrt(mean(X^2)  )
}


for (j in 1:10){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  I = diag(p)
  var.theoretical = a * solve(2 * a * A - I) %*% A
  theta.var = matrix(nrow = 1, ncol = N+1)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = p, nrow = M)
    for (m in 1:M){
      theta.n[m, ] = theta.list[[m]][ , n]
    }
    var.empirical = (a/sum(diag(A))+1+(N-1)*50)/a*cov(theta.n)
    theta.var[n+1] = sqrt.norm(var.empirical - var.theoretical)
  }
  var.agg[j, ]= theta.var
}

data.sgd = data.frame(t(var.agg))
colnames(data.sgd) = data.sgd[1, ]
data.sgd = data.sgd[-1,]
data.sgd['X'] = seq(1, 10000, 50)
data.sgd = melt(data.sgd, id='X')
colnames(data.sgd)[2] = 'alpha'
ggplot(data.sgd, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Error')+ xlab('Iterations')
ggsave(file='fig/lan_tian_ps2_task2e_sgd.png', width=5, heigh=5, dpi=120)

var.agg = matrix(ncol=201, nrow=10)
for (j in 11:20){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  I = diag(p)
  var.theoretical = a * solve(2 * a * A - I) %*% A
  theta.var = matrix(nrow = 1, ncol = N+1)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = p, nrow = M)
    for (m in 1:M){
      theta.n[m, ] = theta.list[[m]][ , n]
    }
    var.empirical = (a+1+(N-1)*50)/a*cov(theta.n)
    theta.var[n+1] = sqrt.norm(var.empirical - var.theoretical)
  }
  var.agg[j-10, ]= theta.var
}

data.sgd.im = data.frame(t(var.agg))
colnames(data.sgd.im) = data.sgd.im[1, ]
data.sgd.im = data.sgd.im[-1,]
data.sgd.im['X'] = seq(1, 10000, 50)
data.sgd.im = melt(data.sgd.im, id='X')
colnames(data.sgd.im)[2] = 'alpha'
ggplot(data.sgd.im, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Error')+ xlab('Iterations')
ggsave(file='fig/lan_tian_ps2_task2e_im.png', width=5, heigh=5, dpi=120)


var.agg = matrix(ncol=201, nrow=10)
for (j in 21:30){
  name = paste('out/task2c_job_', j, '.rda',sep="")
  load(name)
  N = ncol(theta.list[[1]])
  p = nrow(theta.list[[1]])
  M = length(theta.list)
  theta.var = matrix(nrow = 1, ncol = N+1)
  I = diag(p)
  var.theoretical = solve(A)
  theta.var[1] = a
  for (n in 1:N){
    theta.n = matrix(ncol = p, nrow = M)
    for (m in 1:M){
      theta.n[m, ] = theta.list[[m]][ , n]
    }
    var.empirical = cov(theta.n)
    theta.var[n+1] = sqrt.norm(var.empirical - var.theoretical/(1+(n-1)*50))
  }
  var.agg[j-20, ]= theta.var
}

data.asgd = data.frame(t(var.agg))
colnames(data.asgd) = data.asgd[1, ]
data.asgd = data.asgd[-1,]
data.asgd['X'] = seq(1, 10000, 50)
data.asgd = melt(data.asgd, id='X')
colnames(data.asgd)[2] = 'alpha'
ggplot(data.asgd, aes(x = X, y = value, color = alpha)) + geom_line() + ylab('Error(log scale)')+ xlab('Iterations')+
  scale_y_continuous(limits = c(0, 0.025))+scale_y_log10()
ggsave(file='fig/lan_tian_ps2_task2e_asgd.png', width=5, heigh=5, dpi=120)

