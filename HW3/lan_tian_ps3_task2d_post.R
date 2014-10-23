library(ggplot2)
library(reshape2)

data.sgd = matrix(ncol=201, nrow=10)
theta = matrix(1, ncol=1, nrow=100)
for (j in 1:10){
  name = paste('out/Task2c_job_', j, '.rda',sep="")
  load(name)
  data.temp = matrix(ncol=200, nrow=500)
  for (i in 1:length(theta.list)){
    est.var = apply(theta.list[[i]], 2, function(colum) 
      sum(diag(colum%*%t(colum)-theta%*%t(theta))))
    data.temp[i, ] = est.var
  }
  data.sgd[j, ] = c(a, apply(data.temp, 2, mean))
}

data.sgd = data.frame(t(data.sgd))
colnames(data.sgd) = data.sgd[1, ]
data.sgd = data.sgd[-1,]
data.sgd['X'] = seq(1, 10000, 50)
data.sgd = melt(data.sgd, id='X')
colnames(data.sgd)[2] = 'alpha'
ggplot(data.sgd, aes(x = X, y = value, color = alpha)) + geom_line()
ggsave(file='fig/lan_tian_ps2_task2d_sgd.png', width=5, heigh=5, dpi=120)

data.im = matrix(ncol=201, nrow=10)
for (j in 11:20){
  name = paste('out/Task2c_job_', j, '.rda',sep="")
  load(name)
  data.temp = matrix(ncol=200, nrow=500)
  for (i in 1:length(theta.list)){
    est.bias = apply(theta.list[[i]], 2, function(colum) 
      sum(diag((colum-theta)%*%t(colum-theta))))
    data.temp[i, ] = est.var
  }
  data.im[j-10, ] = c(a, apply(data.temp, 2, mean))
}

data.im = data.frame(t(data.im))
colnames(data.im) = data.im[1, ]
data.im = data.im[-1,]
data.im['X'] = seq(1, 10000, 50)
data.im = melt(data.im, id='X')
colnames(data.im)[2] = 'alpha'
ggplot(data.im, aes(x = X, y = value, color = alpha)) + geom_line()
ggsave(file='fig/lan_tian_ps2_task2d_im.png', width=5, heigh=5, dpi=120)

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


