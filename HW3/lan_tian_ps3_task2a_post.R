source('lan_tian_ps3_functions.R')
library(ggplot2)
library(reshape2)

plot.all <-function(data, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im){
  #put all data into a dataframe
  theta.sgd  = apply(theta.sgd, 2, function(colum) 
    log(t(colum) %*% data[[1]] %*% (colum)))
  theta.asgd  = apply(theta.asgd, 2, function(colum) 
    log(t(colum) %*% data[[1]] %*% (colum)))
  theta.asgd.bad  = apply(theta.asgd.bad, 2, function(colum) 
    log(t(colum) %*% data[[1]] %*% (colum)))
  theta.batch  = apply(theta.batch, 2, function(colum) 
    log(t(colum) %*% data[[1]] %*% (colum)))
  theta.sgd.im = apply(theta.sgd.im, 2, function(colum) 
    log(t(colum) %*% data[[1]] %*% (colum)))
  x = seq(1, length(theta.batch))
  x = 1+(x-1)*50
  agg = data.frame(x, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im)
  #only plot 104 points in the final plots
  select = c(floor(1.1^(1:103)),2e4)
  agg = agg[select, ]
  agg = melt(agg, id = 'x')
  ggplot(agg, aes(x = x, y = value, color = variable)) + geom_line()+scale_x_log10(limits=c(200, 1e6))+
    xlab('Iterations')+ylab('excess risk')
  ggsave(file='fig/lan_tian_ps3_task2a.png', width=8, heigh=5, dpi=150)
}

load("out/task2a.rda")
plot.all(d, theta.sgd, theta.asgd, theta.asgd.bad, theta.batch, theta.sgd.im)
