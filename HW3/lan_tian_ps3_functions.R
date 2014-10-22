

random.orthogonal <- function(p) {
  # Get an orthogonal matrix.
  B = matrix(runif(p^2), nrow=p)
  qr.Q(qr(B))
}

plot.all <- function(data, est){
  est$sgd = apply(est$sgd, 2, function(colum) 
    log(t(colum-data$theta) %*% data$A %*% (colum-data$theta)))
  est$asgd = apply(est$asgd, 2, function(colum) 
    log(t(colum-data$theta) %*% data$A %*% (colum-data$theta)))
  est$sgd.im = apply(est$sgd.im, 2, function(colum) 
    log(t(colum-data$theta) %*% data$A %*% (colum-data$theta)))
  est$batch = apply(est$batch, 2, function(colum) 
    log(t(colum-data$theta) %*% data$A %*% (colum-data$theta)))
  x = matrix(seq(1, length(est[[1]])))
  agg = data.frame(est, x)
  agg = melt(agg, id = 'x')
  ggplot(agg, aes(x = x, y = value, color = variable)) + geom_line()+scale_x_log10()
  ggsave(file='fig/lan_tian_ps2_task2b.png', width=5, heigh=5, dpi=120)
}
