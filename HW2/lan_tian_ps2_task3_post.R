#Pset 2 Task 3 post

library(reshape)
library(ggplot2)
#i each set of mu, sigma
#j node
Nt = 0
df1 = data.frame()
df2 = data.frame()
df3 = data.frame()
df4 = data.frame()
m = list()
s = list()
for (i in 1:4){
  merge68 = list()
  merge95 = list()
  mergetheta = list()
  mergemean = list()
  mergestd = list()
  for (j in 1:3){
    name = paste('out/Task3_coverage_', (i)+(j-1)*4, '.rda',sep="")
    load(name)
    Nt = ncol(coverage68)
    merge68[[j]] = coverage68  
    merge95[[j]] = coverage95
    mergetheta[[j]] = logtheta
    mergemean[[j]] = means
    mergestd[[j]] = stds
  }
  df68 = cbind(merge68[[1]], merge68[[2]], merge68[[3]])
  df95 = cbind(merge95[[1]], merge95[[2]], merge95[[3]])
  dftheta = cbind(mergetheta[[1]], mergetheta[[2]], mergetheta[[3]])
  m[[i]] = cbind(mergemean[[1]], mergemean[[2]], mergemean[[3]])
  s[[i]] = cbind(mergestd[[1]], mergestd[[2]], mergestd[[3]])
  if (i==1){
    df1 = data.frame(as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df1) = c('logtheta', 'coverage68', 'coverage95')
    ggplot(df1)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      stat_smooth(aes(x=logtheta, y=coverage68), color='green')+stat_smooth(aes(x=logtheta, y=coverage95), color='green')+
      scale_y_continuous(limits=c(-0.05, 1.05))+geom_hline(yintercept=0.95, color='green', linetype='longdash')+geom_hline(yintercept=0.68, color='green', linetype='longdash')+
      ylab('Coverage')+ggtitle('mu=1.6 sigma=0.7')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/lan_tian_ps2_task3_plot1.png', width=5, heigh=5, dpi=300)
    write(t(as.matrix(df1)), file='data/lan_tian_ps2_task3_par1_theta.dat', ncolumns=3)
  }
  if (i==2){
    df2 = data.frame(as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df2) = c('logtheta', 'coverage68', 'coverage95')
    ggplot(df2)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      stat_smooth(aes(x=logtheta, y=coverage68), color='green')+stat_smooth(aes(x=logtheta, y=coverage95), color='green')+
      scale_y_continuous(limits=c(-0.05, 1.05))+geom_hline(yintercept=0.95, color='green', linetype='longdash')+geom_hline(yintercept=0.68, color='green', linetype='longdash')+
      ylab('Coverage')+ggtitle('mu=2.5 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/lan_tian_ps2_task3_plot2.png', width=5, heigh=5, dpi=300)
    write(t(as.matrix(df2)), file='data/lan_tian_ps2_task3_par2_theta.dat', ncolumns=3)
  }
  if (i==3){
    df3 = data.frame(as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df3) = c('logtheta', 'coverage68', 'coverage95')
    ggplot(df3)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      stat_smooth(aes(x=logtheta, y=coverage68), color='green')+stat_smooth(aes(x=logtheta, y=coverage95), color='green')+
      scale_y_continuous(limits=c(-0.05, 1.05))+geom_hline(yintercept=0.95, color='green', linetype='longdash')+geom_hline(yintercept=0.68, color='green', linetype='longdash')+
      ylab('Coverage')+ggtitle('mu=5.2 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/lan_tian_ps2_task3_plot3.png', width=5, heigh=5, dpi=300)
    write(t(as.matrix(df3)), file='data/lan_tian_ps2_task3_par3_theta.dat', ncolumns=3)
  }
  if (i==4){
    df4 = data.frame(as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df4) = c('logtheta', 'coverage68', 'coverage95')
    ggplot(df4)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      stat_smooth(aes(x=logtheta, y=coverage68), color='green')+stat_smooth(aes(x=logtheta, y=coverage95), color='green')+
      scale_y_continuous(limits=c(-0.05, 1.05))+geom_hline(yintercept=0.95, color='green', linetype='longdash')+geom_hline(yintercept=0.68, color='green', linetype='longdash')+
      ylab('Coverage') + ggtitle('mu=4.9 sigma=1.6')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/lan_tian_ps2_task3_plot4.png', width=5, heigh=5, dpi=300)
    write(t(as.matrix(df4)), file='data/lan_tian_ps2_task3_par4_theta.dat', ncolumns=3)
  }
}
mm = list()
ss = list()
for (i in 1:4){
  mm[[i]]=as.vector(m[[i]])
  print(paste('Task 3 Group',i,'Mean' ))
  print(summary(mm[[i]]))
  ss[[i]]=as.vector(s[[i]])
  print(paste('Task 3 Group',i,'Standard Deviation' ))
  print(summary(ss[[i]]))
}


