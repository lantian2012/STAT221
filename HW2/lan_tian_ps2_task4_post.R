#Pset 2 Task 4 post

library(reshape)
library(ggplot2)
#i each set of mu, sigma
#j node

w = read.table('weights.txt')[[1]]
w = rep(w, 9)
Nt = 0
df1 = data.frame()
df2 = data.frame()
df3 = data.frame()
df4 = data.frame()
for (i in 1:4){
  merge68 = list()
  merge95 = list()
  mergetheta = list()
  for (j in 1:3){
    name = paste('out/Task4_coverage_', (i)+(j-1)*4, '.rda',sep="")
    load(name)
    Nt = ncol(coverage68)
    merge68[[j]] = coverage68  
    merge95[[j]] = coverage95
    mergetheta[[j]] = logtheta
  }
  df68 = cbind(merge68[[1]], merge68[[2]], merge68[[3]])
  df95 = cbind(merge95[[1]], merge95[[2]], merge95[[3]])
  dftheta = cbind(mergetheta[[1]], mergetheta[[2]], mergetheta[[3]])
  if (i==1){
    df1 = data.frame(w, as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df1) = c('w', 'logtheta', 'coverage68', 'coverage95')
    ggplot(df1)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=1.6 sigma=0.7')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-1.png', width=5, heigh=5, dpi=300)
    ggplot(df1)+geom_point(aes(x=w, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=w, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=1.6 sigma=0.7')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-5.png', width=5, heigh=5, dpi=300)
  }
  if (i==2){
    df2 = data.frame(w, as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df2) = c('w', 'logtheta', 'coverage68', 'coverage95')
    ggplot(df2)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=2.5 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-2.png', width=5, heigh=5, dpi=300)
    ggplot(df2)+geom_point(aes(x=w, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=w, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=2.5 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-6.png', width=5, heigh=5, dpi=300)
  }
  if (i==3){
    df3 = data.frame(w, as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df3) = c('w', 'logtheta', 'coverage68', 'coverage95')
    ggplot(df3)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=5.2 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-3.png', width=5, heigh=5, dpi=300)
    ggplot(df3)+geom_point(aes(x=w, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=w, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=5.2 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-7.png', width=5, heigh=5, dpi=300)
  }
  if (i==4){
    df4 = data.frame(w, as.vector(dftheta), as.vector(df68), as.vector(df95))
    colnames(df4) = c('w', 'logtheta', 'coverage68', 'coverage95')
    ggplot(df4)+geom_point(aes(x=logtheta, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=logtheta, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage') + ggtitle('mu=4.9 sigma=1.6')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-4.png', width=5, heigh=5, dpi=300)
    ggplot(df4)+geom_point(aes(x=w, y=coverage68), shape=1, alpha=0.2)+geom_point(aes(x=w, y=coverage95), shape=1, alpha=0.2, color='blue')+
      ylab('Coverage')+ggtitle('mu=5.2 sigma=1.3')+theme(axis.title.x = element_text(size=20),axis.text.x= element_text(size=15),axis.title.y = element_text(size=20),axis.text.y= element_text(size=15))
    ggsave(file='fig/task4-8.png', width=5, heigh=5, dpi=300)
  }
}
