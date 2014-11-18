library(reshape)
library(ggplot2)
data1 = read.csv('1router_allcount.dat')
data1$time = strptime(data1$time, '(%m/%d/%Y %H:%M:%S)')
data1$time.past = data1$time - data1$time[1]
data1$time.hours = data1$time.past/3600
data1$time.hours = as.numeric(data1$time.hours)
data1p = cast(data1, time.hours ~ nme)
names(data1p) <- sub(" ", ".", names(data1p))
ggplot(data1p, aes(x=time.hours))+geom_line(aes(y=dst.corp), color='black')+
  geom_line(aes(y=src.corp), color='magenta')
ggplot(data1p, aes(x=time.hours))+geom_line(aes(y=dst.local), color='black')+
  geom_line(aes(y=src.local), color='magenta')
ggplot(data1p, aes(x=time.hours))+geom_line(aes(y=dst.switch), color='black')+
  geom_line(aes(y=src.switch), color='magenta')
ggplot(data1p, aes(x=time.hours))+geom_line(aes(y=dst.fddi), color='black')+
  geom_line(aes(y=src.fddi), color='magenta')

center = strptime('(2/22/99 11:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - data1$time[1])
data1.fig4 = data1p[data1p$time.hours>=(center-27.5/60),]
data1.fig4 = data1.fig4[data1.fig4$time.hours<=(center+27.5/60),]
data1.fig4 = data1.fig4[c('src.corp', 'src.fddi', 'src.local', 'src.switch',
                          'dst.corp', 'dst.fddi', 'dst.local', 'dst.switch')]
means = log10(colMeans(data1.fig4))
vars = log10(apply(data1.fig4, 2, var))
data1.mv = data.frame(means, vars)
ggplot(data1.mv, aes(x=means, y=vars))+geom_point()+geom_smooth(method=lm, se=F)

center = strptime('(2/22/99 15:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - data1$time[1])
data1.fig4b = data1p[data1p$time.hours>=(center-27.5/60),]
data1.fig4b = data1.fig4b[data1.fig4b$time.hours<=(center+27.5/60),]
data1.fig4b = data1.fig4b[c('src.corp', 'src.fddi', 'src.local', 'src.switch',
                          'dst.corp', 'dst.fddi', 'dst.local', 'dst.switch')]
means.b = log10(colMeans(data1.fig4b))
vars.b = log10(apply(data1.fig4b, 2, var))
data1.mvb = data.frame(means.b, vars.b)
ggplot(data1.mvb, aes(x=means.b, y=vars.b))+geom_point()+geom_smooth(method=lm, se=F)




data2 = read.csv('2router_linkcount.dat')
data2$time = strptime(data2$time, '(%m/%d/%Y %H:%M:%S)')
data2$time.past = data2$time - data2$time[1]
data2$time.hours = data2$time.past/3600
data2$time.hours = as.numeric(data2$time.hours)
data2p = cast(data2, time.hours ~ nme)
names(data1p) <- sub(" ", ".", names(data1p))

center = strptime('(12/25/98 11:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - data2$time[1])
data2.fig4 = data2p[data2p$time.hours>=(center-27.5/60),]
data2.fig4 = data2.fig4[data2.fig4$time.hours<=(center+27.5/60),]
data2.fig4 = data2.fig4[, 2:17]
means = log10(colMeans(data2.fig4))
vars = log10(apply(data2.fig4, 2, var))
data2.mv = data.frame(means, vars)
ggplot(data2.mv, aes(x=means, y=vars))+geom_point()+geom_smooth(method=lm, se=F)

center = strptime('(12/25/98 15:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - data2$time[1])
data2.fig4b = data2p[data2p$time.hours>=(center-27.5/60),]
data2.fig4b = data2.fig4b[data2.fig4b$time.hours<=(center+27.5/60),]
data2.fig4b = data2.fig4b[, 2:17]
means.b = log10(colMeans(data2.fig4b))
vars.b = log10(apply(data2.fig4b, 2, var))
data2.mvb = data.frame(means.b, vars.b)
ggplot(data2.mvb, aes(x=means.b, y=vars.b))+geom_point()+geom_smooth(method=lm, se=F)


