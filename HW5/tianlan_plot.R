library(reshape)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)

data = read.csv('1router_allcount.dat')
data$time = strptime(data$time, '(%m/%d/%Y %H:%M:%S)')
names(data) <- sub(" ", ".", names(data))
data1 = read.csv('1router_allcount.dat')
data1$time = strptime(data1$time, '(%m/%d/%Y %H:%M:%S)')
data1$time.past = data1$time - data1$time[1]
data1$time.hours = data1$time.past/3600
data1$time.hours = as.numeric(data1$time.hours)
data1 = dcast(data1, time.hours~nme, value.var="value")
names(data1) <- sub(" ", ".", names(data1))
data1p = data1
data1.dst = melt(data1[c('dst.fddi','dst.local','dst.switch', 'dst.corp', 'time.hours')],id='time.hours')
data1.dst['type'] = 'dst'
data1.org = melt(data1[c('src.fddi', 'src.local', 'src.switch','src.corp', 'time.hours')],id='time.hours')
data1.org['type'] = 'org'
data1.p = rbind(data1.dst, data1.org)

renames = as.character(data1.p$variable)
for(i in 1:length(renames)){
  renames[i] = strsplit(renames[i], ".", fixed=T)[[1]][2]
}
data1.p['variable'] = as.factor(renames)

st <- ggplot(data1.p)+ geom_line(aes(x=time.hours, y=value, color=type))+
  theme(legend.position="top")+xlab('hour of day')+
  scale_y_continuous(limits=c(0, 1e6),breaks=seq(0, 1e6, 200e3),
                     labels=c("0", "200K", "400K", "600K", "800K", "1M")) +ylab("bytes/sec")
st + facet_wrap(~variable, ncol=1)
ggsave('tianlan_fig2.pdf', width = 10, height = 7, dpi=100)



center = strptime('(2/22/99 11:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - strptime('(2/22/99 00:00:00)', '(%m/%d/%Y %H:%M:%S)'))
data1.fig4 = data1p[data1p$time.hours>=(center-27.5/60),]
data1.fig4 = data1.fig4[data1.fig4$time.hours<=(center+27.5/60),]
data1.fig4 = data1.fig4[c('src.corp', 'src.fddi', 'src.local', 'src.switch',
                          'dst.corp', 'dst.fddi', 'dst.local', 'dst.switch')]
means = log10(colMeans(data1.fig4))
vars = log10(apply(data1.fig4, 2, var))
int1 = mean(vars)-mean(means)
int2 = mean(vars)-2*mean(means)
data1.mv = data.frame(means, vars)
a1<-ggplot(data1.mv, aes(x=means, y=vars))+geom_point()+geom_smooth(method=lm, se=F)+
  geom_abline(intercept=int1, slope=1,linetype="dashed")+geom_abline(intercept=int2, slope=2,linetype="dashed")+
  annotate('text', x = means-0.05, y=vars-0.15, label=1:8)

center = strptime('(2/22/99 15:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - strptime('(2/22/99 00:00:00)', '(%m/%d/%Y %H:%M:%S)'))
data1.fig4b = data1p[data1p$time.hours>=(center-27.5/60),]
data1.fig4b = data1.fig4b[data1.fig4b$time.hours<=(center+27.5/60),]
data1.fig4b = data1.fig4b[c('src.corp', 'src.fddi', 'src.local', 'src.switch',
                          'dst.corp', 'dst.fddi', 'dst.local', 'dst.switch')]
means.b = log10(colMeans(data1.fig4b))
vars.b = log10(apply(data1.fig4b, 2, var))
int1.b = mean(vars.b)-mean(means.b)
int2.b = mean(vars.b)-2*mean(means.b)
data1.mvb = data.frame(means.b, vars.b)
b1<-ggplot(data1.mvb, aes(x=means.b, y=vars.b))+geom_point()+geom_smooth(method=lm, se=F)+
  geom_abline(intercept=int1.b, slope=1,linetype="dashed")+geom_abline(intercept=int2.b, slope=2,linetype="dashed")+
  annotate('text', x = means.b-0.02, y=vars.b-0.15, label=1:8)

pdf(file="tianlan_fig4_1router.pdf", width=8, height=5) 
grid.arrange(a1, b1, ncol=2)
dev.off() 



data2 = read.csv('2router_linkcount.dat')
data2$time = strptime(data2$time, '(%m/%d/%Y %H:%M:%S)')
data2$time.past = data2$time - data2$time[1]
data2$time.hours = data2$time.past/3600
data2$time.hours = as.numeric(data2$time.hours)
data2p = cast(data2, time.hours ~ nme)

center = strptime('(12/25/98 11:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - strptime('(12/25/98 00:00:00)', '(%m/%d/%Y %H:%M:%S)'))
data2.fig4 = data2p[data2p$time.hours>=(center-27.5/60),]
data2.fig4 = data2.fig4[data2.fig4$time.hours<=(center+27.5/60),]
data2.fig4 = data2.fig4[, 2:17]
means = log10(colMeans(data2.fig4))
vars = log10(apply(data2.fig4, 2, var))
int1 = mean(vars)-mean(means)
int2 = mean(vars)-2*mean(means)
data2.mv = data.frame(means, vars)
a2<-ggplot(data2.mv, aes(x=means, y=vars))+geom_point()+geom_smooth(method=lm, se=F)+
  geom_abline(intercept=int1, slope=1,linetype="dashed")+geom_abline(intercept=int2, slope=2,linetype="dashed")+
  annotate('text', x = means-0.05, y=vars-0.3, label=1:16)

center = strptime('(12/25/98 15:30:00)', '(%m/%d/%Y %H:%M:%S)')
center = as.numeric(center - data2$time[1])
data2.fig4b = data2p[data2p$time.hours>=(center-27.5/60),]
data2.fig4b = data2.fig4b[data2.fig4b$time.hours<=(center+27.5/60),]
data2.fig4b = data2.fig4b[, 2:17]
means.b = log10(colMeans(data2.fig4b))
vars.b = log10(apply(data2.fig4b, 2, var))
int1.b = mean(vars.b)-mean(means.b)
int2.b = mean(vars.b)-2*mean(means.b)
data2.mvb = data.frame(means.b, vars.b)
b2<-ggplot(data2.mvb, aes(x=means.b, y=vars.b))+geom_point()+geom_smooth(method=lm, se=F)+
  geom_abline(intercept=int1.b, slope=1,linetype="dashed")+geom_abline(intercept=int2.b, slope=2,linetype="dashed")+
  annotate('text', x = means.b-0.05, y=vars.b-0.15, label=1:16)

pdf(file="tianlan_fig4_2router.pdf", width=8, height=5) 
grid.arrange(a2, b2, ncol=2)
dev.off() 



