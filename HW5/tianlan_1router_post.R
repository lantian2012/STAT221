library(reshape2)
library(ggplot2)

data = read.csv('1router_allcount.dat')
data = dcast(data, time~nme, value.var="value")
names(data) <- sub(" ", ".", names(data))
data = data[c('src.fddi', 'src.local', 'src.switch','src.corp',
              'dst.fddi', 'dst.local', 'dst.switch', 'dst.corp')]
data = t(as.matrix(data))
A = matrix(c(rep(1, 4), rep(c(rep(0, 16), rep(1, 4)), 3),
             rep(c(rep(c(1, rep(0, 3)), 4), 0), 2), rep(c(1, rep(0, 3)), 3),1, 0),
           nrow = 7, byrow=T)

w = 11
n.window = ncol(data)-w+1


estimates = matrix(nrow = ncol(A)+1, ncol=(n.window))
for(i in 1:28){
  name = paste('IID_', i, '.rda',sep="")
  load(name)
  estimates[, ((i-1)*10+1):((i-1)*10+ncol(estimate))] = estimate
}


result = data.frame(t(estimates[1:16, ]))
result['time'] = data.org['time'][((w+1)/2):(nrow(data.org)-(w-1)/2),]
result$time = strptime(result$time, '(%m/%d/%Y %H:%M:%S)')
result$time = result$time - result$time[1]
result$time = result$time/3600
result$time = as.numeric(result$time)
names(result)<-c('fddi->fddi','fddi->local', 'fddi->switch', 'fddi->corp',
                 'local->fddi','local->local', 'local->switch', 'local->corp',
                 'switch->fddi','switch->local', 'switch->switch', 'switch->corp',
                 'corp->fddi','corp->local', 'corp->switch', 'corp->corp', 'hour')

result = result[c('corp->fddi', 'corp->switch','corp->local', 'corp->corp',
                  'local->fddi', 'local->switch','local->local', 'local->corp',
                  'switch->fddi', 'switch->switch','switch->local', 'switch->corp',
                  'fddi->fddi', 'fddi->switch','fddi->local', 'fddi->corp','hour')]
names(result) <- sub("->", "2", names(result))

estimates.agg = A %*% estimates[1:16, ]
estimates.agg = data.frame(t(estimates.agg))
names(estimates.agg) <- c('org.fddi','org.local','org.switch','org.corp',
                          'dst.fddi','dst.local','dst.switch')
estimates.agg['dst.corp'] = rowSums(estimates.agg[, 1:4])- rowSums(estimates.agg[, 5:7])
estimates.agg['hour'] = result['hour']

data.tosmooth = data.org[c('src.fddi','src.local','src.switch','src.corp',
                           'dst.fddi','dst.local','dst.switch', 'dst.corp', 'total')]
data.smooth = matrix(nrow = n.window, ncol = ncol(data.tosmooth))
for (i in 1:n.window){
  data.smooth[i, ] = colMeans(data.tosmooth[i:(i+w-1), ])
}
data.smooth = data.frame(data.smooth)
names(data.smooth)<- c('org.fddi','org.local','org.switch','org.corp',
                       'dst.fddi','dst.local','dst.switch', 'dst.corp', 'total')
data.smooth['hour'] = result['hour']

#result = result[1:100, ]
#data.smooth = data.smooth[1:34, ]
#estimates.agg = estimates.agg[1:34, ]

result.plot <- melt(result,id='hour')
sp <- ggplot(result.plot, aes(x=hour, y=value)) + 
  geom_line()+ scale_y_continuous(limits=c(0, 1e6),breaks=seq(0, 1e6, 200e3),
                                  labels=c("0", "200K", "400K", "600K", "800K", "1M")) +ylab("bytes/sec")
sp = sp + facet_wrap( ~ variable, ncol=4)

data.smooth.plot.dst = melt(data.smooth[c('dst.fddi','dst.local','dst.switch', 'dst.corp', 'hour')],id='hour')
estimates.agg.plot.dst = melt(estimates.agg[c('dst.fddi','dst.local','dst.switch', 'dst.corp', 'hour')], id='hour')
data.smooth.plot.dst['type'] = 'obs'
estimates.agg.plot.dst['type'] = 'est'
data.dst = rbind(data.smooth.plot.dst, estimates.agg.plot.dst)
dst <- ggplot(data.dst)+ geom_line(aes(x=hour, y=value, color=type))+
  theme(legend.position="top", axis.title.x=element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  scale_y_continuous(limits=c(0, 1e6),breaks=seq(0, 1e6, 200e3),
                     labels=c("0", "200K", "400K", "600K", "800K", "1M")) +ylab("bytes/sec")
dst = dst + facet_wrap(~variable, ncol=4)

data.smooth.plot.org = melt(data.smooth[c('org.corp','org.local','org.switch', 'org.fddi', 'hour')],id='hour')
estimates.agg.plot.org = melt(estimates.agg[c('org.corp','org.local','org.switch', 'org.fddi', 'hour')], id='hour')
data.smooth.plot.org['type'] = 'obs'
estimates.agg.plot.org['type'] = 'est'
data.src = rbind(data.smooth.plot.org, estimates.agg.plot.org)
org <- ggplot(data.src)+ geom_line(aes(x=hour, y=value, color=type))+
  theme(legend.position="none",axis.title.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank())
org = org + facet_wrap(~variable, ncol=1)

data.total = data.frame(result['hour'])
data.total['est'] = rowSums(result)
data.total['obs'] = data.smooth['total']
data.total.plot = melt(data.total, id='hour')
total = ggplot(data.total.plot)+ geom_line(aes(x=hour, y=value, color=variable))+
  theme(legend.position="top", axis.title.y=element_blank(), axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank())
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2,heights=c(0.3,0.7), widths = c(0.8, 0.2))))
print(dst, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(total, vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(sp, vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(org, vp=viewport(layout.pos.row=2,layout.pos.col=2))




