
args <- as.numeric(commandArgs(trailingOnly = TRUE))

if(length(args) != 1) {
  args[1] = 1
}
job.id = args[1]

source('tianlan_functions.R')
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

start = (job.id-1)*10+1
end = start + 9
if (job.id == 28)
  end = n.window
estimate = matrix(nrow = ncol(A)+1, ncol = end-start+1)

for(i in start:end){
  estimate[, (i-start+1)] <- locally_iid_EM(data[, i:(i+w-1)], 2, A)
  print(i)
}

save(job.id,estimate,file=sprintf("IID_%d.rda", job.id))
