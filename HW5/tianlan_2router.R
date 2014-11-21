
args <- as.numeric(commandArgs(trailingOnly = TRUE))

if(length(args) != 1) {
  args[1] = 1
}
job.id = args[1]

source('tianlan_functions.R')
library(reshape2)
library(ggplot2)

data = read.csv('2router_linkcount.dat')
data = dcast(data, time~nme, value.var="value")
data = t(as.matrix(data[, 2:ncol(data)]))

# Credits to Ye Kuang.
A.gen <- function(dim) {
  # Generate incidence matrix based on routing scheme in Cao et. al (1998)
  #
  # Args:
  #   dim: number of nodes
  #
  # Returns:
  #   Incidence matrix which maps the node pairs
  A <- array(0, dim=c((2*dim-1), dim^2))
  for (i in 1:dim) {
    A[i, (((i-1)*dim+1):(i*dim))] <- rep(1, dim)
  }
  for (i in 1:(dim-1)) {
    template <- rep(0, dim)
    template[i] <- 1
    A[i+dim, ] <- rep(template, dim)
  }
  return(A)
}

A <- A.gen(8)




w = 11
n.window = ncol(data)-w+1

start = (job.id-1)*10+1
end = start + 9
if (job.id == 28)
  end = n.window
estimate = matrix(nrow = ncol(A)+1, ncol = end-start+1)

for(i in start:end){
  estimate[, (i-start+1)] <- locally_iid_EM(data[, i:(i+w-1)], 2, A, 24000)
  print(i)
}

save(job.id,estimate,file=sprintf("IID_2_%d.rda", job.id))

