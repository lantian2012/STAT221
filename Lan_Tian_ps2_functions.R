
simYgivenTheta <- function(theta, w, N){
  J = length(theta)
  sim = matrix(NA, J, N)
  for (i in 1:J){
    sim[i,] = rpois(N, w[i]*theta[i])
  }
  sim
}

#for each row in mat(j rows), get the interval of 2.5% to 97.5%
#check is theta0[j] is in interval(j)
isCovered95 <- function(mat, theta0){
  interval = apply(mat, 1, quantile, probs=c(0.025, 0.975))
  J = length(theta0)
  result = matrix(FALSE, nrow=J, ncol=1)
  for (j in 1:J){
    if (theta0[j]>interval[1,j] && theta0[j]<interval[2,j])
      result[j] = TRUE
  }
  result
}

#for each row in mat(j rows), get the interval of 2.5% to 97.5%
#check is theta0[j] is in interval(j)
isCovered68 <- function(mat, theta0){
  interval = apply(mat, 1, quantile, probs=c(0.16, 0.84))
  J = length(theta0)
  result = matrix(FALSE, nrow=J, ncol=1)
  for (j in 1:J){
    if (theta0[j]>interval[1,j] && theta0[j]<interval[2,j])
      result[j] = TRUE
  }
  result
}