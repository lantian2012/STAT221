locally_iid_EM <- function(data, c, A, max.iter = 240000){
  data = data[1:nrow(A), ]
  Time = ncol(data)
  J = nrow(data)
  I = ncol(A)
  get.complete.params <- function(old){
    #old = c(lambda1, lambda2, ..., lambdaI, phi)
    #Complete the E-step
    #return all params of the Q(theta, theta(k))
    lambda = old[1:I]
    phi = old[I+1]
    Sigma = diag(phi * lambda^c)
    M = matrix(ncol=Time, nrow=I)
    #for computation
    pre = phi*lambda^c * t(A) %*%ginv(A %*% Sigma %*% t(A))
    for (t in 1:Time){
      M[, t] = lambda + pre %*%(data[, t] - A%*%lambda)
    }
    R = Sigma - pre%*%A%*%Sigma
    return (list(M=M, R=R))
  }
  
  fobj <- function(theta, M, R){
    lambda = theta[1:I]
    phi = theta[I+1]
    sigma.elems = phi*lambda^c
    Sigma.inv = diag(1/(sigma.elems))
    log.Sigma.d = sum(log(sigma.elems))
    trace = sum(diag(R)/sigma.elems)
    Q = -Time/2*(log.Sigma.d + trace)
    M.sum = apply(M, 2, function(column) (column-lambda)^2*1/sigma.elems)
    M.sum = rowSums(M.sum)
    M.sum = sum(M.sum)
    Q = Q - 1/2*M.sum
    Q
  }
  
  update <- function(theta, M, R){
    #M-step
    a = diag(R) + rowSums(M^2)/Time
    b = rowSums(M)/Time
    lambda = theta[1:I]
    phi = theta[I+1]
    f.first = c*phi*lambda^c+(2-c)*lambda^2-2*(1-c)*lambda*b-c*a
    f.last = sum(lambda^(-c+1)*(lambda-b))
    f = c(f.first, f.last)
    J = diag(c(phi*c^2*lambda^(c-1)+2*(2-c)*lambda - 2*(1-c)*b, 0))
    J[1:I, (I+1)] = c*lambda^c
    J[(I+1), 1:I] = (2-c)*lambda^(1-c) - (1-c)*lambda^(-c)*b
    sum.row = rowSums(J)
    left = (1/sum.row)
    sum.column = colSums(J)
    right = (1/sum.column)
    J.normal = t(t(left *J)*right)
    list(diff = -t(t(right*solve(J.normal))*left)%*%f, f = f)
  }
  
  #initial conditions
  a0 =sum(data)/Time/sum(A)
  lambda0 = rep(a0, I)
  phi0 = 0
  if (c==1)
    phi0 = var(as.vector(data))/mean(as.vector(data))
  if (c==2)
    phi0 = 4*var(as.vector(data))/mean(as.vector(data))^2
  inLoop = T
  theta.old = c(lambda0, phi0)
  theta.new = 0
  count = 0
  while(inLoop){
    #E-step
    params = get.complete.params(theta.old) 
    #M-step
    m = update(theta = theta.old, M = params$M, R = params$R)
    theta.new = theta.old + m$diff
    #if (sum(abs(theta.new-theta.old)) < 1e-4)
    #  inLoop = F
    if (sum(abs(m$f)) < 10 || count >= max.iter)
      inLoop = F
    theta.old = theta.new
    count = count + 1
    if (count == 100){
      #print(sum(abs(m$f)))
      #count = 0
    }
  }
  print(count)
  theta.old
}

smoothed.EM3<-function(data, c, A, max.iter = 240000){
  library(MASS)
  get.complete.params <- function(old, data){
    #old = c(eta1, eta2, ..., etaI, eta(I+1))
    #Complete the E-step
    #return all params of the Q(theta, theta(k))
    lambda = exp(old[1:I])
    phi = exp(old[I+1])
    Sigma = diag(phi * lambda^c)
    #diag.elems = phi*lambda^c+1
    M = matrix(ncol=Time, nrow=I)
    #for computation
    pre = phi*lambda^c * t(A) %*%ginv(A %*% Sigma %*% t(A))
    for (t in 1:Time){
      M[, t] = lambda + pre %*%(data[, t] - A%*%lambda)
    }
    R = Sigma - pre%*%A%*%Sigma
    return (list(M=M, R=R))
  }
  
  update <- function(eta, last.iter, M, R){
    #M-step
    #the Q part similar to iid model
    a = diag(R) + rowSums(M^2)/Time
    b = rowSums(M)/Time
    lambda = exp(eta[1:I])
    phi = exp(eta[I+1])
    f.first = (c*phi*lambda^c+(2-c)*lambda^2-2*(1-c)*lambda*b-c*a)*lambda
    f.last = sum(lambda^(-c+1)*(lambda-b))*phi
    f = c(f.first, f.last)
    J = diag(c((phi*c^2*lambda^(c-1)+2*(2-c)*lambda - 2*(1-c)*b)*lambda^2+f.first, 0))
    J[1:I, (I+1)] = c*lambda^c*lambda*phi
    J[(I+1), 1:I] = ((2-c)*lambda^(1-c) - (1-c)*lambda^(-c)*b)*lambda*phi
    J[(I+1), (I+1)] = f.last/phi
    #the Q part related to the prior
    
    f = -last.iter$Sigma %*% (eta-last.iter$eta) + f
    J = -last.iter$Sigma + J
    list(eta=eta - ginv(J) %*% f, J = J)
  }
  data = data[1:nrow(A), ]
  J = nrow(data)
  I = ncol(A)
  
  a0 =sum(data)/ncol(data)/sum(A)
  lambda0 = rep(a0, I)
  phi0 = 4*var(as.vector(data))/mean(as.vector(data))^2
  inLoop = T
  last.iter = list(eta=log(c(lambda0, phi0)), Sigma=diag(c(lambda0, phi0)))
  w = 11
  Time = w
  n.window = ncol(data)-w+1
  estimates = matrix(nrow = ncol(A)+1, ncol=(n.window))
  #load('v.rda')
  V = diag(rep(1, 17))
  for (i in 1:n.window){
    data.window = data[, i:(i+w-1)]
    a0 =sum(data.window)/ncol(data.window)/sum(A)
    lambda0 = rep(a0, I)
    phi0 = 4*var(as.vector(data.window))/mean(as.vector(data.window))^2
    inLoop = T
    eta.old = log(c(lambda0, phi0))
    params = 0
    Sigma.update = matrix(ncol=I+1, nrow=I+1)
    count = 0
    while(inLoop){
      count = count + 1
      #e-step
      params = get.complete.params(eta.old, data.window)
      #m-step
      new.data = update(eta.old, last.iter, params$M, params$R)
      eta.new = new.data$eta
      Sigma.update = new.data$J
      if (sum(abs(eta.new-eta.old)) < 1e-4 || count > max.iter)
        inLoop = F
      #if(sum(abs(exp(eta.new)-exp(eta.old)))<=1e-4){
      #  inLoop = T
      #}
      eta.old = eta.new
    }
    last.iter = list(eta=eta.old, Sigma = (ginv(Sigma.update)+V))
    estimates[, i] = eta.old
    print(i)
  }
}




smoothed.EM2<-function(data, c, A, max.iter = 240000){
  library(mvtnorm)
  log.prior<-function(last.iter, eta){
    dmvnorm(eta, mean = last.iter$eta, sigma = (last.iter$Sigma+V), log = T)
  }
  
  get.complete.params <- function(old, data){
    #old = c(eta1, eta2, ..., eta(I+1))
    #Complete the E-step
    #return all params of the Q(eta, eta(k))
    lambda = exp(old[1:I])
    phi = exp(old[I+1])
    Sigma = diag(phi * lambda^c)
    M = matrix(ncol=Time, nrow=I)
    #for computation
    pre = phi*lambda^c * t(A) %*%solve(A %*% Sigma %*% t(A))
    for (t in 1:Time){
      M[, t] = lambda + pre %*%(data[, t] - A%*%lambda)
    }
    R = Sigma - pre%*%A%*%Sigma
    return (list(M=M, R=R))
  }
  
  posterior<-function(eta, M, R, last.iter){
    #calculate the Q funtion (likelihood + prior)
    lambda = exp(eta[1:I])
    phi = exp(eta[I+1])
    sigma.elems = phi*lambda^c
    Sigma.inv = diag(1/(sigma.elems))
    log.Sigma.d = sum(log(sigma.elems))
    trace = sum(diag(R)/sigma.elems)
    Q = -Time/2*(log.Sigma.d + trace)
    #M.sum = apply(M, 2, function(column) (column-lambda)^2*1/sigma.elems)
    #M.sum = sum(M.sum)
    M.sum = sum((t(M)-lambda)^2/sigma.elems)
    Q = Q - 1/2*M.sum
    Q = Q + log.prior(last.iter, eta)
    Q
  }
  
  get.Jacobian <- function(old, last.iter, M, R){
    log.lambda = old[1:I]
    lambda = exp(log.lambda)
    log.phi = old[I+1]
    phi = exp(log.phi)
    iter.Sigma = last.iter$Sigma
    iter.Sigma.inv = solve(iter.Sigma)
    pre1 = rowSums(M)
    pre2 = rowSums(M^2)
    diag.first = -Time/2/phi*(c^2*diag(R)*lambda^(-c)+(2-c)^2*lambda^(2-c)+
                                1/Time*(c^2*lambda^(-c)*pre2-2*(1-c)^2*lambda^(1-c)*pre2))-
      diag(iter.Sigma.inv)[1:I]
    diag.last = -Time/2/phi*sum(diag(R)/lambda^c) - 1/2/phi*sum(rowSums(t(t(M)-lambda)^2)/lambda^c)-
      iter.Sigma.inv[I+1, I+1]
    right = Time/2/phi*(-c*diag(R)*lambda^(-c)+(2-c)*lambda^(2-c)+
                          1/w*(-c*lambda^(-c)*pre2-2*(1-c)*lambda^(1-c)*pre1))-
      iter.Sigma.inv[1:I, I+1]
    J = diag(c(diag.first, diag.last))
    J = J-iter.Sigma.inv+diag(diag(iter.Sigma.inv))
    J[I+1, 1:I] = right
    J[1:I, I+1] = right
    J
  }
  
  data = data[1:nrow(A), ]
  J = nrow(data)
  I = ncol(A)
  
  a0 =sum(data)/ncol(data)/sum(A)
  lambda0 = rep(a0, I)
  phi0 = 0
  if (c==1)
    phi0 = var(as.vector(data))/mean(as.vector(data))
  if (c==2)
    phi0 = 4*var(as.vector(data))/mean(as.vector(data))^2
  inLoop = T
  last.iter = list(eta=log(c(lambda0, phi0)), Sigma=diag(c(lambda0, phi0)))
  V = 5*diag(c(lambda0, phi0))
  w = 11
  Time = w
  n.window = ncol(data)-w+1
  estimates = matrix(nrow = ncol(A)+1, ncol=(n.window))
  for (i in 1:n.window){
    data.window = data[, i:(i+w-1)]
    a0 =sum(data.window)/ncol(data.window)/sum(A)
    lambda0 = rep(a0, I)
    phi0 = 0
    if (c==1)
      phi0 = var(as.vector(data.window))/mean(as.vector(data.window))
    if (c==2)
      phi0 = 4*var(as.vector(data.window))/mean(as.vector(data.window))^2
    inLoop = T
    eta.old = log(c(lambda0, phi0))
    params = 0
    while(inLoop){
      #e-step
      params = get.complete.params(eta.old, data.window)
      #m-step
      op.result = optim(eta.old, posterior, M=params$M, R=params$R, last.iter=last.iter, control=list(fnscale=-1))
      eta.new = op.result$par
      if (sum(abs(eta.new-eta.old)) < 1e-4)
        inLoop = F
      eta.old = eta.new
    }
    J = get.Jacobian(eta.old, last.iter, params$M, params$R)
    print(solve(J))
    last.iter = list(eta=eta.old, Sigma = solve(J))
    estimates[, i] = eta.old
    print(i)
  }
}





smoothed.EM <-function(data, c, A, max.iter = 240000){
  data = data[1:nrow(A), ]
  Time = ncol(data)
  J = nrow(data)
  I = ncol(A)
  
  get.MR <- function(old){
    #old = c(lambda1, lambda2, ..., lambdaI, phi)
    #Complete the E-step
    #return all params of the Q(theta, theta(k))
    lambda = exp(old[1:I])
    phi = exp(old[I+1])
    Sigma = diag(phi * lambda^c)
    M = matrix(ncol=Time, nrow=I)
    print(phi * lambda^c)
    #for computation
    pre = phi*lambda^c * t(A) %*%solve(A %*% Sigma %*% t(A))
    for (t in 1:Time){
      M[, t] = lambda + pre %*%(data[, t] - A%*%lambda)
    }
    R = Sigma - pre%*%A%*%Sigma
    return (list(M=M, R=R))
  }
  
  get.f <- function(old, last.iter, M, R){
    log.lambda = old[1:I]
    lambda = exp(log.lambda)
    log.phi = old[I+1]
    phi = exp(log.phi)
    iter.log.lambda = last.iter$eta[1:I]
    iter.log.phi = last.iter$eta[I+1]
    iter.Sigma = last.iter$Sigma
    iter.lambda = exp(iter.log.lambda)
    iter.phi = exp(iter.log.phi)
    partsum = (-c*lambda^(-c)*rowSums(M^2)-2*(1-c)*lambda^(-c+1)*rowSums(M))/Time
    part1 = -Time/phi/2*(c*phi - c*diag(R)*lambda^(-c)+(2-c)*lambda^(2-c)+partsum)
    part2 = rowSums(solve(iter.Sigma))[1:I]*(log.lambda - iter.log.lambda)
    f.first = part1 - part2
    part1 = -Time/2*(I-1/phi*sum(diag(R)/lambda^c))+1/phi/2*
      sum(rowSums(t(t(M)-lambda)^2)/lambda^c)
    part2 = sum(solve(iter.Sigma)[I+1, ])*(log.phi - iter.log.phi)
    f.last = part1 + part2
    return (c(f.first, f.last))
  }
  
  update <- function(old, last.iter, M, R, f){
    log.lambda = old[1:I]
    lambda = exp(log.lambda)
    log.phi = old[I+1]
    phi = exp(log.phi)
    iter.Sigma = last.iter$Sigma
    iter.Sigma.inv = solve(iter.Sigma)
    pre1 = rowSums(M)
    pre2 = rowSums(M^2)
    diag.first = -Time/2/phi*(c^2*diag(R)*lambda^(-c)+(2-c)^2*lambda^(2-c)+
                              1/Time*(c^2*lambda^(-c)*pre2-2*(1-c)^2*lambda^(1-c)*pre2))-
      diag(iter.Sigma.inv)[1:I]
    diag.last = -Time/2/phi*sum(diag(R)/lambda^c) - 1/2/phi*sum(rowSums(t(t(M)-lambda)^2)/lambda^c)-
      iter.Sigma.inv[I+1, I+1]
    right = Time/2/phi*(-c*diag(R)*lambda^(-c)+(2-c)*lambda^(2-c)+
                          1/w*(-c*lambda^(-c)*pre2-2*(1-c)*lambda^(1-c)*pre1))-
      iter.Sigma.inv[1:I, I+1]
    J = diag(c(diag.first, diag.last))
    J = J-iter.Sigma.inv+diag(diag(iter.Sigma.inv))
    J[I+1, 1:I] = right
    J[1:I, I+1] = right
    sum.row = rowSums(J)
    left = (1/sum.row)
    sum.column = colSums(J)
    right = (1/sum.column)
    J.normal = t(t(left *J)*right)
    list(diff=-t(t(right*solve(J.normal))*left)%*%f, J=J)
  }
  
  EM.step<-function(data, last.iter){
    data = data[1:nrow(A), ]
    Time = ncol(data)
    J = nrow(data)
    I = ncol(A)
    #initial conditions
    a0 =sum(data)/Time/sum(A)
    lambda0 = rep(a0, I)
    phi0 = 0
    if (c==1)
      phi0 = var(as.vector(data))/mean(as.vector(data))
    if (c==2)
      phi0 = 4*var(as.vector(data))/mean(as.vector(data))^2
    inLoop = T
    eta.old = log(c(lambda0, phi0))
    eta.new = 0
    count = 0
    J = 0
    while(inLoop){
      #E-step
      params = get.MR(eta.old)
      f = get.f(eta.old, last.iter, params$M, params$R)
      #M-step
      m = update(eta.old, last.iter, params$M, params$R, f)
      J = m$J
      eta.new = eta.old + m$diff
      #if (sum(abs(theta.new-theta.old)) < 1e-4)
      #  inLoop = F
      if (sum(abs(f)) < 10 || count >= max.iter)
        inLoop = F
      eta.old = eta.new
      count = count + 1
      if (count == 100){
        #print(sum(abs(m$f)))
        #count = 0
      }
    }
    print(count)
    sum.row = rowSums(J)
    left = (1/sum.row)
    sum.column = colSums(J)
    right = (1/sum.column)
    J.normal = t(t(left *J)*right)
    list(eta=eta.old, Sigma=t(t(right*solve(J.normal))*left))
  }
  
  Time = ncol(data)
  J = nrow(data)
  I = ncol(A)
  a0 =sum(data)/Time/sum(A)
  lambda0 = rep(a0, I)
  phi0 = 0
  if (c==1)
    phi0 = var(as.vector(data))/mean(as.vector(data))
  if (c==2)
    phi0 = 4*var(as.vector(data))/mean(as.vector(data))^2
  inLoop = T
  eta.old = log(c(lambda0, phi0))
  last.iter = list(eta=eta.old, Sigma=diag(c(log(phi0)*log(lambda0), log(phi0))))
  V = 5*diag(eta.old)
  w = 11
  n.window = ncol(data)-w+1
  estimates = matrix(nrow = ncol(A)+1, ncol=(n.window))
  for (i in 1:n.window){
    last.iter = EM.step(data[, i:(i+w-1)], last.iter)
    estimates[,i] = last.iter$eta
  }
}





