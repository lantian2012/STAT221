locally_iid_EM <- function(data, c, A){
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
    pre = phi*lambda^c * t(A) %*%solve(A %*% Sigma %*% t(A))
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
    if (sum(abs(m$f)) < 1000)
      inLoop = F
    theta.old = theta.new
    count = count + 1
    if (count == 100){
      #print(sum(abs(m$f)))
      count = 0
    }
  }
  theta.old
}