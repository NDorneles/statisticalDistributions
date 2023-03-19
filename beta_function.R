complete_beta <- function(DF1, DF2) {
  #Complete beta function value of the degrees of freedom DF1, DF2
  source('gamma_function.R')
  beta <- gamma(DF1) * gamma(DF2) / gamma(DF1 + DF2)
  return(beta)
}

incomplete_integral <- function(x, DF1, DF2) {
  #Incomplete beta integral function of x and the degrees of freedom DF1, DF2
  y <- x**(DF1 - 1) * (1 - x)**(DF2 - 1)
  return(y)
}

incomplete_beta <- function(x, DF1, DF2) {
  #Incomplete beta function of x and the degrees of freedom DF1, DF2.
  #The integral is calculated using a Simpson's Rule.
  xent <- seq(1e-5, x, 0.0001)
  sum_integral <- 0
  for (i in 1:length(xent)) {
    if(i == length(xent)) {
      break
    }
    f <- (xent[i+1]-xent[i])/6*(incomplete_integral(xent[i], DF1, DF2)+4*incomplete_integral((xent[i+1]+xent[i])/2, DF1, DF2)+incomplete_integral(xent[i+1], DF1, DF2))
    sum_integral <- sum_integral + f
  }
  return(sum_integral)
}

regularized_beta <- function(x, DF1, DF2) {
  #Regularized beta function of x with its degrees of freedom 
  i <- incomplete_beta(x, DF1, DF2) / complete_beta(DF1, DF2)
  return(i)
}