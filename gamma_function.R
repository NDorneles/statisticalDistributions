integral_gamma <- function(u, t) {
  #Gamma integral function adapted for indetermined bounds. Both equations works
  #y <- (1 / u - 1)**(t - 1) * exp(-(1 / u - 1)) / (u**2)
  y <- (u / (1 - u))**(t - 1) * exp(-(u / (1 - u))) / ((1 - u)**2)
  return(y)
}

incomplete_gamma_integral <- function(x, t) {
  #Incomplete gamma integral function of x and t.
  #Defined as an integral from zero to a point x.
  y <- x**(t - 1) * exp(-x)
  return(y)
}

gamma <- function(t) {
  #Gamma function calculation for differents ranges of t.
  prod <- 1
  #For negative non-integer values, it stores the value of 1/t and ads one unit until the value becomes positive.
  if (t < 0) {
    while (t < 0) {
      prod <- -prod * 1 / abs(t)
      t <- t + 1
    }
  }
  #For values between 0 and 1, the gamma function is approximated by this polynomial regression.
  #Estimated by the author with R² = 0.99778 for this range.
  if (0 < t && t < 1) {
    soma <- prod*(-184.06582*t**5 + 585.11561*t**4 - 721.82921*t**3 + 435.55841*t**2 - 132.56646*t + 18.65162) 
  }
  #Fot t = 0.5 the is a solution.
  if (t == 0.5) {
    soma <- prod * sqrt(pi)
  }
  #For values of t higher or equals 1. The solution is calculated by the numerical approximation 
  if (t >= 1) {
    #step definition
    passo <- 0.0001
    #First value for x. It starts as a close value of 0
    x1 <- 0.001
    soma <- 0
    #It adds to the sum until the value 1, the end of the integral. 
    while (x1 < 1) {
      x2 <- x1 + passo
      if (x2 >= 1) {
        break
      }
      simpson <- (x2 - x1)/ 6 * (integral_gamma(x1, t) + 4 * integral_gamma((x1 + x2)/2, t) + integral_gamma(x2, t))
      soma <- soma + simpson
      x1 <- x2
    }
  }
  return(soma)
}

incomplete_gamma <- function(x, t) {
  #Value of thhe incomplete gamma function of x and t
  #Calculated with the result of the incomplete gamma integral function using the Simpson Rule.
  xent <- seq(0, x, 0.01)
  soma <- 0
  for (i in 1:length(xent)) {
    if (i == length(xent)) {
      break
    }
    f <- (xent[i+1]-xent[i])/6*(incomplete_gamma_integral(xent[i], t)+4*incomplete_gamma_integral((xent[i+1]+xent[i])/2, t)+incomplete_gamma_integral(xent[i+1], t))
    soma <- soma + f
  }
  return(soma)
}

regularized_gamma <- function(x, t) {
  #Regulized gamma function of t. Returs the ratio of a incomplete gamma function ant its complete function.
  y <- incomplete_gamma(x, t) / gamma(t)
  return(y)
}
