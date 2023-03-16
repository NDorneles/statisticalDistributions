int_erf <- function(x) {
  #Returns a value of the erf integral function for x.
  y <- 2 / sqrt(pi) * exp(-(x**2))
  return(y)
}
