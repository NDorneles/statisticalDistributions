beta <- function(x, DF1, DF2, cul = FALSE) {
  #Beta function value for x with DF1 and DF2 degrees of freedom
  source('beta_function.R')
  if (cul == FALSE) {
    y <- 1/complete_beta(DF1, DF2) * x**(DF1 - 1) * (1 - x)**(DF2 - 1)
  }
  if (cul == TRUE) {
    y <- regularized_beta(x, DF1, DF2)
  }
  return(y)
}