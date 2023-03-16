erf <- function(x) {
  #Calculation of the erf integral function by Simpson Rule. It might works with others functions
  source('erf_integral.R')
  y <- c(1:length(x) * 0)
  for (i in 1:length(x)){
    step <- x[i] / 100
    xe <- seq(0, x[i], step)
    y[i] <- 0
    for (j in 1:length(xe)){
      if (j == length(xe)) {
        break
      }
      f <- (xe[j+1]-xe[j])/6*(int_erf(xe[j])+4*int_erf((xe[j+1]+xe[j])/2)+int_erf(xe[j+1]))
      y[i] <- y[i] + f
    }
  }
  return(y)
}