normal <- function(x, mu, sigma, cul){
  
  if (cul == 0){ 
  
    y <- 1/sigma/sqrt(2*pi)*exp(-0.5*((x-mu)/sigma)**2)
  
  }
  
  if (cul == 1){
    
    source('erro_normal.R')
    
    y <- 1/2*(1 + erf((x-mu)/sigma/sqrt(2)))
    
  }
  
  return(list(x = x, y = y))
}

x11()
plot(normal(seq(-5,5,0.05), 0, 1,0))
x11()
plot(normal(seq(-5,5,0.05), 0, 1,1))




