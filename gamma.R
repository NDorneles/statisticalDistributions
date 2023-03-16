dist.gamma <- function(x, alpha, beta, cul){
  
  if (cul == 0){ 
    
    source('funcao_gamma.R')
    
    y <- beta**alpha*x**(alpha-1)*exp(-beta*x)/gamma(alpha)
    
  }
  
  if (cul == 1){
    
    
  }
  
  return(list(x = x, y = y))
}

