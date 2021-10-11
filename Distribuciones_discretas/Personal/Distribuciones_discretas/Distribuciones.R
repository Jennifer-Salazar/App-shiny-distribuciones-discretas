## Distribución binomial 
# Siempre se debe de hacer hacia abajo
binomial <- function(num, size, prob, acumulada = FALSE){
  if(acumulada == FALSE){
    resultado <- pbinom(num, size=size, prob=prob)-pbinom(num-1, size=size, prob = prob)
  }else{
    resultado <- pbinom(num, size=size, prob=prob)
  }
  return(resultado)
}

poisson <- function(num, lambda, acumulada = FALSE){
  if(acumulada == FALSE){
    resultado <- ppois(num, lambda = lambda)-ppois(num-1, lambda=lambda)
  }else{
    resultado <- ppois(num, lambda = lambda)
  }
  return(resultado)
}

hipergeometrica <- function(x, K, N, n, acumulada = FALSE){
  if(acumulada == FALSE){
    resultado <- phyper(x, K, N-K, n)- phyper(x-1, K, N-K, n)
  }else{
    resultado <- phyper(x, K, N-K, n)
  }
  return(resultado)
}

