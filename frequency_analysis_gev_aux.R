# Arquivo auxiliar para o funcionamento da função geral `fun.fit.gev()`
# do script `frequency_analysis_gev.Rmd`.


# QUANTIS -----------------------------------------------------------------

# Quantis para parâmetro de forma diferente de zero
fun.q.gev <- function(p,      # probabilidade de não excedência
                      param   # vetor c/ parâmetros c(xi, alpha, kappa)
){
  
  # Extrair os valores do vetor 'param'
  xi <- param[[1]]; alpha <- param[[2]]; kappa <- param[[3]]
  
  # Quantil
  yp <- xi + alpha/kappa*(1 - (-log(p))^kappa)
  
  return(yp) # quantil explícito em função dos parâmetros
  # e da probabilidade de não excedência
  
}

# Quantis para parâmetro de forma igual a zero
fun.q.gu <- function(p,     # probabilidade de não excedência
                     param  # vetor c/ parâmetros c(csi, alpha)
){                 
  
  # Extrair os valores do vetor 'param'
  xi <- param[[1]]; alpha <- param[[2]]
  
  # Quantil
  yp <- xi - alpha*log(-log(p))
  
  return(yp) # quantil explícito em função dos parâmetros
  # e da probabilidade de não excedência
  
}


# VEROSSIMILHANÇA ---------------------------------------------------------

# Estimador de Máxima Verossimilhança - GEV
fun.l.gev <- function(param, # vetor c/ os parâmetros, 1. csi 2. alpha, 3. kappa (ou somente 1 e 2)
                      y       # vetor c/ a série AM observada
){
  
  # Tamanho da série
  n <- length(y)
  
  # Dependendo do tamanho do vetor, escolher a distribuição
  n.par <- length(param)
  
  if(n.par == 3){ # GEV completa
    
    # Extrair parâmetros
    xi <- param[[1]]; alpha <- param[[2]]; kappa <- param[[3]]
    
    # Avaliar kappa e domínio da função
    if(kappa < 0){ # Testar se kappa é negativo (Fréchet)
      
      # Testar limite inferior
      if(min(y) < (xi + alpha/kappa)){
        
        # Marcador de erro
        ln.L <- 1e6
        
      } else{
        
        # Se estiver dentro do domínio, calcular a verossimilhança
        ln.L <- -(- n*log(alpha) + sum((1/kappa - 1)*log(1 - kappa/alpha*(y - xi)) - (1 - kappa/alpha*(y - xi))^(1/kappa)))
        
      }
      
    } else{ # kappa positivo (Weibull)
      
      # Testar limite superior
      if(max(y) > (xi + alpha/kappa)){
        
        ln.L <- 1e6
        
      } else{
        
        ln.L <- -(- n*log(alpha) + sum((1/kappa - 1)*log(1 - kappa/alpha*(y - xi)) - (1 - kappa/alpha*(y - xi))^(1/kappa)))
        
      }
      
    }
    
    # Testar parâmetro de escala (deve ser maior que zero)
    if(alpha < 0) ln.L <- 1e6
    
    return(ln.L)
    
  } else{
    
    # Aqui avaliaremos quando kappa = 0 (ou seja, n.par == 2)
    # Extrair parâmetros
    xi <- param[[1]]; alpha <- param[[2]]
    
    # Como a distribuição de Gumbel não possui limites, precisamos avaliar somente
    # se o parâmetro de escala é positivo
    if(alpha < 0){
      
      ln.L <- 1e6
      
    } else{
      
      ln.L <- -(-n*log(alpha) - sum((y - xi)/alpha + exp(-(y - xi)/alpha)))
      
    }
    
    return(ln.L)
    
  }
  
  
}
