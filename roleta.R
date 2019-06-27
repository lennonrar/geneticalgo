roleta = function(pop, tamanho, pesos){
  sumpeso <- sum(pesos)
  sorted <- sample(0:sumpeso, 1)
  position <- 0
  popPais = pop[1:tamanho,]
  for (i in range(tamanho)){
    repeat{
      position = position + 1
      sorted = sorted - pesos[position]
      if(is.na(sorted) || sorted <= 0){
        break
      }
    }
    popPais[i,] <- pop[position, ]
  }
  return(popPais)
}
