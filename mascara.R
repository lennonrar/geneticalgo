mascara = function(popPais, mask, tamanho){
  popFilhos = popPais
  
  for (i in 0:length(popPais[,1])){
    m <- popPais[i,] & mask
    j <- 0
    print(m)
    for (j in length(m[1,])){
        if (m[j] == 1){
          #filho1
          popFilhos[i,j] = popPais[i,j]
          #filho2
          popFilhos[i+1,j] = popPais[i+1,j]
        }else{
          #filho1
          popFilhos[i,j] = popPais[i+1,j]
          #filho2
          popFilhos[i+1,j] = popPais[i,j]
        }
        
      }
    popFilhos[i,6] = calcularFitness(popFilhos[i,1:5])    
    popFilhos[i+1,6] = calcularFitness(popFilhos[i+1,1:5])
  }
  return(popFilhos)
}
