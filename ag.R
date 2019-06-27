ag = function (tamanho, geracoes){
  source("iniciarPop.R")
  #source("torneio.R")
  source("funcaoDeAvaliacao.R")
  #source("pontoCorte.R")
  source("mutacao.R")
  source("roleta.R")
  source("mascara.R")
  
  pesos <- sort(c(sample(1:5, tamanho, replace = TRUE)))
  mask <- c(sample(0:1, tamanho, replace = TRUE))
  melhor = 0; media = 0
  pop = iniciarPop(tamanho)
  for (i in 1:geracoes){
    pop = pop[order(pop[,6], decreasing = T),]
    #popPais = torneio (pop, 50, 3)
    popPais = roleta (pop, tamanho, pesos)
    #popFilhos = pontoCorte (popPais) #trocar por mascara binaria
    popFilhos = mascara(popPais, mask, tamanho)
    inicio = (nrow(pop) - nrow(popFilhos))+1
    pop[inicio:nrow(pop),] = popFilhos
    pop = mutacao(pop,10)
    melhor[i] = pop[1,6]
    media[i] = mean(pop[,6])
  }
  plot(melhor, col = "red", ylim = c(min(media), max(melhor)), type="l", ylab = "")
  par(new=T)
  plot(media, col = "blue", ylim = c(min(media), max(melhor)), type="l", ylab = "fitness")
  return (popFilhos)
}
