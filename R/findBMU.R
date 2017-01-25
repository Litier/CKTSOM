
##### buscar prototipo mas sercano
FindBMU <- function(dato,prototipos){
  d <-distancia(prototipos,dato)
  dminima <-min(d)
  index <- match(dminima,d)

  return (index)
}



#### calcula la distancia entre 2 puntos
calculaDistancia <- function(punto1,punto2,datos){
  tipoDeDatos<-sapply(datos, class)
  lista<-1:length(datos)
  lista[tipoDeDatos == "numeric"]<- 1
  lista[tipoDeDatos != "numeric"]<- 0
  restaPArcial <- c()
  for (i in 1:length(datos)) {
    if(lista[i]==1){
      factorResta <- punto1[i] - punto2[i]
      numero <- factorResta[1,]
      numero <- numero^2
      restaPArcial <-c(restaPArcial,numero)
    }
  }
  disc <- sum(restaPArcial)
  d <- sqrt(disc)
  return(d)
}


##distancia hasta los prototipos
distancia <- function(prototipos,puntofijo){
  resultado <- c()
  for (i in 1:length(prototipos[,1])){
    resultado <- c(resultado,calculaDistancia(puntofijo,prototipos[i,],prototipos))
  }
  return(resultado)
}
