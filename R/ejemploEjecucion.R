library(ggplot2)

ejemplo1 <- function(){
  data(iris)

  #View(trees)
  #View(iris)
  #################
  nIteraciones <- 600000
  alfaMAyor <- 1
  AlfaMenor<- 0
  Rinicial <- 7
  Rfinal <- 1
  k <- 2
  h <- 3

  #####################prueba con iris

  ml <- list(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width,iris$Species)
  t <- proc.time() # Inicia el cron?metro
  neuronas <- myFunc(k,h,alfaMAyor,AlfaMenor,Rinicial,Rfinal,nIteraciones, ml, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
  # NUESTRO CODIGO
  tiempo <-proc.time()-t    # Detiene el cron?metro
  neuronas$Species<-NA
  irisMa<- rbind(iris,neuronas)
  nGraficos<- c(1:4)

  graficar(iris[,-5],neuronas[,-5],k)
}




##############################################################################
ejemplo2 <- function(){

  #Inicializar variables
  matrizAdjacencia <- matrix(data = rep(0,169),nrow = 13)
  matrizAdjacencia[2,1]<- 1
  matrizAdjacencia[3,1]<- 1
  matrizAdjacencia[4,1]<- 1
  matrizAdjacencia[5,2]<- 1
  matrizAdjacencia[6,2]<- 1
  matrizAdjacencia[7,2]<- 1
  matrizAdjacencia[8,3]<- 1      # 3 -- 6
  matrizAdjacencia[9,3]<- 1
  matrizAdjacencia[10,4]<- 1
  matrizPesos <- matrix(runif(13, 1.0, 5.5))
  matrizPesos <- cbind(matrizPesos,runif(13, 3.0, 9.5))
  matrizPesos <- cbind(matrizPesos,runif(13, 0.0, 9.5))
  matrizPesos <- cbind(matrizPesos,runif(13, 7.0, 8.5))
  dataPesos <- data.frame(matrizPesos)
  vectorClusters <- c(0,1,1,1,2,2,2,3,3,3,4,4,4)

  #Grafico
  graficar2(dataPesos,3,matrizAdjacencia,vectorClusters)
}


#######################################################################3
ejemplo3 <- function(){
  data(iris)
  dataIris <-  iris
  dataIris <- dataIris[,-5]
  names (dataIris) <-c("X1","X2","X3","X4")
  vectorClustersDatos <- c(rep(1,25))
  vectorClustersDatos <- c(vectorClustersDatos ,rep(2,45))
  vectorClustersDatos <- c(vectorClustersDatos ,rep(3,30))

  matrizAdjacencia <- matrix(data = rep(0,49),nrow = 7)
  matrizAdjacencia[2,1]<- 1
  matrizAdjacencia[3,1]<- 1
  matrizAdjacencia[4,2]<- 1
  matrizAdjacencia[5,2]<- 1
  matrizAdjacencia[6,3]<- 1      # 3 -- 6

  matrizPesos <- matrix(runif(7, 1.0, 5.5))
  matrizPesos <- cbind(matrizPesos,runif(7, 3.0, 9.5))
  matrizPesos <- cbind(matrizPesos,runif(7, 0.0, 9.5))
  matrizPesos <- cbind(matrizPesos,runif(7, 7.0, 8.5))
  dataPesos <- data.frame(matrizPesos)
  #Inicializa las neuronas copiando los datos de forma aleatoria
  for(i in 1:length(dataPesos[,1])){
    n<- ceiling(runif(1,1,150))
    dataPesos[i,]  <- dataIris[n,]
  }
  #Define el grupo de cada neurona
  vectorClusters <- c(1,2,2,2,3,2,3)
  #Asigna el BMU a cada dato
  for (i in 1:150){
    n<- findBMU_R(dataIris[i,],dataPesos)
    vectorClustersDatos[i] <- vectorClusters[n]
  }
  #Grafica

  #graficarGrafo(dataIris,dataPesos,matrizAdjacencia)
  #graficarGrafo(dataIris,dataPesos,matrizAdjacencia,vectorClusters)
  graficarGrafo(dataIris,dataPesos,matrizAdjacencia,vectorClusters,vectorClustersDatos)
}
