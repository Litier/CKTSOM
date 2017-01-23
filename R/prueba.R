#########################################################prueba 1

##########
#graficar(Preprocesamiento,neuronas,k)



############  datos prueba
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



vectorClusters <- c(0,1,1,1,2,2,1)
#############3
graficar(dataPesos,2,matrizAdjacencia,vectorClusters)

######


##otros datos   k = 3
#########################################################prueba 2
############  datos prueba
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
#############3
graficar(dataPesos,3,matrizAdjacencia,vectorClusters)
