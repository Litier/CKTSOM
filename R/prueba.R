library(ggplot2)
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



##BKTSOM::graficar(dataPesos,2,matrizAdjacencia,vectorClusters)


length(dataPesos[,1])


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



vectorClusters <- c(1,2,2,2,3,2,3)
#############3

############  datos prueba
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

for(i in 1:length(dataPesos[,1])){
  n<- ceiling(runif(1,1,150))
  dataPesos[i,]  <- dataIris[n,]
}

vectorClusters <- c(1,2,2,2,3,2,3)

for (i in 1:150){
  n<- findBMU(dataIris[i,],dataPesos)
  vectorClustersDatos[i] <- vectorClusters[n]
}

graficarGrafo(dataPesos,dataIris,matrizAdjacencia,vectorClusters,vectorClustersDatos)


