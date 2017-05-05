fit <- function(data, numberOfIterations = 600000,initialLearningRate = 1, finalLearningRate = 0,
                   initialRadius = 7,finalRadius = 1,numberOfChildrenperNode = 2,
                   treeHeight = 3, rejectionRate=0.05){


  ##validar que el trainingRatio estre entre 0-1
  if (!(rejectionRate > 0 && rejectionRate < 1)){
      writeLines("Rejection ratio ]0.0 , 1.0[")
      return(NA)
  }
  #########

  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
  clusterVector<- c(1:(2^(treeHeight+1)-1))

  ##Calculate the group of each data
  trainingDataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)
  trainingDistancias<- c(1:length(data[,1]))

  for (i in 1:length(data[,1])) {
    trainingDistancias[i] <- distancia_R(neurons[trainingDataBMU[i],],data[i,])
  }
  
  ##Calculate threshold
  sortedDist <- trainingDistancias[order(trainingDistancias, decreasing=T)]
  thrshld    <- sortedDist[floor(length(sortedDist)*rejectionRate)]

  som <- function(data) {
      testDataBMU    <- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)
      testDistancias <- c(1:length(data[,1]))
	  
	  for (i in 1:length(data[,1])) {
	    testDistancias[i] <- distancia_R(neurons[testDataBMU[i],],data[i,])
	  }
	  
	  predictions    <- rep(1,times=length(data[,1]))
	  predictions[which(testDistancias>thrshld)] <- -1
	  testDistancias     <- data.frame(testDistancias)
	  predictions        <- data.frame(predictions)
	  return (c(predictions, testDistancias))
  }
  training <- data.frame(data,trainingDataBMU,trainingDistancias)

return (c(som=som,training,thrshld=thrshld,neurons))

}
