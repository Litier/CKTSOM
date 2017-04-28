prueba <- function(data,numberOfIterations = 600000,initialLearningRate = 1, finalLearningRate = 0,
                   initialRadius = 7,finalRadius = 1,numberOfChildrenperNode = 2,
                   treeHeight = 3,trainingRatio = 0.66){

  ##validar que el trainingRatio estre entre 0-1
  if (trainingRatio > 0 && trainingRatio < 1){
    largo <- length(data[,1])

    ##training and test
    lastTraining <- trainingRatio * largo
    data <- data[sample(1:largo), ]   ##desornena y guarda en training
    training <- data[1:lastTraining,]
    test<- data[(lastTraining+1):largo,]
    #########

    neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, training)
    clusterVector<- c(1:15)

    ##Calculate the group of each data
    TrainingDataBMU<- calculateBMUForData(training,neurons,clusterVector,numberOfChildrenperNode,treeHeight)
    TrainingDistancias<- c(1:length(training[,1]))

    for (i in 1:length(training[,1])) {
      TrainingDistancias[i] <- distancia_R(neurons[TrainingDataBMU[i],],training[i,])
    }
    ###test
    ##Calculate the group of each data
    TestDataBMU<- calculateBMUForData(test,neurons,clusterVector,numberOfChildrenperNode,treeHeight)
    TestDistancias<- c(1:length(test[,1]))

    for (i in 1:length(test[,1])) {
      TestDistancias[i] <- distancia_R(neurons[TestDataBMU[i],],test[i,])
    }


    training <- data.frame(training,TrainingDataBMU,TrainingDistancias)
    test <- data.frame(test,TestDataBMU,TestDistancias)


    return (c(training,test,neurons))
  } else {
    writeLines("trainingRatio ]0.0 , 1.0[")
    return(NA)
  }
}
