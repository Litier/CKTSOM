validate <- function(data,numberOfIterations = 600000,initialLearningRate = 1, finalLearningRate = 0,
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
    clusterVector<- c(1:length(neurons[,1]))

    ##Calculate the group of each data
    ####################Training
    trainingResult <-matrix(ncol = 2,nrow = length(training[,1]))
    colnames(trainingResult) <- c("trainingDataBMU","trainingDistancias")
    for (i in 1:length(training[,1])) {
      stimulus <- training[i,]
      trainingResult[i,]<-calculateBMUandDistance(neurons,stimulus, numberOfChildrenperNode, treeHeight)
    }


    ####################Test
    testResult <-matrix(ncol = 2,nrow = length(test[,1]))
    colnames(testResult) <- c("testDataBMU","testDistancias")
    for (i in 1:length(test[,1])) {
      stimulus <- test[i,]
      testResult[i,]<-calculateBMUandDistance(neurons,stimulus, numberOfChildrenperNode, treeHeight)
    }

    training <- data.frame(training,trainingResult)
    test <- data.frame(test,testResult)


    return (c(training,test,neurons))
  } else {
    writeLines("trainingRatio ]0.0 , 1.0[")
    return(NA)
  }
}
