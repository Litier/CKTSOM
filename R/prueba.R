prueba <- function(data){
  ###parameters
  numberOfIterations <- 600000
  initialLearningRate <- 1
  finalLearningRate<- 0
  initialRadius <- 7
  finalRadius <- 1
  numberOfChildrenperNode <- 2
  treeHeight <- 3

  largo <- length(data[,1])

  ##training
  ultimo <- 66*largo / 100
  training <- data[sample(1:largo), ]   ##desornena y guarda en training
  ##obtiene el 66%
  training <- training[1:ultimo,]

  ###test
  ultimo <- 33*largo / 100
  test <- data[sample(1:largo), ]   ##desornena y guarda
  ##obtiene el 33%
  test<- test[1:ultimo,]


  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, training)
  clusterVector<- c(1:15)
  ##training
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


  return (c(training,test))
}




