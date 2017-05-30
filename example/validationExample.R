library(CKTSOM)
library(ggplot2)
##Set seed to generate Tree
#set_seed(543)
##################### IRIS DATASET
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
treeHeight <- 3


data(iris)
data<-iris[-5] ## load a dataset
##Execution algorithm
result <- validate(data = data)
#using parameters for training
#result <- validate(data = data,numberOfIterations = numberOfIterations,initialLearningRate = initialLearningRate,finalLearningRate = finalLearningRate,initialRadius = initialRadius,finalRadius = finalRadius,numberOfChildrenperNode = numberOfChildrenperNode, treeHeight = treeHeight,trainingRatio = 0.66)

####Split result
##Training
training <- data.frame(result[1:((length(result)-length(data))/2)])
##Test
test <- data.frame(result[(((length(result)-length(data))/2 )+ 1):(length(result)-length(data))])
##Neurons tree
neurons <- data.frame(result[(length(result)-length(data)+1):(length(result))])


##################
###  Training  ###
##################
drop <- c("trainingDataBMU","trainingDistancias")
data = training[,!(names(training) %in% drop)]
###   training plot
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot


###   plot  (8 cluster)
numberofGroups <- 8
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)
dataBMU<- c(training$trainingDataBMU)

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)  #plot the scatter plot
###  plot  (4 cluster)
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)  #plot the scatter plot


##################
###    Test    ###
##################
drop <- c("testDataBMU","testDistancias")
data = test[,!(names(test) %in% drop)]
### Test plot
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot
###  plot  (8 cluster)
numberofGroups <- 8
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)
dataBMU<- c(test$testDataBMU)

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU) #plot the scatter plot
###  plot  (4 cluster)
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)  #plot the scatter plot
