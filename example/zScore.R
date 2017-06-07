library(CKTSOM)
library(ggplot2)
##Set seed to generate Tree
##setSeed(543)
##################### IRIS DATASET
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3


data(iris)
data<-iris[-5] ## load a dataset
##Execution algorithm
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)


##Grouping of neurons
numberofGroups <- 3
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)

########
########     Outlayers
########
outliers <- getOutlayers(neurons,data ,numberOfChildrenperNode,treeHeight)


#delete outlayer of data
procesData <- data[-outliers,]

##Grouping of neurons
numberofGroups <- 3
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(procesData,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(procesData,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
