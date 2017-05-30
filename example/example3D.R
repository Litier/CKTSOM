library(CKTSOM)
library("rgl")

#set SEED
#setSeed(147)


##################### Example 3D: IRIS DATA
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
treeHeight <- 3

##training phase
data(iris)
data<-iris[-5] ## load a dataset
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)

##Grouping of neurons
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
plot3d(data$Sepal.Length,data$Sepal.Width,data$Petal.Length,size = 5,col= dataBMU,axes = FALSE,xlab ="",ylab ="",zlab ="")
points3d(neurons$Sepal.Length,neurons$Sepal.Width,neurons$Petal.Length,col=clusterVector,size = 7)
for (i in c(1:(length(neurons[,1])  - numberOfChildrenperNode ** treeHeight))) {
  mini <- miniLista(i,numberOfChildrenperNode)
  lines3d(neurons$Sepal.Length[mini],neurons$Sepal.Width[mini],neurons$Petal.Length[mini],size = 5,color="black")
}


##move plot
movie3d(spin3d(axis = c(0,0,1), rpm = 1), duration=120,  type = "png")
