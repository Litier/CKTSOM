library(CKTSOM)
library(ggplot2)

##################### EXAMPLE 2 : LIFECYLCE DATA
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
treeHeight <- 3

##training phase
data(LifeCycleSavings)
data<-LifeCycleSavings ## load a dataset
##remove outliers
data<-data[!(data$ddpi>10 | data$sr>20),]

ti <- proc.time() # start timer
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop timer
tf-ti #print execution time

##visualization phase
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot
###########
#####################    visualization 2
###########
##Manual grouping of neurons
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
