library(CKTSOM)
library(ggplot2)

#set SEED
setSeed(140)


##################### EXAMPLE 1 : IRIS DATASET
###parameters
numberOfIterations <- 100000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 4
finalRadius <- 0
numberColumn <- 5
numberRow <- 5

##training phase
data(iris)
data<-iris[-5] ## load a dataset
neurons <- trainSOM(numberColumn, numberRow, initialLearningRate, finalLearningRate,initialRadius, finalRadius,numberOfIterations, data)


##visualization phase
##Display phase without grouping
visualizationSOM(data,neurons,numberColumn)  #plot the scatter plot
