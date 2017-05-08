# Self-Organizing Trees

Implemenation of the Self-Organizing Trees algorithm, a tree-based adaptation of the SOM.

#### DESCRIPTION

This R library has been designed for the construction and visualization of a k-ary Self-Organizing Trees.

In this implementation we consider complete k-ary trees that are defined using only two parameters, i.e, the depth of the tree and the number of children per node.

It is possible to train the tree using the Self-Organizing Maps (SOM) algorithm, but placed in the context of tree structures instead of the traditional grid structure. This requires an adaptation of the algorithm that implies a completely different behavior.

The SOM  is a machine learning algorithm that learns from data using a unsupervised learning paradigm, allowing the clustering of multidimensional data.

The Self Organizing Tree, present three main changes with respect to the SOM. First, it uses a tree structure instead of the traditional grid. Second, the neighborhood is defined through the hierarchical relationships of the trees up to the root, rather that the direct edges in the grid. Third the search for the fittest neuron, process known as the best matching unit search is performed on the hierarchical structure in log(n) time.

The graphical visualization is focused on a scatter plot that shows the tree connections and the weights as long as the data set being processed.

#### Instalation 

For installing the library you must execute the follwing command in R

```R
library(devtools)
install_github("Litier/CKTSOM")
```

#### Example of execution 

###### Example 1

```R
library(CKTSOM)
library(ggplot2)

#set SEED
#setSeed(147)
##################### EXAMPLE 1 : IRIS DATASET
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
ti <- proc.time() # start timer
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop timer
tf-ti #print execution time

##visualization phase
##Display phase without grouping
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot


##Manual grouping of neurons
clusterVector <-rep(0,15)
clusterVector[c(1:7)]<- 1
clusterVector[c(8:9)]<- 3
clusterVector[c(10:11)]<- 4
clusterVector[c(12:13)]<- 5
clusterVector[c(14:15)]<- 6

##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
```
###### Example 2
```R
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
```
##### Validation
```R
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
#result <- prueba(data = data,numberOfIterations = numberOfIterations,initialLearningRate = initialLearningRate,finalLearningRate = finalLearningRate,initialRadius = initialRadius,finalRadius = finalRadius,numberOfChildrenperNode = numberOfChildrenperNode, treeHeight = treeHeight,trainingRatio = 0.66)

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
clusterVector<- c(1:length(neurons[,1]))
clusterVector[1:7] <- 1
data <- training[-(5:6)]
dataBMU<- c(training$trainingDataBMU)
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)  #plot the scatter plot
###  plot  (4 cluster)
dataBMU[dataBMU == 15 ] <- 14
dataBMU[dataBMU == 13 ] <- 12
dataBMU[dataBMU == 11 ] <- 10
dataBMU[dataBMU == 9 ] <- 8

clusterVector<- c(1:length(neurons[,1]))
clusterVector[1:7] <- 1
clusterVector[c(8:9)]<- 8
clusterVector[c(10:11)]<- 10
clusterVector[c(12:13)]<- 12
clusterVector[c(14:15)]<- 14

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)  #plot the scatter plot


##################
###    Test    ###
##################
drop <- c("testDataBMU","testDistancias")
data = test[,!(names(test) %in% drop)]
### Test plot
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot
###  plot  (8 cluster)
clusterVector<- c(1:length(neurons[,1]))
clusterVector[1:7] <- 1
data <- test[-(5:6)]
dataBMU<- c(test$testDataBMU)
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU) #plot the scatter plot
###  plot  (4 cluster)
dataBMU[dataBMU == 15 ] <- 14
dataBMU[dataBMU == 13 ] <- 12
dataBMU[dataBMU == 11 ] <- 10
dataBMU[dataBMU == 9 ] <- 8

clusterVector<- c(1:length(neurons[,1]))
clusterVector[1:7] <- 1
clusterVector[c(8:9)]<- 8
clusterVector[c(10:11)]<- 10
clusterVector[c(12:13)]<- 12
clusterVector[c(14:15)]<- 14

clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU) #plot the scatter plot
```
