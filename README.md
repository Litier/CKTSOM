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
install_github("Litier/BKTSOM")
```

#### Example of execution 

###### Example 1

```R
library(BKTSOM)
library(ggplot2)
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
graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot
```
###### Example 2
```R
library(BKTSOM)
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
graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot
```

