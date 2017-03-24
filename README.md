# Self-Organizing Trees

Implemenation of the Self Organizing maps algorithm.

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
library(ggplot2)
#Initializing variables
AdjacencyMatrix <- matrix(data = rep(0,49),nrow = 7)
AdjacencyMatrix[2,1]<- 1
AdjacencyMatrix[3,1]<- 1
AdjacencyMatrix[4,2]<- 1
AdjacencyMatrix[5,2]<- 1
AdjacencyMatrix[6,3]<- 1      # 3 -- 6

WeightMatrix <- matrix(runif(7, 1.0, 5.5))
WeightMatrix <- cbind(WeightMatrix,runif(7, 3.0, 9.5))
WeightMatrix <- cbind(WeightMatrix,runif(7, 0.0, 9.5))
WeightMatrix <- cbind(WeightMatrix,runif(7, 7.0, 8.5))
WeightDF <- data.frame(WeightMatrix)

ClusterVectors <- c(0,1,1,1,2,2,1)

#Grafico
BKTSOM::graficar(WeightDF,2,AdjacencyMatrix,ClusterVectors)
```
###### Example 2
```R
library(ggplot2)
#Initializing variables
AdjacencyMatrix <- matrix(data = rep(0,169),nrow = 13)
AdjacencyMatrix[2,1]<- 1
AdjacencyMatrix[3,1]<- 1
AdjacencyMatrix[4,1]<- 1
AdjacencyMatrix[5,2]<- 1
AdjacencyMatrix[6,2]<- 1
AdjacencyMatrix[7,2]<- 1
AdjacencyMatrix[8,3]<- 1      # 3 -- 6
AdjacencyMatrix[9,3]<- 1
AdjacencyMatrix[10,4]<- 1
WeightMatrix <- matrix(runif(13, 1.0, 5.5))
WeightMatrix <- cbind(WeightMatrix,runif(13, 3.0, 9.5))
WeightMatrix <- cbind(WeightMatrix,runif(13, 0.0, 9.5))
WeightMatrix <- cbind(WeightMatrix,runif(13, 7.0, 8.5))
WeightDF <- data.frame(WeightMatrix)
ClusterVectors <- c(0,1,1,1,2,2,2,3,3,3,4,4,4)

#the plot
BKTSOM::graficar(WeightDF,3,AdjacencyMatrix,ClusterVectors)
```
###### Example 3. plotting the graph
```R
library(ggplot2)
dataIris <-  iris
dataIris <- dataIris[,-5]
names (dataIris) <-c("X1","X2","X3","X4")
vectorClustersDatos <- c(rep(1,25))
vectorClustersDatos <- c(vectorClustersDatos ,rep(2,45))
vectorClustersDatos <- c(vectorClustersDatos ,rep(3,30))

AdjacencyMatrix <- matrix(data = rep(0,49),nrow = 7)
AdjacencyMatrix[2,1]<- 1
AdjacencyMatrix[3,1]<- 1
AdjacencyMatrix[4,2]<- 1
AdjacencyMatrix[5,2]<- 1
AdjacencyMatrix[6,3]<- 1      # 3 -- 6

MatrixWeights <- matrix(runif(7, 1.0, 5.5))
MatrixWeights <- cbind(MatrixWeights,runif(7, 3.0, 9.5))
MatrixWeights <- cbind(MatrixWeights,runif(7, 0.0, 9.5))
MatrixWeights <- cbind(MatrixWeights,runif(7, 7.0, 8.5))
dataDF <- data.frame(MatrixWeights)
#Inicializa las neuronas copiando los datos de forma aleatoria
for(i in 1:length(dataDF[,1])){
  n<- ceiling(runif(1,1,150))
  dataDF[i,]  <- dataIris[n,]
}
#Define el grupo de cada neurona
ClusterVectors <- c(1,2,2,2,3,2,3)
#Asigna el BMU a cada dato
for (i in 1:150){
  n<- BKTSOM::findBMU(dataIris[i,],dataDF)
  ClusterVectorsDF[i] <- vectorClusters[n]
}
#Grafica

#BKTSOM::graficarGrafo(dataIris,dataDF,AdjacencyMatrix)
#BKTSOM::graficarGrafo(dataIris,dataDF,AdjacencyMatrix,ClusterVectors)
BKTSOM::graficarGrafo(dataIris,dataDF,AdjacencyMatrix,ClusterVectors,ClusterVectorsDF)
```
