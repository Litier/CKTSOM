#' train entrena la red neuronal usando una topologia de arbol
#'
#' @param numberOfChildrenperNode A integer number
#' @param treeHeight A integer number
#' @param initialLearningRate A float number
#' @param finalLearningRate A float number
#' @param initialRadius A integer number
#' @param finalRadius A integer number
#' @param numberOfIterations A integer number
#' @param data A data frame
#'
#'
#' @return La red neuronal entrenada con topologia de un arbol k ario completo
#' @examples
#' library(ggplot2)
#' ##################### EXAMPLE 1 : IRIS DATASET
#' ###parameters
#' numberOfIterations <- 600000
#' initialLearningRate <- 1
#' finalLearningRate<- 0
#' initialRadius <- 7
#' finalRadius <- 1
#' numberOfChildrenperNode <- 2
#' treeHeight <- 3
#'
#' ##training phase
#' data(iris)
#' data<-iris[-5] ## load a dataset
#' ti <- proc.time() # start watch
#' neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
#' tf <-proc.time()    # stop watch
#' tf-ti #print execution time
#'
#' ##visualization phase
#' graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot
train <- function(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data){
  neurons <- train_Rcpp(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations,as.list(data), names(data))
  return(neurons)
}


trainSOM <- function(numberColumn, numberRow, initialLearningRate, finalLearningRate,initialRadius, finalRadius,numberOfIterations, data){
  neurons <- trainSOM_Rcpp(numberColumn, numberRow, initialLearningRate, finalLearningRate,initialRadius, finalRadius,numberOfIterations,as.list(data), names(data))
  return(neurons)
}




## el BMU para cada dato del data set
calculateBMUForData <- function(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight) {
  dataBMU<- rep(0,length(data[,1]))
  ini<-(length(neurons[,1])-(numberOfChildrenperNode**treeHeight)+1)
  fin<-length(neurons[,1])
  for (i in c(1:length(dataBMU))) {
    dataBMU[i]<-findBMU(neurons[c(ini:fin),],data[i,])
  }
  dataBMU<- dataBMU+ini-1
  for(i in 1:length(dataBMU)){
    dataBMU[i]<- clusterVector[dataBMU[i]]
  }
  return(dataBMU)
}

"
FindBMU_tree_C <- function(dataNeuron,dataStimulus,numberOfChildrenperNode,treeHeight){
bmu <- FindBMU_tree( dataNeuron, dataStimulus, numberOfChildrenperNode,  treeHeight)
return(bmu + 1)
}
"


#point1 , point2::Data.Frame
calculateDistance <- function(point1, point2){
  return (calculateEuclideanDistance (point1[1,],point2[1,] ))
}

setSeed <- function(semilla=123){
  set_seed(semilla) ## c++
  set.seed(semilla) ## R
}


calculateBMUandDistance <- function(dataNeuron,dataStimulus,numberOfChildrenperNode,treeHeight){
  result <- findBmuAndDistance(dataNeuron,dataStimulus,numberOfChildrenperNode,treeHeight)
  result[1] <- result[1] + 1
  return(result)
}

calculateGroups <- function(numberOfGroups,numberOfChildrenperNode,treeHeight){
  level <- 0
  levelGroup <- numberOfChildrenperNode**level
  while (levelGroup < numberOfGroups){
    level<- level +1
    levelGroup <- numberOfChildrenperNode**level
  }
  size <- calculateNumberOfNeurons(numberOfChildrenperNode,treeHeight)
  id <- 1
  groups <- rep(id,size)

  ini <- min(buscaHijos(level,numberOfChildrenperNode))
  fin <- ini + numberOfChildrenperNode ** (level) -1
  for(i in c(ini:fin)){
    id <- id+1
    groups[i] <- id
    groups <- marcarHijos(i,numberOfChildrenperNode,treeHeight,size,groups )
  }

  return(groups)
}

marcarHijos<- function(node,numberOfChildrenperNode,treeHeight,size,groups ){
  hijos <- buscaHijos(node,numberOfChildrenperNode)
  if(hijos[1]<size){
    for(i in hijos){
      groups[i] <- groups[node]
      groups <- marcarHijos(i,numberOfChildrenperNode,treeHeight,size,groups )
    }
  }
  return(groups)
}

calculateNumberOfNeurons<- function( numberOfChildrenperNode, treeHeight){
  sum <- 0
  for (i in c(0:treeHeight)) {
    sum <- sum + numberOfChildrenperNode**i;
  }
  return(sum)
}

getOutlayers <- function(neurons,data ,numberOfChildrenperNode,treeHeight){
  clusterVector<- c(1:length(neurons[,1]))
  ## calculate the bmu and distance for each data
  result <-matrix(ncol = 2,nrow = length(data[,1]))
  for (i in 1:length(data[,1])) {
    stimulus <- data[i,]
    result[i,]<-calculateBMUandDistance(neurons,stimulus, numberOfChildrenperNode, treeHeight)
  }
  ## calculate mu and sigma
  ##media (mu)
  media <- mean(result[,2])
  #desviacion estandar (o desviación típica)  (sigma)
  desviacionEstandar <- sd(result[,2])

  #generate  Z-Score
  #(d - (mu) )  / (sigma)
  Zscore <- vector(length = length(result[,1]))
  Zscore<- (result[,2] - media)/desviacionEstandar
  ##get outlayers
  #z-score < 2sigma
  outlayers <- c(1:length(data[,1]))
  outlayers <- outlayers[Zscore > 2*desviacionEstandar]
  return(outlayers)
}
