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


findBMU <- function(listNeuron,stimulus){
  BMU <- findBMU_Rcpp(listNeuron,stimulus)
  return(BMU)
}



## el BMU para cada dato del data set
calculateBMUForData <- function(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight) {
  dataBMU<- rep(0,length(data[,1]))
  ini<-(length(neurons[,1])-(numberOfChildrenperNode**treeHeight)+1)
  fin<-length(neurons[,1])
  for (i in c(1:length(dataBMU))) {
    dataBMU[i]<-findBMU_R(data[i,],neurons[c(ini:fin),])
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
