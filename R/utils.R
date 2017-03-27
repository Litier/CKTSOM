source("r/RcppExports.R")
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

