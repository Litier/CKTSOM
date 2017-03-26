#library(devtools)
#install_github("Litier/BKTSOM")

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
ti <- proc.time() # start watch
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop watch
tf-ti #print execution time

##visualization phase
graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot


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

ti <- proc.time() # start watch
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop watch
tf-ti #print execution time

##visualization phase
graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot


######### prueba #View(trees)

###parameters
numberOfIterations <- 100000
initialLearningRate <- 0.9
finalLearningRate<- 0
initialRadius <- 3
finalRadius <- 0
numberOfChildrenperNode <- 2
treeHeight <- 2

##training phase
data(trees)
data<-trees ## load a dataset
ti <- proc.time() # start watch
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop watch
tf-ti #print execution time

##visualization phase
graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot
################################   LifeCycleSavings


data(LifeCycleSavings)
View(LifeCycleSavings)


data <- as.list(LifeCycleSavings[,1:5])
nombres <- names(LifeCycleSavings)
neuronas <- myFunc(k,h,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data, nombres)

graficar(LifeCycleSavings,neuronas,k)


data<-iris
c(1:nrow(data))
sample(c(1:nrow(data)))
sample(c(1:150))
