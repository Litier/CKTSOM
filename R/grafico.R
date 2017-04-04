library(ggplot2)

makePairs <- function(data)
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol],
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

buscaPadre <- function(n,k){
  a<-(n+k-2)%/%k
  return(a)
}

graficar <- function(datos,neuronas,k){
  gg1 = makePairs(datos)
  mega_iris = data.frame(gg1$all)

  ggneurona = makePairs(neuronas)
  mega_neuronas = data.frame(ggneurona$all)
  #mega_neuronas = data.frame(ggneurona$all, Species=rep(iris$Species, length=nrow(ggneurona$all)))

  # mi pairs plot
  p <-ggplot(mega_iris, aes_string(x = "x", y = "y")) +
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(colour = "red",size = 1) +
    geom_point(data = mega_neuronas,size = 1.5,shape = 19)
  color <- 1
  ##marca las lineas, agregandole el color por nivel
  for (i in 1:buscaPadre(length(neuronas[,1]),k)) {
    if (k^color == i) {
      color <- color +1
    }
    for(j in 0:(length(mega_iris[,1]) /length(datos[,1])-1)){
      p <- p + geom_path(data = mega_neuronas[miniLista2(i,k)+((length(neuronas[,1])*j)),],colour = color)
    }
  }
  p
}


##mejorar el codigo de grafico //tiene basura
clusterVisualization <- function(datos,neuronas,numberOfChildrenperNode,vectorClusters = NA,vectorClusterData = NA){
  #busca el BMU de cada dato
  if(is.na(vectorClusters) || is.na(vectorClusterData)){
    gg1 = makePairs(datos)
    mega_iris = data.frame(gg1$all)

    ggneurona = makePairs(neuronas)
    mega_neuronas = data.frame(ggneurona$all)
    #mega_neuronas = data.frame(ggneurona$all, Species=rep(iris$Species, length=nrow(ggneurona$all)))

    # mi pairs plot
    p <-ggplot(mega_iris, aes_string(x = "x", y = "y")) +
      facet_grid(xvar ~ yvar, scales = "free") +
      geom_point(colour = "red",size = 1) +
      geom_point(data = mega_neuronas,size = 1.5,shape = 19)
    color <- 1
    ##marca las lineas, agregandole el color por nivel
    for (i in 1:buscaPadre(length(neuronas[,1]),numberOfChildrenperNode)) {
      if (numberOfChildrenperNode^color == i) {
        color <- color +1
      }
      for(j in 0:(length(mega_iris[,1]) /length(datos[,1])-1)){
        p <- p + geom_path(data = mega_neuronas[miniLista2(i,numberOfChildrenperNode)+((length(neuronas[,1])*j)),],colour = color)
      }
    }
    p
  }else{
    gg1 = makePairs(datos)
    mega_data = data.frame(gg1$all, Categoria=rep(vectorClusterData, length=nrow(gg1$all)))


    ggneurona = makePairs(neuronas)
    mega_neuronas = data.frame(ggneurona$all, Categoria=rep(vectorClusters, length=nrow(ggneurona$all)))

    # mi pairs plot
    p <-ggplot(mega_data, aes_string(x = "x", y = "y")) +
      facet_grid(xvar ~ yvar, scales = "free") +
      geom_point(colour = mega_data$Categoria,size = 1,alpha=0.55)

    color <- 1
    ##marca las lineas, agregandole el color por nivel
    for (i in 1:buscaPadre(length(neuronas[,1]),numberOfChildrenperNode)) {
      if (numberOfChildrenperNode^color == i) {
        color <- color +1
      }
      for(j in 0:(length(mega_data[,1]) /length(datos[,1])-1)){
        p <- p + geom_path(data = mega_neuronas[miniLista2(i,numberOfChildrenperNode)+((length(neuronas[,1])*j)),],colour = color)
      }
    }
    p <- p + geom_point(data = mega_neuronas,size = 1.6, stroke = 1,shape = 21,colour = "black", fill =mega_neuronas$Categoria,alpha=1)
    p
  }
}

######################3
##revisar codigo debajo //eliminar basura
############################
miniLista2 <- function(padre,k){
  hijos <- buscaHijos(padre ,k)
  nimil <- c(hijos[1])
  for (i in hijos[2:length(hijos)]) {
    nimil <- c(nimil,padre,i)
  }

  return(nimil)
}


buscaHijos <- function(numero,k){
  a<- c()
  n<- 1
  for (i in 1:k) {
    a<-c(k*numero+n,a)
    n<- n-1
  }
  return(a)
}

graficarColoro <- function(datos,neuronas,numberOfChildrenperNode,vectorClusters,vectorClusterData){
  #busca el BMU de cada dato

  gg1 = makePairs(datos)
  mega_data = data.frame(gg1$all, Categoria=rep(vectorClusterData, length=nrow(gg1$all)))


  ggneurona = makePairs(neuronas)
  mega_neuronas = data.frame(ggneurona$all, Categoria=rep(vectorClusters, length=nrow(ggneurona$all)))

  # mi pairs plot
  p <-ggplot(mega_data, aes_string(x = "x", y = "y")) +
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(colour = mega_data$Categoria,size = 1,alpha=0.55)

  color <- 1
  ##marca las lineas, agregandole el color por nivel
  for (i in 1:buscaPadre(length(neuronas[,1]),numberOfChildrenperNode)) {
    if (numberOfChildrenperNode^color == i) {
      color <- color +1
    }
    for(j in 0:(length(mega_data[,1]) /length(datos[,1])-1)){
      p <- p + geom_path(data = mega_neuronas[miniLista2(i,numberOfChildrenperNode)+((length(neuronas[,1])*j)),],colour = color)
    }
  }
  p <- p + geom_point(data = mega_neuronas,size = 1.6, stroke = 1,shape = 21,colour = "black", fill =mega_neuronas$Categoria,alpha=1)
  p
}



miniLista <- function(padre,k,matriz){
  hijos <- buscaHijos(padre ,k)
  nimil<- c()
  for (i in hijos){
    if( matriz[i,padre] == 1){
      nimil <- c(nimil,padre,i)
    }

  }

  return(nimil)
}












graficar2 <- function(neuronas,k,matrizAdjacencia,vectorClusters){
  largo = length(neuronas[,1])-1

  ##gg1 = makePairs(datos)
  #  mega_iris = data.frame(gg1$all)
  ##mega_iris = data.frame(gg1$all, Categoria=rep(Categorias$I, length=nrow(gg1$all)))


  ggneurona = makePairs(neuronas)
  mega_neuronas = data.frame(ggneurona$all)
  mega_neuronas = data.frame(ggneurona$all, Categoria=rep(vectorClusters, length=nrow(ggneurona$all)))

  # mi pairs plot
  p <-ggplot(mega_neuronas, aes_string(x = "x", y = "y")) +
    facet_grid(xvar ~ yvar, scales = "free")

  color <- 1

  for (i in 1:buscaPadre(length(neuronas[,1]),k)) {
    if (k^color == i) {
      color <- color +1
    }
    for(j in 0:11){

      p <- p + geom_path(data = mega_neuronas[miniLista(i,k,matrizAdjacencia)+((length(neuronas[,1])*j)),],colour = color)
    }


  }
  ##i <- 84
  ##while (i >0){
  ##  mega_neuronas <- mega_neuronas[-c(i), ]
  ##  i <- i - 7
  ##}

  p <- p +geom_point(data = mega_neuronas,aes(colour=factor(Categoria)), na.rm = TRUE, alpha=0.8)


  p

}


##########################3
