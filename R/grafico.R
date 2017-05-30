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
      p <- p + geom_path(data = mega_neuronas[miniLista(i,k)+((length(neuronas[,1])*j)),],colour = color)
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
        p <- p + geom_path(data = mega_neuronas[miniLista(i,numberOfChildrenperNode)+((length(neuronas[,1])*j)),],colour = color)
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
        p <- p + geom_path(data = mega_neuronas[miniLista(i,numberOfChildrenperNode)+((length(neuronas[,1])*j)),],colour = color)
      }
    }
    p <- p + geom_point(data = mega_neuronas,size = 1.6, stroke = 1,shape = 21,colour = "black", fill =mega_neuronas$Categoria,alpha=1)
    p
  }
}

miniLista <- function(padre,k){
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

######problemas con muchos plots y malla grande
visualizationSOM <- function(datos,neuronas,numberColumn){
  gg1 = makePairs(datos)
  mega_iris = data.frame(gg1$all)

  ggneurona = makePairs(neuronas)
  mega_neuronas = data.frame(ggneurona$all)

  #mega_neuronas = data.frame(ggneurona$all, Species=rep(iris$Species, length=nrow(ggneurona$all)))
  # mi pairs plot
  p <-ggplot(mega_iris, aes_string(x = "x", y = "y")) +
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(colour = "red",size = 1)


  ##marca las lineas, agregandole el color por nivel
  for (i in 1:length(neuronas[,1])) {

    for(j in 0:(length(mega_neuronas[,1]) /length(neuronas[,1])-1)){
      if (i %% numberColumn != 0) {
        p <- p + geom_path(data = mega_neuronas[c(i,i+1)+((length(neuronas[,1])*j)),])
      }
      #dibuja la linea de un punto a otro que se encuentra debajo de este "tomando la lista
      #como si fuera una matriz"
      if (i+ numberColumn <= length(neuronas[,1])) {
        p <- p + geom_path(data = mega_neuronas[c(i,i+numberColumn)+((length(neuronas[,1])*j)),])
      }
    }
  }
  p <- p + geom_point(data = mega_neuronas,size = 1.5,shape = 19)

  p
}
