library(ggplot2)

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





graficarGrafo <- function(neuronas,datos,matrizAdjacencia,vectorClusters,vectorClustersDatos){

  gg1 = makePairs(datos)
  mega_iris = data.frame(gg1$all)
  mega_iris = data.frame(gg1$all, Categoria=rep(vectorClustersDatos, length=nrow(gg1$all)))

  ggneurona = makePairs(neuronas)
  mega_neuronas = data.frame(ggneurona$all)
  mega_neuronas = data.frame(ggneurona$all, Categoria=rep(vectorClusters, length=nrow(ggneurona$all)))

  # mi pairs plot
  p <-ggplot(mega_iris, aes_string(x = "x", y = "y")) +
    facet_grid(xvar ~ yvar, scales = "free")+
    geom_point(aes(colour=factor(Categoria)),size = 1.2)




  for (i in 1:length(neuronas[,1])) {
    j <- 1
    while (j < i){
      if (matrizAdjacencia[i,j] == 1){
        for(k in 0:length(neuronas[1,])*length(neuronas[1,])-length(neuronas[1,]) -1){

          p <- p + geom_path(data = mega_neuronas[c(i,j)+((length(neuronas[,1])*k)),])
        }
      }
      j<- j +1
    }






  }
  p <- p +geom_point(data = mega_neuronas,aes(colour=factor(Categoria)),size = 2, na.rm = TRUE, alpha=0.8)


  p

}
##########


