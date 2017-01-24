# BKTSOM
binary complete k-ary self organizing maps

#### Descripción 

Librería diseñada para R la cual permite la construcción y visualización de un árbol k-anario completo. 
Para la construcción del árbol se utilizó Self-Organizing Maps (SOM) que es un tipo de red neuronal  el cual tiene la capacidad de realizar aprendizaje automático y se caracteriza por poder agrupar datos multidimensionales.
La visualización gráfica está enfocada en una matriz de gráficos de dispersión.

#### Instalar 

Para instalar la librería se debe ejecutar el siguiente comando en R
```R
library(devtools)
install_github("Litier/BKTSOM")
```

#### Ejemplo ejecución 

###### Ejemplo 1

```R
#Inicializar variables
matrizAdjacencia <- matrix(data = rep(0,49),nrow = 7)
matrizAdjacencia[2,1]<- 1
matrizAdjacencia[3,1]<- 1
matrizAdjacencia[4,2]<- 1
matrizAdjacencia[5,2]<- 1
matrizAdjacencia[6,3]<- 1      # 3 -- 6

matrizPesos <- matrix(runif(7, 1.0, 5.5))
matrizPesos <- cbind(matrizPesos,runif(7, 3.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(7, 0.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(7, 7.0, 8.5))
dataPesos <- data.frame(matrizPesos)

vectorClusters <- c(0,1,1,1,2,2,1)

#Grafico
BKTSOM::graficar(dataPesos,2,matrizAdjacencia,vectorClusters)
```
###### Ejemplo 2
```R
#Inicializar variables
matrizAdjacencia <- matrix(data = rep(0,169),nrow = 13)
matrizAdjacencia[2,1]<- 1
matrizAdjacencia[3,1]<- 1
matrizAdjacencia[4,1]<- 1
matrizAdjacencia[5,2]<- 1
matrizAdjacencia[6,2]<- 1
matrizAdjacencia[7,2]<- 1
matrizAdjacencia[8,3]<- 1      # 3 -- 6
matrizAdjacencia[9,3]<- 1
matrizAdjacencia[10,4]<- 1
matrizPesos <- matrix(runif(13, 1.0, 5.5))
matrizPesos <- cbind(matrizPesos,runif(13, 3.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(13, 0.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(13, 7.0, 8.5))
dataPesos <- data.frame(matrizPesos)
vectorClusters <- c(0,1,1,1,2,2,2,3,3,3,4,4,4)

#Grafico
BKTSOM::graficar(dataPesos,3,matrizAdjacencia,vectorClusters)
```
