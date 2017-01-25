\name{graficarGrafo}
\alias{graficarGrafo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Funcion para graficar matriz de dispersion con un grafo sobrepuesto
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

}
\usage{
graficarGrafo(neuronas,data,matrizAdjacencia,neuronClusters,dataCluster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{neuronas}{
%%     ~~Describe \code{x} here~~
  Data frame de las neuronas
}  \item{data}{
%%     ~~Describe \code{x} here~~
  Data frame que contiene los puntos
}  \item{matrizAdjacencia}{
%%     ~~Describe \code{x} here~~
  Matriz que contiene las interconexiones del grafo
}  \item{neuronClusters}{
%%     ~~Describe \code{x} here~~
  Vector que contiene el numero del grupo al cual pertenece una neurona
}  \item{dataCluster}{
%%     ~~Describe \code{x} here~~
  Vector que contiene el numero del grupo al cual pertenece el dato
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dataIris <-  iris
dataIris <- dataIris[,-5]
names (dataIris) <-c("X1","X2","X3","X4")
vectorClustersDatos <- c(rep(1,25))
vectorClustersDatos <- c(vectorClustersDatos ,rep(2,45))
vectorClustersDatos <- c(vectorClustersDatos ,rep(3,30))

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

for(i in 1:length(dataPesos[,1])){
  n<- ceiling(runif(1,1,150))
  dataPesos[i,]  <- dataIris[n,]
}

vectorClusters <- c(1,2,2,2,3,2,3)

for (i in 1:150){
  n<- FindBMU(dataIris[i,],dataPesos)
  vectorClustersDatos[i] <- vectorClusters[n]
}

graficarGrafo(dataPesos,dataIris,matrizAdjacencia,vectorClusters,vectorClustersDatos)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
