\name{findBMU}
\alias{findBMU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Busca el BMU de un dato
}
\description{
    Busca la neurona más cercana a un dato en particular
}
\usage{
findBMU(dato,prototipos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dato}{
    Dato al cual se buscara su BMU
}
 \item{prototipos}{
    Coleccion de datos de los cuales se seleccionara el BMU
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
vectorClustersDatos <- c(rep(0,150))
matrizPesos <- matrix(runif(7, 1.0, 5.5))
matrizPesos <- cbind(matrizPesos,runif(7, 3.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(7, 0.0, 9.5))
matrizPesos <- cbind(matrizPesos,runif(7, 7.0, 8.5))
neuronas <- data.frame(matrizPesos)
#Asigna un valor a cada neurona
for(i in 1:length(neuronas[,1])){
  n<- ceiling(runif(1,1,150))
  dataPesos[i,]  <- dataIris[n,]
}
#asigna el grupo de cada neurona
vectorClusters <- c(1,2,2,2,3,2,3)
# busca el BMU de cada dato de iris para luego asignar su grupo copiandolo del BMU
for (i in 1:150){
  n<- findBMU(dataIris[i,],neuronas)
  vectorClustersDatos[i] <- vectorClusters[n]
}
##vector que contiene el BMU de cada dato
vectorClustersDatos
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
