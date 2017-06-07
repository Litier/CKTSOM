#include <Rcpp.h>
#include <string>
#include <stdlib.h>     /* srand, rand */
#include <time.h>

using namespace Rcpp;


/**
* Compute the number of neurons in the tree considering the depth of the tree as well as the number of children per node.
*/
int calculateNumberOfNeurons(int numberOfChildrenperNode, int treeHeight){
  int sum = 0 ;
  for (int i = 0 ; i <= treeHeight ; i++ ){
    sum = sum + pow(numberOfChildrenperNode,i);
  }
  return sum;
}

/**
* Genera un vector de los hijos de una neurona.
* TODO: explicar
*/
Rcpp::NumericVector findChildren(int neuron,int  numberOfChildrenperNode){
  Rcpp::NumericVector children(numberOfChildrenperNode);
  neuron++;  //aumento el numero para tener la lista que empiesa 1
  int n = 0;
  for (int i = 1; i <=numberOfChildrenperNode ;i++){
    children[numberOfChildrenperNode -i] = numberOfChildrenperNode*neuron+n;  //disminuye el numero para una lista que empieza en 0
    n = n -1;
  }
  return children;
}

//obtiene el padre de una neurona
//N - (N%k)
int findFather(int neuron,int numberOfChildrenperNode){
  neuron++;  //aumento el numero para tener la lista que empiesa 1
  int father = (neuron + numberOfChildrenperNode - 2 )/numberOfChildrenperNode;
  return father-1;  //disminulle el numero para una lista que empiesa en 0
}

//obtiene los hermanos de una neurona, retornando un vector (incluyendo la neurona )
Rcpp::NumericVector findBrothers(int neuron ,int numberOfChildrenperNode){
  int father = findFather(neuron,numberOfChildrenperNode);
  Rcpp::NumericVector brothers = findChildren(father,numberOfChildrenperNode);
  return brothers;
}

//mueve la neurona al estimulo
Rcpp::NumericVector updateNeuron(Rcpp::NumericVector neuron, Rcpp::NumericVector stimulus,float learningRate){
  for (int i =0 ; i <neuron.size(); i++){
    neuron[i] = neuron[i] - learningRate* (neuron[i] - stimulus[i]);
  }
  return neuron;
}

//actualiza el arbol
NumericMatrix updateStructure(NumericMatrix neurons, NumericVector stimulus,
                              float radius, float learningRate, int BMU, int numberOfChildrenperNode){
  //mueve el BM
  neurons(BMU,_) =updateNeuron(neurons(BMU,_),stimulus,learningRate);

  //busca el padre del BMU
  int father = findFather(BMU,numberOfChildrenperNode);

  //disminuye la tasa de aprendizaje, ya que en cada nivel que sube el entrenamiento
  //se encuentra mas lejos del BMU y mas cerca de la raiz
  learningRate =learningRate * 0.9;

  //mueve el padre
  if (father >= 0 && radius > 1){
    neurons = updateStructure(neurons, stimulus, radius,  learningRate,  father,  numberOfChildrenperNode);


  }

  //mueve hermano
  if(BMU > 0 && radius > 1){
    Rcpp::NumericVector brothers = findBrothers(BMU,numberOfChildrenperNode);
    for (int i = 0; i < brothers.size();i++){
      int brother = brothers[i];
      //revisa para no mover el BMU
      if (brother!= BMU){
        //mueve los hermanos reduciendo la tasa de aprendizaje aun mas, para evitar que se junte con el BMU
        neurons(brother,_) =updateNeuron(neurons(brother,_),stimulus,learningRate*0.2);
      }
    }
  }
  return neurons;
}



//calcula la distancia eucludiana entre 2 puntos
float calculateEuclideanDistance2Point (NumericVector point1,NumericVector point2 ){
  NumericVector resta(point1.size());
  point1[is_na(point1)] = 0;
  point2[is_na(point2)] = 0;

  resta = point1 - point2;
  resta = resta*resta;
  float disc = sum(resta);
  float d = sqrt(disc);
  return d;
}

// [[Rcpp::export]]
float calculateEuclideanDistance (DataFrame point1,DataFrame point2){
  NumericMatrix point1Matrix = internal::convert_using_rfunction(point1, "as.matrix");
  NumericVector point1Vector = point1Matrix(0,_);

  NumericMatrix point2Matrix = internal::convert_using_rfunction(point2, "as.matrix");
  NumericVector point2Vector = point2Matrix(0,_);
  return calculateEuclideanDistance2Point(point1Vector,point2Vector);
}

//calcula la distancia del estimulo a las neuronas
NumericVector distance (NumericMatrix neurons,NumericVector stimulus){

  NumericVector distances(neurons(_,1).size());
  for (int i = 0; i <distances.size();i++ ){
    distances(i) = calculateEuclideanDistance2Point(neurons(i,_),stimulus);
  }
  return distances;
}

//Busca el BMU partiendo en la raiz, llegando a las hojas
int FindBMU_tree(NumericVector stimulus,NumericMatrix neurons,int numberOfChildrenperNode, int treeHeight){
  int BMU = 0;
  int lastfather = (neurons(_,0).size()-1)-pow(numberOfChildrenperNode,treeHeight);

  while (BMU <= lastfather){
    //busca las neuronas del siguiente nivel
    NumericVector children  = findChildren(BMU,numberOfChildrenperNode);
    //genera un vector con las neuronas del nivel
    NumericMatrix neuronsChildren(children.size(),stimulus.size());
    for(int i = 0; i < children.size(); i++){
      neuronsChildren(i,_) = neurons(children[i],_);
    }
    //calcula la distancia de el estimulo a las neuronas del nivel
    NumericVector dist = distance(neuronsChildren,stimulus);
    NumericVector::iterator it = std::min_element(dist.begin(), dist.end());
    int indexBestNeuron = it - dist.begin();
    //cambia el BMU al hijo mas cercano
    BMU = children[indexBestNeuron];
  }
  return BMU;
}

//desordena el set de datos
NumericMatrix disorder(NumericMatrix data){
  int random1,random2;
  for(int i = 0; i < data(_,0).size()*2/3;i++){
    random1 = rand() % data(_,0).size();
    NumericVector dataTemp = data(random1,_);
    random2 = rand() % data(_,0).size();
    data(random1,_)=data(random2,_);
    data(random2,_)=dataTemp;
  }
  return data;
}

// [[Rcpp::export]]
Rcpp::DataFrame train_Rcpp(int numberOfChildrenperNode,int treeHeight,float initialLearningRate ,float finalLearningRate,
                       int initialRadius,int  finalRadius, int iterations
                         , Rcpp::List lst,
                         Rcpp::CharacterVector Names = Rcpp::CharacterVector::create()) {

  int columnLength = lst.size();
  int neuronsSize = calculateNumberOfNeurons(numberOfChildrenperNode,treeHeight);
  SEXP ll = lst[0];
  Rcpp::NumericVector y(ll);
  int dataSize = y.size();

  Rcpp::NumericMatrix data(dataSize,columnLength);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    data( _ ,i) =y;
  }
  //Datos listos para trabajar en matriz

  Rcpp::NumericMatrix neurons(neuronsSize,columnLength);

  //genera los datos copiando del dataset
  int minD =0;
  int maxD = neurons(_,0).size()-1;
  int indexDato ;
  for (int i = 0; i < neuronsSize; i++) {
    indexDato = minD + ((double) rand() / (RAND_MAX)) * (maxD - minD);
    neurons(i,_) = data(indexDato,_);

  }
  //neurons listas para mover

  float learningRate = initialLearningRate;
  float learningRateStep = (initialLearningRate - finalLearningRate) / iterations;

  float radius = initialRadius;
  float radiusStep = (initialRadius - finalRadius) / iterations;


  //desordena los datos
  data = disorder(data);
  //inicializa la epoca
  int index = 0;
  int dataLength = data(_,0).size();

  ///////////////////////////////
  ///////////////////////////////START TRAINING
  ///////////////////////////////

  for(int i = 0 ; i <iterations ; i++){
    //inicia nueva epoca
    if (index == dataLength){
      data = disorder(data);
      index = 0;
    }

    //busca el BMU
    int bestNeuron = FindBMU_tree( data(index,_), neurons,numberOfChildrenperNode, treeHeight);
    //actualiza la red neuronal
    neurons = updateStructure( neurons,  data(index,_),round(radius),  learningRate, bestNeuron, numberOfChildrenperNode);

    radius -= radiusStep;
    learningRate -= learningRateStep;
    index+=1;
  }


  ///////////////////////////////
  ///////////////////////////////END TRAINING
  ///////////////////////////////


  //genera el dataFrame para retornar
  Rcpp::List tmp(columnLength);
  Rcpp::CharacterVector lnames = Names.size() < lst.size() ?
  lst.attr("names") : Names;
  Rcpp::CharacterVector names(columnLength);
  Rcpp::List listaTempo(neuronsSize);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    dataSize = y.size();
    Rcpp::NumericVector xx = neurons(_,i);
    listaTempo = Rcpp::List::create(Rcpp::Named("vec") = xx);
    tmp[i ] = listaTempo;
    if (std::string(lnames[i]).compare("") != 0) {
      names[i] = lnames[i];
    } else {

      std::string Result;          // string which will contain the result

      std::ostringstream convert;   // stream used for the conversion

      convert << i;      // insert the textual representation of 'Number' in the characters in the stream

      Result = convert.str();
      names[i] = "V" + Result;
    }
  }
  Rcpp::DataFrame result(tmp);
  result.attr("names") = names;

  return result;
}


//metodos para topologia de grafo
//busca el BMU entre todas las neuronas
// [[Rcpp::export]]
int findBMU(DataFrame dataNeuron,DataFrame dataStimulus){
  Rcpp::NumericMatrix neurons = internal::convert_using_rfunction(dataNeuron, "as.matrix");
  NumericMatrix stimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
  NumericVector stimulusVector = stimulusMatrix(0,_);
  //Datos listos para trabajar en matriz
  NumericVector range = distance(neurons, stimulusVector);
  NumericVector::iterator it = std::min_element(range.begin(), range.end());

  return it - range.begin() + 1;
}


// [[Rcpp::export]]
void set_seed(int seed){
    srand(seed);
  }



// [[Rcpp::export]]
NumericVector findBmuAndDistance(DataFrame dataNeuron,DataFrame dataStimulus,int numberOfChildrenperNode, int treeHeight){
  NumericVector result(2);

  NumericMatrix dataNeuronMatrix = internal::convert_using_rfunction(dataNeuron, "as.matrix");

  NumericMatrix dataStimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
  NumericVector stimulus = dataStimulusMatrix(0,_);
  //find BMU
  result[0] = FindBMU_tree( stimulus,dataNeuronMatrix, numberOfChildrenperNode, treeHeight);

  //Distance to BMU
  result[1] = calculateEuclideanDistance2Point (dataNeuronMatrix(result[0],_),stimulus);
  return result;
}


/*
 // [[Rcpp::export]]
 int FindBMU_tree(DataFrame dataNeuron,DataFrame dataStimulus,int numberOfChildrenperNode, int treeHeight){
 Rcpp::NumericMatrix neurons = internal::convert_using_rfunction(dataNeuron, "as.matrix");
 NumericMatrix stimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
 NumericVector stimulus = stimulusMatrix(0,_);

 int bmu = FindBMU_tree( stimulus, neurons, numberOfChildrenperNode,  treeHeight);
 return bmu;
 }



 // [[Rcpp::export]]
 float calculateEuclideanDistance(DataFrame point1data,DataFrame point2data ){
 NumericMatrix point1Matrix = internal::convert_using_rfunction(point1data, "as.matrix");
 NumericVector point1 = point1Matrix(0,_);

 NumericMatrix point2Matrix = internal::convert_using_rfunction(point2data, "as.matrix");
 NumericVector point2 = point2Matrix(0,_);

 return calculateEuclideanDistance2Point ( point1, point2 );
 }
*/
///////////////////////////////////////////SOM


//actualiza el matriz de SOM
NumericMatrix updateStructureSOM(NumericMatrix neurons, NumericVector stimulus,
                                 float radius, float learningRate, int BMU, int numberColumn,int numberRow, NumericVector vecinoVicitado ){


    //mueve el BMU
  if(vecinoVicitado[BMU] == 0){
    neurons(BMU,_) = updateNeuron(neurons(BMU,_),stimulus,learningRate);
    vecinoVicitado[BMU] = 1;
  }

    radius = radius - 1;
    //disminuye la tasa de aprendizaje, ya que en cada nivel que sube el entrenamiento
    //se encuentra mas lejos del BMU y mas cerca de la raiz
    learningRate = learningRate * 0.9;

    //mueve vecinos
    if (radius >= 1) {
      if(((BMU+1)  % numberColumn != 0) && (BMU+1) < (numberColumn * numberRow)){
        neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU+1,numberColumn,numberRow,vecinoVicitado);
      }

      if(((BMU+1) -1)  % numberColumn != 0 &&  BMU > 1){
        neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU-1,numberColumn,numberRow,vecinoVicitado);
      }

      if((BMU) - numberColumn >= 0){
        neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU - numberColumn ,numberColumn,numberRow,vecinoVicitado);
      }

      if((BMU) + numberColumn < (numberColumn * numberRow)){
        neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU  + numberColumn ,numberColumn,numberRow,vecinoVicitado);
      }


  }
  return neurons;
}

int findBmuSom(NumericVector stimulusVector,NumericMatrix neurons){

  NumericVector range = distance(neurons, stimulusVector);
  NumericVector::iterator it = std::min_element(range.begin(), range.end());

  return it - range.begin();
}

// [[Rcpp::export]]
Rcpp::DataFrame trainSOM_Rcpp(int numberColumn,int numberRow,float initialLearningRate ,float finalLearningRate,
                           int initialRadius,int  finalRadius, int iterations
                             , Rcpp::List lst,
                             Rcpp::CharacterVector Names = Rcpp::CharacterVector::create()) {

  int columnLength = lst.size();
  int neuronsSize = numberColumn * numberRow;
  SEXP ll = lst[0];
  Rcpp::NumericVector y(ll);
  int dataSize = y.size();

  Rcpp::NumericMatrix data(dataSize,columnLength);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    data( _ ,i) =y;
  }
  //Datos listos para trabajar en matriz

  Rcpp::NumericMatrix neurons(neuronsSize,columnLength);

  //genera los datos copiando del dataset
  int minD =0;
  int maxD = neurons(_,0).size()-1;
  int indexDato ;
  for (int i = 0; i < neuronsSize; i++) {
    indexDato = minD + ((double) rand() / (RAND_MAX)) * (maxD - minD);
    neurons(i,_) = data(indexDato,_);

  }
  //neurons listas para mover
  float learningRate = initialLearningRate;

  float learningRateStep = (initialLearningRate - finalLearningRate) / iterations;

  float radius = initialRadius;

  float radiusStep = (initialRadius - finalRadius) / iterations;


  //desordena los datos
  data = disorder(data);
  //inicializa la epoca
  int index = 0;
  int dataLength = data(_,0).size();

  ///////////////////////////////
  ///////////////////////////////START TRAINING
  ///////////////////////////////

  for(int i = 0 ; i <iterations ; i++){
    //inicia nueva epoca
    if (index == dataLength){
      data = disorder(data);
      index = 0;
    }

    //busca el BMU
    int bestNeuron = findBmuSom( data(index,_), neurons);
    //actualiza la red neuronal
    NumericVector vecinoVicitado(neuronsSize);
    neurons = updateStructureSOM( neurons,  data(index,_),round(radius),  learningRate, bestNeuron, numberColumn, numberRow,  vecinoVicitado);

    radius -= radiusStep;
    learningRate -= learningRateStep;
    index+=1;
  }


  ///////////////////////////////
  ///////////////////////////////END TRAINING
  ///////////////////////////////


  //genera el dataFrame para retornar
  Rcpp::List tmp(columnLength);
  Rcpp::CharacterVector lnames = Names.size() < lst.size() ?
  lst.attr("names") : Names;
  Rcpp::CharacterVector names(columnLength);
  Rcpp::List listaTempo(neuronsSize);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    dataSize = y.size();
    Rcpp::NumericVector xx = neurons(_,i);
    listaTempo = Rcpp::List::create(Rcpp::Named("vec") = xx);
    tmp[i ] = listaTempo;
    if (std::string(lnames[i]).compare("") != 0) {
      names[i] = lnames[i];
    } else {

      std::string Result;          // string which will contain the result

      std::ostringstream convert;   // stream used for the conversion

      convert << i;      // insert the textual representation of 'Number' in the characters in the stream

      Result = convert.str();
      names[i] = "V" + Result;
    }
  }
  Rcpp::DataFrame result(tmp);
  result.attr("names") = names;

  return result;
}

