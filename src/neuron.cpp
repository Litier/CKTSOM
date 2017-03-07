#include <Rcpp.h>
#include <string>
using namespace Rcpp;


//genera vector de largo en de numeros random entre [min,max]
Rcpp::NumericVector runifC(int n, double min=0, double max=1) {
  Rcpp::NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = min + ((double) rand() / (RAND_MAX)) * (max - min);
  }
  return out;
}

//determina cuantas neuronas hay en el arbol
int totalNeuronas(int nHijos, int altura){
  int total = 0 ;
  for (int i = 0 ; i <= altura ; i++ ){
    total = total + pow(nHijos,i);
  }
  return total;
}

//genera un vector de los hijos de una neurona
Rcpp::NumericVector hijos(int numero,int  k){
  Rcpp::NumericVector a(k);
  numero++;  //aumento el numero para tener la lista que empiesa 1
  int n = 1;
  for (int i = 1; i <=k ;i++){
    a[k -i] = k*numero+n -1;  //disminulle el numero para una lista que empiesa en 0
    n = n -1;
  }
  return a;
}

//obtiene el padre de una neurona
int buscapadre(int numero,int k){
  numero++;  //aumento el numero para tener la lista que empiesa 1
  int padre = (numero+k-2)/k;
  return padre-1;  //disminulle el numero para una lista que empiesa en 0
}


Rcpp::NumericVector buscarHermanos(int numero ,int k){
  int padre = buscapadre(numero,k);
  Rcpp::NumericVector hermanos = hijos(padre,k);
  return hermanos;
}


Rcpp::NumericVector mueveNeurona(Rcpp::NumericVector neurona, Rcpp::NumericVector estimulo,
                                 float alfa){
  for (int i =0 ; i <neurona.size(); i++){
    neurona[i] = neurona[i] - alfa* (neurona[i] - estimulo[i]);
  }
  return neurona;
}




NumericMatrix mueveEstructura(NumericMatrix neuronas, NumericVector estimulo,
                              float radio, float alfa, int neuronaEstimulada, int k){
  //mover
//  Rcpp::Rcout << "neuronaEstimulada is " << neuronaEstimulada << std::endl;


  //NumericVector eliminar = neuronas(neuronaEstimulada,_);
  //  Rcpp::Rcout << "neuronas(_,neuronaEstimulada) is " << eliminar << std::endl;
  //  Rcpp::Rcout << "estimulo is " << estimulo << std::endl;
  //Rcpp::Rcout << "alfa is " << alfa << std::endl;

  //  Rcpp::Rcout << "----->neuronaEstimulada is " << neuronaEstimulada << std::endl;

  neuronas(neuronaEstimulada,_) =mueveNeurona(neuronas(neuronaEstimulada,_),estimulo,alfa);
  //modificar neuronas

  int padre = buscapadre(neuronaEstimulada,k);
  radio = radio - 1;
  alfa =alfa * 0.9;
  //  Rcpp::Rcout << "padre is " << padre << std::endl;
  //mueve padre
  if (padre >= 0 && radio > 1){
    neuronas = mueveEstructura(neuronas, estimulo, radio,  alfa,  padre,  k);


  }

  //mueve hermano
  if(neuronaEstimulada > 0 && radio > 1){
    Rcpp::NumericVector hermanos = buscarHermanos(neuronaEstimulada,k);
    for (int i = 0; i < hermanos.size();i++){
      int tempHer = hermanos[i];
      if (tempHer!= neuronaEstimulada){
        neuronas(tempHer,_) =mueveNeurona(neuronas(tempHer,_),estimulo,alfa*0.2);
      }
    }
  }


  return neuronas;
}



NumericVector buscarHojas(int k,int h){
  int total = pow(k,h);
  NumericVector hojas(total);
  int largo = totalNeuronas(k,h);
  int primeraHoja = largo-total+1;
  for(int i = 0; i < total ; i++){
    hojas(i) = primeraHoja + i-1;
  }
  return hojas;
}


float calculaDistancia (NumericVector punto1,NumericVector punto2 ){
  NumericVector resta(punto1.size());
  punto1[is_na(punto1)] = 0;
  punto2[is_na(punto2)] = 0;

  resta = punto1 - punto2;
  resta = resta*resta;
  float disc = sum(resta);
  float d = sqrt(disc);
  return d;
}





NumericVector distancia_arbol (NumericMatrix neuronas,NumericVector puntofijo, int k, int h){
  NumericVector indexHojas = buscarHojas(k,h);
  NumericVector distancias(indexHojas.size());




  for (int i = 0; i <indexHojas.size();i++ ){
    distancias(i) = calculaDistancia(neuronas(indexHojas(i),_),puntofijo);
  }
  //Rcpp::Rcout << "distancias is " << distancias << std::endl;

  return distancias;
}


int FindBMU_arbol(NumericMatrix datos,NumericMatrix neuronas,int indexEstimulo,int k, int h){
  NumericVector dist = distancia_arbol(neuronas, datos(indexEstimulo,_),k,h);

  NumericVector::iterator it = std::min_element(dist.begin(), dist.end());
  // we want the value so dereference
  return it - dist.begin();
}





// [[Rcpp::export]]
Rcpp::DataFrame myFunc(int k,int h,float alfaMAyor ,float AlfaMenor,
                       int Rinicial,int  Rfinal, int nIteraciones
                         , Rcpp::List lst,
                         Rcpp::CharacterVector Names = Rcpp::CharacterVector::create()) {

  int n = lst.size();
  int largoDatos = 0;
  int nNeuronas = totalNeuronas(k,h);
  SEXP ll = lst[0];
  Rcpp::NumericVector y(ll);
  largoDatos = y.size();


  Rcpp::NumericMatrix datos(largoDatos,n);
  for (int i = 0; i < n; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    datos( _ ,i) =y;
  }
  //Datos listos para trabajar en matriz

  //genera los datos de forma aleatoria
  Rcpp::NumericMatrix neuronas(nNeuronas,n);
  for (int i = 0; i < n; i++) {
    neuronas(_,i)= runifC(nNeuronas,min(datos(_,i)) ,max(datos(_,i)));

  }
  //genera los datos copiando del dataset
  //Rcpp::NumericMatrix neuronas(nNeuronas,n);
  int minD =0;
  int maxD = neuronas(_,0).size()-1;
  int indexDato ;
  for (int i = 0; i < nNeuronas; i++) {
    indexDato = minD + ((double) rand() / (RAND_MAX)) * (maxD - minD);
    neuronas(i,_) = datos(indexDato,_);

  }
  //neuronas listas para mover


  //configuracion del radio
  float alfa = alfaMAyor;
  float restoAlfa = (alfaMAyor - AlfaMenor) / nIteraciones;
  //configuracion de alfa

  float radio = Rinicial;
  float restoRadio = (Rinicial - Rfinal) / nIteraciones;

  for(int i = 0 ; i <nIteraciones ; i++){
    Rcpp::NumericVector nRandomr = runifC(1,0,(largoDatos-1));

    int mejorNeurona = FindBMU_arbol( datos, neuronas,nRandomr(0),k, h);
    //Rcpp::Rcout << "mejorNeurona111 is " << mejorNeurona << std::endl;

    mejorNeurona = neuronas(_,0).size() - ( pow(k,h) -mejorNeurona);
    //Rcpp::Rcout << "mejorNeurona22222222222222222 is " << mejorNeurona << std::endl;
    ///
    //Rcpp::Rcout << "mejorNeurona1 is " << mejorNeurona << std::endl;
    // Rcpp::Rcout << "nRandomr[0] is " <<  nRandomr[0]<< std::endl;
    ///

    int random = (int)nRandomr[0];
    //
    // Rcpp::Rcout << "random is " <<  random<< std::endl;
    //  Rcpp::Rcout << "neuronas is " <<  neuronas<< std::endl;
    //  NumericVector n = datos(random,_);
    //  Rcpp::Rcout << "datos(_,random) is " <<  n<< std::endl;
    // Rcpp::Rcout << "round(radio) is " <<  round(radio)<< std::endl;
    ///
    //
    neuronas = mueveEstructura( neuronas,  datos(random,_),round(radio),  alfa, mejorNeurona, k);
    radio -= restoRadio;
    alfa-=restoAlfa;
  }





  //genera el dataFrame para retornar
  Rcpp::List tmp(n);
  Rcpp::CharacterVector lnames = Names.size() < lst.size() ?
  lst.attr("names") : Names;
  Rcpp::CharacterVector names(n);
  Rcpp::List listaTempo(nNeuronas);
  for (int i = 0; i < n; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    largoDatos = y.size();
    ////////////////generar la lista
    //Rcpp::NumericVector xx(k*h, 2.0);

    Rcpp::NumericVector xx = neuronas(_,i);
    listaTempo = Rcpp::List::create(Rcpp::Named("vec") = xx);

    ///////////////
    // tmp[i + 2] = do_something(lst[i]);
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





  //return result;


  return result;
}

NumericVector distancia (NumericMatrix neuronas,NumericVector puntofijo){

  NumericVector distancias(neuronas(_,1).size());
  for (int i = 0; i <neuronas(_,1).size();i++ ){
    distancias(i) = calculaDistancia(neuronas(i,_),puntofijo);
  }
  //Rcpp::Rcout << "distancias is " << distancias << std::endl;

  return distancias;
}

// [[Rcpp::export]]
int FindBMU(Rcpp::List listNeuronas,Rcpp::NumericVector dato){
  int n = listNeuronas.size();
  int largoDatos = 0;
  SEXP ll = listNeuronas[0];
  Rcpp::NumericVector y(ll);
  largoDatos = y.size();


  Rcpp::NumericMatrix neuronas(largoDatos,n);
  for (int i = 0; i < n; i++) {
    SEXP ll = listNeuronas[i];
    Rcpp::NumericVector y(ll);
    neuronas( _ ,i) =y;
  }
  //Datos listos para trabajar en matriz

  NumericVector dist = distancia(neuronas, dato);

  NumericVector::iterator it = std::min_element(dist.begin(), dist.end());
  // we want the value so dereference
  return it - dist.begin() + 1;
}
