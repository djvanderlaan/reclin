#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector equivalence_rcpp(IntegerVector a, IntegerVector b, int n) {
  if (a.length() != b.length()) 
    throw std::runtime_error("Lengths of a and be do not match.");
  IntegerVector classes(n);
  //Initialize each element its own class.
  int k = 0;
  for (IntegerVector::iterator p = classes.begin(); 
    p != classes.end(); ++p, ++k) (*p) = k;
  //For each piece of input information...
  int m = a.length();
  for (int l = 0; l < m; l++) {
    //Track first element up to its ancestor.
    int j = a[l];
    while (classes[j] != j) j = classes[j];
    //Track second element up to its ancestor.
    int k = b[l];
    while (classes[k] != k) k = classes[k];
    //If they are not already related, make them so.
    if (j != k) {
      classes[j] = k;
    }
  }
  //Final sweep up to highest ancestors.
  for (int j = 0; j < classes.length(); j++) {
    while (classes[j] != classes[classes[j]]) classes[j] = classes[classes[j]];
  }
  return classes;
}

