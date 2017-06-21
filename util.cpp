#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
unordered_set<int> s;
//[[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void builhash(Rcpp::IntegerVector x) {
  s.clear();
  for(auto i : x)
    s.insert(i);
  // return wrap(&s);
}

// [[Rcpp::export]]
bool inHash(int i)
{
  return !(s.find(i)==s.end());
}

// [[Rcpp::export]]
void setVec_i(Rcpp::IntegerVector v, int start, int len,  Rcpp::IntegerVector vals)
{
  int * dest = &v[start-1];
  int * src = & vals[0];
  memcpy(dest, src, len * sizeof(int));
}

// [[Rcpp::export]]
void setVec_d(Rcpp::NumericVector v, int start, int len,  Rcpp::NumericVector vals)
{
  double * dest = &v[start-1];
  double * src = & vals[0];
  memcpy(dest, src, len * sizeof(double));
}
