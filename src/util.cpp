#include <Rcpp.h>
#include <zlib.h>
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
//[Rcpp::plugins(cpp11)]
//[Rcpp::export]
Rcpp::RawVector memCompress1(Rcpp::NumericVector vals){
  
}
