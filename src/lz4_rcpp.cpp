#include <Rcpp.h>
#include <cstring>
#include <string>
#include "lz4.h"
using namespace Rcpp;
using namespace std;
// [[Rcpp::plugins(cpp11)]]
void _lzcompress(char * dest, const char * src, int nsrc, int nMaxSize, int &nCompressed, int acceleration, bool verbose = false)
{
  int offset = 2 * sizeof(int);//reserve places for nSrc
  
  if(verbose)
    Rcout << "acceleration: " << acceleration << endl;
  nCompressed = LZ4_compress_fast(src, dest + offset, nsrc, nMaxSize, acceleration);//TODO::check if nMaxSize need to minus offset
  //add bytes for nSrc and nDest
  memcpy(dest, &nsrc, sizeof(int));
  memcpy(dest + sizeof(int), &nCompressed, sizeof(int));
 
  
}

// [[Rcpp::export]]
Rcpp::RawVector lzcompress(RawVector vec, int acceleration = 1, bool verbose = false)   
{
  
    
    const char * src = (const char*)&vec[0];
    int nUncompressed = vec.size(); //* sizeof(int);
    
    if(verbose)
      Rcout << "nUncompressed: " << nUncompressed << endl;
    
    int offset = 2 * sizeof(int);//reserve places for nSrc
    
    int nMaxSize = nUncompressed + offset;
    
    char dest[nMaxSize];
    
    int ndest, nCompressed;
    _lzcompress(dest, src, nUncompressed,nMaxSize, nCompressed, acceleration, verbose);
   
    if(verbose)
      Rcout << "nCompressed: " << nCompressed << endl;
   
    
    //return a copy of subset of actual compresssed length
    ndest = nCompressed + offset;
    
    RawVector output(ndest);
    
    memcpy((char*)&output[0], dest, ndest);
    return output;
    
    
  
}

// [[Rcpp::export]]
RawVector lzdecompress(RawVector input, bool verbose = false) {
  const char * src = (const char*)&input[0];
  int nsrc = input.size();
  
  if(verbose)
    Rcout << "input size: " << nsrc << endl;
  
  int offset = sizeof(int) * 2;
  //get nUncompressed
  int nUncompressed, nCompressed;
  memcpy((char *)&nUncompressed, src, sizeof(int));
  memcpy((char *)&nCompressed, src + sizeof(int), sizeof(int));
  if(verbose)
    Rcout << "nUncompressed:" << nUncompressed << " nCompressed:" << nCompressed << endl;
  //validity check
  if(nsrc != (offset + nCompressed)){
    
    string err = "the size of the compressed blobs is not the same as what recorded in header!" + to_string(nsrc) + ":" + to_string(nCompressed);
    stop(err);
  }
    
  //decompress directly from the byte stream of db into vec
  RawVector output(nUncompressed);
  int nDest = LZ4_decompress_safe (src + offset, (char *)(&output[0]), nCompressed, nUncompressed);
  if(nDest != nUncompressed){
    
    string err = "the size of the uncompressed blobs is not the same as what recorded in header!" + to_string(nDest) + ":" + to_string(nUncompressed);
    stop(err);
  }
    
    
  
 
  return output;
  
}


