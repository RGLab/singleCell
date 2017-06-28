#include <Rcpp.h>
#include <cstring>
#include "lmdb.h"
#include "lz4.h"
using namespace Rcpp;
using namespace std;
#define E(expr) CHECK((rc = (expr)) == MDB_SUCCESS, #expr)
#define RES(err, expr) ((rc = expr) == (err) || (CHECK(!rc, #expr), 0))
#define CHECK(test, msg) ((test) ? (void)0 : ((void)fprintf(stderr, \
"%s:%d: %s: %s\n", __FILE__, __LINE__, msg, mdb_strerror(rc)), abort()))

// [[Rcpp::export]]
List lmdb_open(std::string dbfile)
{
  int rc;
  MDB_env *env;
  MDB_txn *txn;
  E(mdb_env_create(&env));
  // E(mdb_env_set_maxreaders(env, 1));
  E(mdb_env_set_mapsize(env, 4096*1e7));//max 38G
  // 
  rc = mdb_env_open(env, dbfile.c_str(), 0, 0664);
  if(rc)
    stop("Can't create open db file! ", mdb_strerror(rc));
  
  // rc = mdb_txn_begin(env, NULL, 0, &txn);
  // if(rc)
  //   stop("Can't begin transaction!", mdb_strerror(rc));
  // 
  // MDB_dbi dbi;
  // rc = mdb_dbi_open(txn, NULL, 0, &dbi);
  if(rc){
    
    // mdb_txn_abort(txn);
    mdb_env_close(env);
    
    stop("Can't create open the default database! ", mdb_strerror(rc));
  }
  // mdb_txn_abort(txn);  
  
  return List::create(Named("env", XPtr<MDB_env>(env, false))//we don't own the pointer so disable the default gc from R
                     // , Named("dbi", dbi)
                      // , Named("txn", XPtr<MDB_txn>(txn, false))
                      );
}

// [[Rcpp::export]]
void lmdb_close(List db){
  
  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  // XPtr<MDB_txn > _txn(as<XPtr<MDB_txn> >(db["txn"]));
  
  MDB_env * env = _env.get();
  // MDB_txn * txn = _txn.get();
  // MDB_dbi dbi(as<MDB_dbi>(db["dbi"]));
  
  //TODO: deal with the situation when txn has not been commited or abort
  // mdb_txn_abort(txn);
  // txn = NULL;
  // mdb_dbi_close(env, dbi);
  
  mdb_env_close(env);
  env = NULL;
}

// [[Rcpp::export]]
void mdb_insert_cols(List db, IntegerVector cidx, Rcpp::List vecs)   
{
  int rc;

  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  // XPtr<MDB_txn > _txn(as<XPtr<MDB_txn> >(db["txn"]));
  MDB_env * env = _env.get();
  // MDB_dbi dbi(as<MDB_dbi>(db["dbi"]));
  // MDB_txn * txn = _txn.get();
  if(!env)
    stop("Not valid db connection!");
  MDB_txn * txn;
  E(mdb_txn_begin(env, NULL, 0, &txn));
  
  MDB_dbi dbi;
  E(mdb_dbi_open(txn, NULL, 0, &dbi));
  
  
  MDB_val key, data;
  key.mv_size = sizeof(int);
  /* Note: Most error checking omitted for simplicity */
  for(int i = 0; i < cidx.size(); i++)
  {
    
    //init key and data obj
    key.mv_data = &cidx[i];
    NumericVector val = vecs[i];
    const char * src = (const char*)&val[0];
    
    //compress
    int nUncompressed = val.size() * sizeof(double);
    int offset = 2 * sizeof(int);//reserve places for nSrc
    int nMaxSize = nUncompressed + offset;
    char dest[nMaxSize];
    int nCompressed = LZ4_compress_default(src, dest + offset, nUncompressed, nMaxSize);
    //add bytes for nSrc and nDest
    memcpy(dest, &nUncompressed, sizeof(int));
    memcpy(dest + sizeof(int), &nCompressed, sizeof(int));
    
    //write to db
    data.mv_size = nCompressed + offset;
    data.mv_data = dest;
    E(mdb_put(txn, dbi, &key, &data, 0));  
  }
  
  
  E(mdb_txn_commit(txn));
  mdb_dbi_close(env, dbi);
  
  
}

// [[Rcpp::export]]
List mdb_get_cols(List db, IntegerVector cidx) {
  int rc;
  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  // XPtr<MDB_txn > _txn(as<XPtr<MDB_txn> >(db["txn"]));
  MDB_env * env = _env.get();
  // MDB_dbi dbi(as<MDB_dbi>(db["dbi"]));
  // MDB_txn * txn = _txn.get();
  
  MDB_val key, data;
 
  MDB_cursor *cursor;
  
  //init key and data obj
  key.mv_size = sizeof(int);
  MDB_txn * txn;
  E(mdb_txn_begin(env, NULL, MDB_RDONLY, &txn));
  MDB_dbi dbi;
  E(mdb_cursor_open(txn, dbi, &cursor));
  
  char * buffer;
  int ncol = cidx.size();
  List res(ncol);
  int offset = sizeof(int) * 2;
  for(int i = 0; i < ncol; i++)
  {
    key.mv_data = &cidx[i];
    E(mdb_cursor_get(cursor, &key, &data, MDB_SET));
  
    //get nUncompressed
    int nUncompressed, nCompressed;
    memcpy((char *)&nUncompressed, data.mv_data, sizeof(int));
    memcpy((char *)&nCompressed, (char *)data.mv_data + sizeof(int), sizeof(int));
    
    //validity check
    if(data.mv_size != (offset + nCompressed)){
      
      mdb_cursor_close(cursor);
      mdb_txn_abort(txn);
      string err = "the size of the compressed blobs stored in mdb is not the same as what recorded in header!" + to_string(data.mv_size - offset) + ":" + to_string(nCompressed);
      stop(err);
    }
      
    //decompress directly from the byte stream of db into vec
    NumericVector vec(nUncompressed/sizeof(double));
    
    buffer = new char[nUncompressed];
    int nDest = LZ4_decompress_safe ((const char*)data.mv_data + offset, (char *)(&vec[0]), nCompressed, nUncompressed);
    if(nDest != nUncompressed){
      
      mdb_cursor_close(cursor);
      mdb_txn_abort(txn);
      string err = "the size of the uncompressed blobs stored in mdb is not the same as what recorded in header!" + to_string(nDest) + ":" + to_string(nUncompressed);
      stop(err);
    }
    
    res[i] = vec;
  }
  
  mdb_cursor_close(cursor);
  mdb_txn_abort(txn);
  mdb_dbi_close(env, dbi);
  
  return res;
  
}


