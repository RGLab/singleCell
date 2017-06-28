#include <Rcpp.h>
#include <cstring>
#include "lmdb.h"
#include "lz4.h"
using namespace Rcpp;
using namespace std;
// #define E(expr) CHECK((rc = (expr)) == MDB_SUCCESS, #expr)
// #define RES(err, expr) ((rc = expr) == (err) || (CHECK(!rc, #expr), 0))
// #define CHECK(test, msg) ((test) ? (void)0 : ((void)fprintf(stderr, \
// "%s:%d: %s: %s\n", __FILE__, __LINE__, msg, mdb_strerror(rc)), stop()))
void check_rc(int rc)
{
  if(rc != MDB_SUCCESS)
  {
    stop(mdb_strerror(rc));
  }
}
// [[Rcpp::export]]
List lmdb_open(std::string dbfile)
{
  int rc;
  MDB_env *env;
  MDB_txn *txn;
  rc = mdb_env_create(&env);
  check_rc(rc);
  // E(mdb_env_set_maxreaders(env, 1));
  rc = mdb_env_set_mapsize(env, 4096*1e7);//max 38G
  check_rc(rc);
  
  rc = mdb_env_open(env, dbfile.c_str(), 0, 0664);
  
  if(rc){
    
    mdb_env_close(env);
    
    check_rc(rc);
  }
  
  
  return List::create(Named("env", XPtr<MDB_env>(env, false))//we don't own the pointer so disable the default gc from R
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
  rc = mdb_txn_begin(env, NULL, 0, &txn);
  check_rc(rc);
  
  MDB_dbi dbi;
  rc = mdb_dbi_open(txn, NULL, MDB_INTEGERKEY, &dbi);
  check_rc(rc);
  
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
    rc = mdb_put(txn, dbi, &key, &data, MDB_APPEND);  //MDB_APPEND flag allows faster write assuming key is in correct order
    check_rc(rc);
  }
  
  
  rc = mdb_txn_commit(txn);
  check_rc(rc);
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
  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  check_rc(rc);
  
  MDB_dbi dbi;
  rc = mdb_dbi_open(txn, NULL, 0, &dbi);
  check_rc(rc);
  
  rc = mdb_cursor_open(txn, dbi, &cursor);
  check_rc(rc);
  
  char * buffer;
  int ncol = cidx.size();
  List res(ncol);
  int offset = sizeof(int) * 2;
  for(int i = 0; i < ncol; i++)
  {
    key.mv_data = &cidx[i];
    rc = mdb_cursor_get(cursor, &key, &data, MDB_SET);
    check_rc(rc);
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


