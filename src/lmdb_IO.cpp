#include <Rcpp.h>
#include <lmdb.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List lmdb_open(std::string dbfile)
{
  int rc;
  MDB_env *env;
  MDB_dbi *dbi;
  MDB_txn *txn;
  rc = mdb_env_create(&env);
  if(rc)
    stop("Can't create mdb env!Error code: ", rc);
  
  rc = mdb_env_open(env, dbfile.c_str(), 0, 0666);
  if(rc)
    stop("Can't create open db file!Error code: ", rc);
  
  rc = mdb_txn_begin(env, NULL, 0, &txn);
  if(rc)
    stop("Can't begin transaction!Error code: ", rc);
  rc = mdb_dbi_open(txn, NULL, 0, dbi);
  if(rc)
    stop("Can't create open the default database!Error code: ", rc);
  
  return List::create(Named("env", XPtr<MDB_env>(env))
                     , Named("dbi", XPtr<MDB_dbi>(dbi))
                      , Named("txn", XPtr<MDB_txn>(txn))
                      );
}

// [[Rcpp::export]]
void lmdb_close(List db){
  
  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  XPtr<MDB_dbi> _dbi(as<XPtr<MDB_dbi> >(db["dbi"]));
  MDB_env * env = _env.get();
  MDB_dbi * dbi = _dbi.get();
  mdb_dbi_close(env, *dbi);
  mdb_env_close(env);
}

// [[Rcpp::export]]
void mdb_insert_cols(List db, IntegerVector cidx, Rcpp::List vecs)   
{
  int rc;

  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  XPtr<MDB_dbi> _dbi(as<XPtr<MDB_dbi> >(db["dbi"]));
  XPtr<MDB_txn > _txn(as<XPtr<MDB_txn> >(db["txn"]));
  MDB_env * env = _env.get();
  MDB_dbi * dbi = _dbi.get();
  MDB_txn * txn = _txn.get();
  
  
  MDB_val key, data;
  key.mv_size = sizeof(int);
  /* Note: Most error checking omitted for simplicity */
  for(int i = 0; i < cidx.size(); i++)
  {
    
    //init key and data obj
    key.mv_data = &cidx[i];
    NumericVector val = vecs[i];
    data.mv_size = val.size() * sizeof(double);
    data.mv_data = &val[0];
    rc = mdb_put(txn, *dbi, &key, &data, 0);  
  }
  
  
  rc = mdb_txn_commit(txn);
  if (rc) {
    stop("mdb_txn_commit: ", mdb_strerror(rc));
    
  }
  
  
}

// [[Rcpp::export]]
List mdb_get_cols(List db, IntegerVector cidx) {
  int rc;
  XPtr<MDB_env> _env(as<XPtr<MDB_env> >(db["env"]));
  XPtr<MDB_dbi> _dbi(as<XPtr<MDB_dbi> >(db["dbi"]));
  XPtr<MDB_txn > _txn(as<XPtr<MDB_txn> >(db["txn"]));
  MDB_env * env = _env.get();
  MDB_dbi * dbi = _dbi.get();
  MDB_txn * txn = _txn.get();
  
  MDB_val key, data;
 
  MDB_cursor *cursor;
  
  //init key and data obj
  key.mv_size = sizeof(int);
  
  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  rc = mdb_cursor_open(txn, *dbi, &cursor);
  char * bytes;
  List res(cidx);
  for(int i = 0; i < cidx.size(); i++)
  {
    key.mv_data = &cidx[i];
    mdb_cursor_get(cursor, &key, &data, MDB_SET);
    //cp buffer
    int cnt = data.mv_size;
    bytes = new char[cnt];
    memcpy(bytes, data.mv_data, cnt);
    //decompress the byte stream
    //convert to double
    NumericVector vec(cnt/sizeof(double));
    memcpy((char *)&vec[0], bytes, cnt);
    res[i] = vec;
  }
  mdb_cursor_close(cursor);
  mdb_txn_abort(txn);
  
  return res;
  
}


