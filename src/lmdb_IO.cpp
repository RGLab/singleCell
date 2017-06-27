#include <Rcpp.h>
#include <lmdb.h>
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
  // E(mdb_env_set_mapsize(env, 10485760));
  // 
  rc = mdb_env_open(env, dbfile.c_str(), 0, 0664);
  if(rc)
    stop("Can't create open db file! ", mdb_strerror(rc));
  
  rc = mdb_txn_begin(env, NULL, 0, &txn);
  if(rc)
    stop("Can't begin transaction!", mdb_strerror(rc));
  
  MDB_dbi *dbi = new MDB_dbi;
  rc = mdb_dbi_open(txn, NULL, 0, dbi);
  if(rc){
    delete dbi;
    stop("Can't create open the default database! ", mdb_strerror(rc));
  }
    
  
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
  delete dbi;
  dbi = NULL;
  mdb_env_close(env);
  env = NULL;
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
  if(!env||!dbi||!txn)
    stop("Not valid db connection!");
  
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
    E(mdb_put(txn, *dbi, &key, &data, 0));  
  }
  
  
  E(mdb_txn_commit(txn));
  
  
  
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
  
  E(mdb_txn_begin(env, NULL, MDB_RDONLY, &txn));
  E(mdb_cursor_open(txn, *dbi, &cursor));
  
  char * bytes;
  int ncol = cidx.size();
  List res(ncol);
  for(int i = 0; i < ncol; i++)
  {
    key.mv_data = &cidx[i];
    E(mdb_cursor_get(cursor, &key, &data, MDB_SET));
  
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


