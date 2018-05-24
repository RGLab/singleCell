#include <Rcpp.h>
#include <tiledb/query.h>
#include <tiledb/array.h>
#include <string>
using namespace Rcpp;
// 
// // [[Rcpp::export]]
// void tiledb_query_reset_buffers(XPtr<tiledb::Query> query) {
//   try {
//     query->reset_buffers();
//     
//   } catch (tiledb::TileDBError& err) {
//     throw Rcpp::exception(err.what()); 
//   }
// }

tiledb_query_type_t _string_to_tiledb_query_type(std::string qtstr) {
  if (qtstr == "READ") {
    return TILEDB_READ;
  } else if (qtstr == "WRITE") {
    return TILEDB_WRITE;
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB query type \"" << qtstr << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> tiledb_query(XPtr<tiledb::Context> ctx,
                                 std::string uri,
                                 std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  try {
    auto query = XPtr<tiledb::Query>(
      new tiledb::Query(tiledb::Query(*ctx.get(), uri, query_type)));
    // query->set_layout(TILEDB_COL_MAJOR);
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void tiledb_query_set_coordinates(XPtr<tiledb::Query> query, std::vector<unsigned> coords) {
  try {
    query->set_coordinates(coords);
    
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}

// [[Rcpp::export]]
void tiledb_query_finalize(XPtr<tiledb::Query> query) {
  try {
    query->finalize();
    
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}
// [[Rcpp::export]]
IntegerVector tiledb_dim(std::string dbdir)
{
  tiledb::Context ctx;
  tiledb::ArraySchema as(ctx, dbdir);
  
  auto dm = as.domain().dimensions();
  unsigned nDim = dm.size();
  IntegerVector res(nDim);
  for(unsigned i = 0; i < nDim; i++)
  {
    auto bound = dm[i].domain<unsigned>();
    res[i] = bound.second;
  }
  return res;
}
