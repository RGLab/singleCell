#include <Rcpp.h>
#include <tiledb/query.h>
#include <tiledb/array.h>
#include <string>
using namespace Rcpp;
// [[Rcpp::export]]
void create_tiledb(std::string dbdir, std::string attr, std::vector<unsigned> row_domain, std::vector<unsigned> col_domain) {
  // Create TileDB context
  tiledb::Context ctx;
  std::array<unsigned, 2> row = {{row_domain[0], row_domain[1]}};
  std::array<unsigned, 2> col = {{col_domain[0], col_domain[1]}};
  
  
  // Create dimensions
  auto d1 = tiledb::Dimension::create<unsigned>(ctx, "row", row, 1);//tile by col
  auto d2 = tiledb::Dimension::create<unsigned>(ctx, "col", col, col_domain[1]);
  

  // Create domain
  tiledb::Domain domain(ctx);
  domain.add_dimension(d1).add_dimension(d2);
  
  // Create attributes
  tiledb::Attribute a1 = tiledb::Attribute::create<int>(ctx, attr);
   a1.set_compressor({TILEDB_LZ4, -1});
  
  // Create array schema
  tiledb::ArraySchema schema(ctx, TILEDB_DENSE);
  schema.set_order({{TILEDB_COL_MAJOR, TILEDB_COL_MAJOR}});
  //  schema.set_capacity(2);
  schema.set_domain(domain);
  schema.add_attributes(a1);
  
  // Check array schema
  //  try {
  schema.check();
  //  } catch (tiledb::TileDBError& e) {
  //    std::cout << e.what() << "\n";
  //    return -1;
  //  }
  
  // Create array
  tiledb::Array::create(dbdir, schema);
  
  // Nothing to clean up - all C++ objects are deleted when exiting scope
  
  //  return 0;
}

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
// [[Rcpp::export]]
IntegerMatrix region_selection_tiledb(std::string dbdir,  std::string attr, std::vector<unsigned> ridx, std::vector<unsigned> cidx) {
  // Create TileDB Context
  tiledb::Context ctx;

  //check ridx,cidx and buf size
  // Create query
  tiledb::Query query(ctx, dbdir, TILEDB_READ);
  query.set_layout(TILEDB_GLOBAL_ORDER);
  
  const std::vector<unsigned> subarray = {(unsigned)ridx[0], (unsigned)ridx[1], (unsigned)cidx[0], (unsigned)cidx[1]};
  query.set_subarray(subarray);
  // Rcout <<  ridx[1] << std::endl;
  auto max_sizes = tiledb::Array::max_buffer_elements(ctx, dbdir, subarray);
  // Rcout << max_sizes[TILEDB_COORDS].second << std::endl;

  unsigned nrow = ridx[1] - ridx[0] + 1;
  unsigned ncol = cidx[1] - cidx[0] + 1;
  // Rcout << nrow << " " << ncol << std::endl;
  IntegerMatrix mat(nrow, ncol);
  unsigned size = nrow * ncol;
  
  int * buf = &mat[0];
  query.set_buffer(attr, buf, size);

  query.submit();
  query.finalize();
  
  
   return mat;
}
