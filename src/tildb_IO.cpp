#include <Rcpp.h>
#include <tiledb/tiledb>
#include <string>
using namespace Rcpp;
// [[Rcpp::export]]
void create_tiledb(std::string dbdir, std::string attr, std::vector<int> row_domain, std::vector<int> col_domain, std::vector<unsigned> tile_extend, bool isSparse = false) {
  // Create TileDB context
  tiledb::Context ctx;
  std::array<int, 2> row = {{row_domain[0], row_domain[1]}};
  std::array<int, 2> col = {{col_domain[0], col_domain[1]}};
  
  
  // Create dimensions
  auto d1 = tiledb::Dimension::create<int>(ctx, "row", row, tile_extend[0]);
  auto d2 = tiledb::Dimension::create<int>(ctx, "col", col, tile_extend[1]);
  

  // Create domain
  tiledb::Domain domain(ctx);
  domain.add_dimension(d1).add_dimension(d2);
  
  // Create attributes
  tiledb::Attribute a1 = tiledb::Attribute::create<int>(ctx, attr);
   a1.set_compressor({TILEDB_LZ4, -1});
  
  // Create array schema
  tiledb::ArraySchema schema(ctx, isSparse?TILEDB_SPARSE:TILEDB_DENSE);
  schema.set_order({{TILEDB_COL_MAJOR, TILEDB_COL_MAJOR}});
  //  schema.set_capacity(2);
  schema.set_domain(domain);
  schema.add_attributes(a1);
  
  // Check array schema
  //  try {
  schema.check();
  schema.dump();
  //  } catch (tiledb::TileDBError& e) {
  //    std::cout << e.what() << "\n";
  //    return -1;
  //  }
  
  // Create array
  tiledb::Array::create(dbdir, schema);
  
  // Nothing to clean up - all C++ objects are deleted when exiting scope
  
  //  return 0;
}


// [[Rcpp::export]]
IntegerMatrix region_selection_tiledb(std::string dbdir, 
                                      std::string attr, 
                                      std::vector<int> ridx, 
                                      std::vector<int> cidx, XPtr<tiledb::Config> cfg) {
  // Create TileDB Context
  tiledb::Context ctx(*cfg.get());

  //check ridx,cidx and buf size
  // Create query
  tiledb::Query query(ctx, dbdir, TILEDB_READ);
  query.set_layout(TILEDB_GLOBAL_ORDER);
  
  const std::vector<int> subarray = {ridx[0], ridx[1], cidx[0], cidx[1]};
  query.set_subarray(subarray);
  // Rcout <<  ridx[1] << std::endl;
  auto max_sizes = tiledb::Array::max_buffer_elements(ctx, dbdir, subarray);
  // Rcout << max_sizes[TILEDB_COORDS].second << std::endl;

  int nrow = ridx[1] - ridx[0] + 1;
  int ncol = cidx[1] - cidx[0] + 1;
  // Rcout << nrow << " " << ncol << std::endl;
  IntegerMatrix mat(nrow, ncol);
  int size = nrow * ncol;
  
  int * buf = &mat[0];
  query.set_buffer(attr, buf, size);

  query.submit();
  query.finalize();
  
  return mat;
}
