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
  auto d1 = tiledb::Dimension::create<unsigned>(ctx, "row", row);
  auto d2 = tiledb::Dimension::create<unsigned>(ctx, "col", col);
  
  //  auto d1 = tiledb::Dimension::create<uint64_t>(ctx, "row", {{1, 4}}, 2);
  //  auto d2 = tiledb::Dimension::create<uint64_t>(ctx, "col", {{1, 4}}, 2);
  
  // Create domain
  tiledb::Domain domain(ctx);
  domain.add_dimension(d1).add_dimension(d2);
  
  // Create attributes
  tiledb::Attribute a1 = tiledb::Attribute::create<int>(ctx, attr);
  //  a1.set_compressor({TILEDB_LZ4, -1});
  
  // Create array schema
  tiledb::ArraySchema schema(ctx, TILEDB_SPARSE);
  schema.set_order({{TILEDB_COL_MAJOR, TILEDB_ROW_MAJOR}});
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
// [[Rcpp::export]]
void write_tiledb(std::string dbdir, std::string attr, std::vector<int> data, std::vector<unsigned> coords) {
  // Create TileDB context
  tiledb::Context ctx;
  
  // Create query
  tiledb::Query query(ctx, dbdir, TILEDB_WRITE);
  query.set_layout(TILEDB_GLOBAL_ORDER);
  query.set_buffer(attr, data);
  //  query.set_buffer("a2", a2_buff);
  //  query.set_buffer("a3", a3_buff);
  query.set_coordinates(coords);
  
  // Submit query
  query.submit();
  // Finalize query
  query.finalize();
  
  // Nothing to clean up - all C++ objects are deleted when exiting scope
  
  //  return 0;
}
// [[Rcpp::export]]
IntegerMatrix point_selection_tiledb(std::string dbdir, std::string attr, std::vector<unsigned> ridx, std::vector<unsigned> cidx) {
  // Create TileDB Context
  tiledb::Context ctx;
  
  unsigned nrow = ridx.size();
  unsigned ncol = cidx.size();
  IntegerMatrix mat(nrow, ncol);
  int * buf = &mat[0];
  
  int cnt = 0;
  for(auto j : cidx)
    for(auto i : ridx)
    {
      // Create query
      tiledb::Query query(ctx, dbdir, TILEDB_READ);
      query.set_layout(TILEDB_COL_MAJOR);
      query.set_subarray<uint64_t>({i, i, j, j});
      query.set_buffer(attr, &(buf[cnt++]), 1);
      query.submit();
      query.finalize();
      //		  auto result_el = query.result_buffer_elements();
      //		  if(result_el["a1"].second > 0)
      //			  cout << buf[cnt-1] << endl;
      //		  cnt++;
    }
    
    
    //  for (unsigned i = 0; i < result_el["a1"].second; ++i)
    //    std::cout << a1_data[i] << "\n";
    
    // Nothing to clean up - all C++ objects are deleted when exiting scope
    
    //  return 0;
    return mat;
}
// [[Rcpp::export]]
IntegerMatrix region_selection_tiledb(std::string dbdir,  std::string attr, std::vector<unsigned> ridx, std::vector<unsigned> cidx) {
  // Create TileDB Context
  tiledb::Context ctx;

  //check ridx,cidx and buf size
  // Create query
  tiledb::Query query(ctx, dbdir, TILEDB_READ);
  query.set_layout(TILEDB_COL_MAJOR);
  
  const std::vector<unsigned> subarray = {(unsigned)ridx[0], (unsigned)ridx[1], (unsigned)cidx[0], (unsigned)cidx[1]};
  query.set_subarray(subarray);
  // Rcout <<  ridx[1] << std::endl;
  auto max_sizes = tiledb::Array::max_buffer_elements(ctx, dbdir, subarray);
  // Rcout << max_sizes[TILEDB_COORDS].second << std::endl;
  std::vector<unsigned> coords_buff(max_sizes[TILEDB_COORDS].second);
  
  unsigned nrow = ridx[1] - ridx[0] + 1;
  unsigned ncol = cidx[1] - cidx[0] + 1;
  // Rcout << nrow << " " << ncol << std::endl;
  IntegerMatrix mat(nrow, ncol);
  unsigned size = nrow * ncol;
  
  int * buf = &mat[0];
  query.set_buffer(attr, buf, size);
  query.set_coordinates(coords_buff);
  
  query.submit();
  query.finalize();
  
  auto result_el = query.result_buffer_elements();
  auto nElements = result_el[attr].second;
  // Rcout << nElements << std::endl;
  //repopulate buf by assigning elements to its right position
  for(int i = nElements - 1; i >=0; i--)
  {
    auto coord_idx = 2*i;
    auto r_offset = coords_buff[coord_idx] - ridx[0];
    auto c_offset = coords_buff[coord_idx + 1] - cidx[0];//convert to zero-based idx
    auto offset = c_offset * nrow + r_offset;
    std::swap(buf[offset], buf[i]);
  }
  //	  auto result_el = query.result_buffer_elements();
  //	  if(result_el["a1"].second > 0)
  //		  cout << buf[cnt-1] << endl;
  //		  cnt++;
  
  
  //  for (unsigned i = 0; i < result_el["a1"].second; ++i)
  //    std::cout << a1_data[i] << "\n";
  
  // Nothing to clean up - all C++ objects are deleted when exiting scope
  
   return mat;
}
