#include <Rcpp.h>
#include <tiledb/tiledb>

#include <string>
using namespace Rcpp;
// // [[Rcpp::export]]
// IntegerMatrix point_selection_tiledb(std::string dbdir, std::string attr, std::vector<unsigned> ridx, std::vector<unsigned> cidx) {
//   // Create TileDB Context
//   tiledb::Context ctx;
// 
//   unsigned nrow = ridx.size();
//   unsigned ncol = cidx.size();
//   IntegerMatrix mat(nrow, ncol);
//   int * buf = &mat[0];
// 
//   int cnt = 0;
//   for(auto j : cidx)
//     for(auto i : ridx)
//     {
//       // Create query
//       tiledb::Query query(ctx, dbdir, TILEDB_READ);
//       query.set_layout(TILEDB_COL_MAJOR);
//       query.set_subarray<unsigned>({i, i, j, j});
//       query.set_buffer(attr, &(buf[cnt++]), 1);
//       query.submit();
//       query.finalize();
//       //		  auto result_el = query.result_buffer_elements();
//       //		  if(result_el["a1"].second > 0)
//       //			  cout << buf[cnt-1] << endl;
//       //		  cnt++;
//     }
// 
// 
//     //  for (unsigned i = 0; i < result_el["a1"].second; ++i)
//     //    std::cout << a1_data[i] << "\n";
// 
//     // Nothing to clean up - all C++ objects are deleted when exiting scope
// 
//     //  return 0;
//     return mat;
// }
// 

// [[Rcpp::export]]
NumericMatrix region_selection_tiledb_sparse(std::string dbdir
                                               ,  std::string attr
                                               , std::vector<int> ridx
                                               , std::vector<int> cidx
                                               , XPtr<tiledb::Context> ctx_ptr) {
  // Create TileDB Context
  tiledb::Context ctx = *ctx_ptr.get();
  

  //check ridx,cidx and buf size
  // Create query
  tiledb::Query query(ctx, dbdir, TILEDB_READ);
  query.set_layout(TILEDB_COL_MAJOR);

  const std::vector<int> subarray = {ridx[0], ridx[1], cidx[0], cidx[1]};
  query.set_subarray(subarray);
  // Rcout <<  ridx[1] << std::endl;
  auto max_sizes = tiledb::Array::max_buffer_elements(ctx, dbdir, subarray);
  // Rcout << max_sizes[TILEDB_COORDS].second << std::endl;
  std::vector<int> coords_buff(max_sizes[TILEDB_COORDS].second);

  int nrow = ridx[1] - ridx[0] + 1;
  int ncol = cidx[1] - cidx[0] + 1;
  // Rcout << nrow << " " << ncol << std::endl;
  NumericMatrix mat(nrow, ncol);
  int size = nrow * ncol;

  double * buf = &mat[0];
  query.set_buffer(attr, buf, size);
  query.set_coordinates(coords_buff);

  query.submit();
  query.finalize();

  auto result_el = query.result_buffer_elements();
  auto nElements = result_el[attr].second;
  // Rcout << "DEBUG: " << nElements << std::endl;
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
