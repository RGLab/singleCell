// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/singleCell.h"
#include <Rcpp.h>

using namespace Rcpp;

// h5createDataset1
void h5createDataset1(std::string h5file, std::string ds_name, std::vector<int> dims, std::string storage_mode, std::vector<int> chunk_dims, int compressor, int nLevel);
RcppExport SEXP _singleCell_h5createDataset1(SEXP h5fileSEXP, SEXP ds_nameSEXP, SEXP dimsSEXP, SEXP storage_modeSEXP, SEXP chunk_dimsSEXP, SEXP compressorSEXP, SEXP nLevelSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type h5file(h5fileSEXP);
    Rcpp::traits::input_parameter< std::string >::type ds_name(ds_nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type dims(dimsSEXP);
    Rcpp::traits::input_parameter< std::string >::type storage_mode(storage_modeSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type chunk_dims(chunk_dimsSEXP);
    Rcpp::traits::input_parameter< int >::type compressor(compressorSEXP);
    Rcpp::traits::input_parameter< int >::type nLevel(nLevelSEXP);
    h5createDataset1(h5file, ds_name, dims, storage_mode, chunk_dims, compressor, nLevel);
    return R_NilValue;
END_RCPP
}
// h5write1
bool h5write1(Rcpp::NumericMatrix data, std::string filename, std::string ds_name, std::vector<int> colIndx);
RcppExport SEXP _singleCell_h5write1(SEXP dataSEXP, SEXP filenameSEXP, SEXP ds_nameSEXP, SEXP colIndxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< std::string >::type ds_name(ds_nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type colIndx(colIndxSEXP);
    rcpp_result_gen = Rcpp::wrap(h5write1(data, filename, ds_name, colIndx));
    return rcpp_result_gen;
END_RCPP
}
// h5read1
NumericVector h5read1(std::string filename, std::string ds_name, std::vector<int> colIndx);
RcppExport SEXP _singleCell_h5read1(SEXP filenameSEXP, SEXP ds_nameSEXP, SEXP colIndxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< std::string >::type ds_name(ds_nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type colIndx(colIndxSEXP);
    rcpp_result_gen = Rcpp::wrap(h5read1(filename, ds_name, colIndx));
    return rcpp_result_gen;
END_RCPP
}
// h5read2
void h5read2(std::string filename, std::string ds_name, std::vector<int> src_colIndx, NumericMatrix mat, std::vector<int> dest_colIndx);
RcppExport SEXP _singleCell_h5read2(SEXP filenameSEXP, SEXP ds_nameSEXP, SEXP src_colIndxSEXP, SEXP matSEXP, SEXP dest_colIndxSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< std::string >::type ds_name(ds_nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type src_colIndx(src_colIndxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type dest_colIndx(dest_colIndxSEXP);
    h5read2(filename, ds_name, src_colIndx, mat, dest_colIndx);
    return R_NilValue;
END_RCPP
}
// h5read_region
NumericMatrix h5read_region(std::string filename, std::string ds_name, std::vector<int> ridx, std::vector<int> cidx);
RcppExport SEXP _singleCell_h5read_region(SEXP filenameSEXP, SEXP ds_nameSEXP, SEXP ridxSEXP, SEXP cidxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< std::string >::type ds_name(ds_nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ridx(ridxSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type cidx(cidxSEXP);
    rcpp_result_gen = Rcpp::wrap(h5read_region(filename, ds_name, ridx, cidx));
    return rcpp_result_gen;
END_RCPP
}
// create_tiledb
void create_tiledb(std::string dbdir, std::string attr, std::vector<int> row_domain, std::vector<int> col_domain, std::vector<unsigned> tile_extend, bool isSparse);
RcppExport SEXP _singleCell_create_tiledb(SEXP dbdirSEXP, SEXP attrSEXP, SEXP row_domainSEXP, SEXP col_domainSEXP, SEXP tile_extendSEXP, SEXP isSparseSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type dbdir(dbdirSEXP);
    Rcpp::traits::input_parameter< std::string >::type attr(attrSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type row_domain(row_domainSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type col_domain(col_domainSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned> >::type tile_extend(tile_extendSEXP);
    Rcpp::traits::input_parameter< bool >::type isSparse(isSparseSEXP);
    create_tiledb(dbdir, attr, row_domain, col_domain, tile_extend, isSparse);
    return R_NilValue;
END_RCPP
}
// region_selection_tiledb
NumericMatrix region_selection_tiledb(std::string dbdir, std::string attr, std::vector<int> ridx, std::vector<int> cidx, XPtr<tiledb::Context> ctx_ptr);
RcppExport SEXP _singleCell_region_selection_tiledb(SEXP dbdirSEXP, SEXP attrSEXP, SEXP ridxSEXP, SEXP cidxSEXP, SEXP ctx_ptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type dbdir(dbdirSEXP);
    Rcpp::traits::input_parameter< std::string >::type attr(attrSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ridx(ridxSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type cidx(cidxSEXP);
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx_ptr(ctx_ptrSEXP);
    rcpp_result_gen = Rcpp::wrap(region_selection_tiledb(dbdir, attr, ridx, cidx, ctx_ptr));
    return rcpp_result_gen;
END_RCPP
}
// region_selection_tiledb_sparse
NumericMatrix region_selection_tiledb_sparse(std::string dbdir, std::string attr, std::vector<int> ridx, std::vector<int> cidx, XPtr<tiledb::Context> ctx_ptr);
RcppExport SEXP _singleCell_region_selection_tiledb_sparse(SEXP dbdirSEXP, SEXP attrSEXP, SEXP ridxSEXP, SEXP cidxSEXP, SEXP ctx_ptrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type dbdir(dbdirSEXP);
    Rcpp::traits::input_parameter< std::string >::type attr(attrSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ridx(ridxSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type cidx(cidxSEXP);
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx_ptr(ctx_ptrSEXP);
    rcpp_result_gen = Rcpp::wrap(region_selection_tiledb_sparse(dbdir, attr, ridx, cidx, ctx_ptr));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query
XPtr<tiledb::Query> tiledb_query(XPtr<tiledb::Context> ctx, std::string uri, std::string type);
RcppExport SEXP _singleCell_tiledb_query(SEXP ctxSEXP, SEXP uriSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query(ctx, uri, type));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query_set_coordinates
void tiledb_query_set_coordinates(XPtr<tiledb::Query> query, IntegerVector coords);
RcppExport SEXP _singleCell_tiledb_query_set_coordinates(SEXP querySEXP, SEXP coordsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type coords(coordsSEXP);
    tiledb_query_set_coordinates(query, coords);
    return R_NilValue;
END_RCPP
}
// tiledb_query_finalize
void tiledb_query_finalize(XPtr<tiledb::Query> query);
RcppExport SEXP _singleCell_tiledb_query_finalize(SEXP querySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    tiledb_query_finalize(query);
    return R_NilValue;
END_RCPP
}
// tiledb_dim
IntegerVector tiledb_dim(std::string dbdir);
RcppExport SEXP _singleCell_tiledb_dim(SEXP dbdirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type dbdir(dbdirSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_dim(dbdir));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_singleCell_h5createDataset1", (DL_FUNC) &_singleCell_h5createDataset1, 7},
    {"_singleCell_h5write1", (DL_FUNC) &_singleCell_h5write1, 4},
    {"_singleCell_h5read1", (DL_FUNC) &_singleCell_h5read1, 3},
    {"_singleCell_h5read2", (DL_FUNC) &_singleCell_h5read2, 5},
    {"_singleCell_h5read_region", (DL_FUNC) &_singleCell_h5read_region, 4},
    {"_singleCell_create_tiledb", (DL_FUNC) &_singleCell_create_tiledb, 6},
    {"_singleCell_region_selection_tiledb", (DL_FUNC) &_singleCell_region_selection_tiledb, 5},
    {"_singleCell_region_selection_tiledb_sparse", (DL_FUNC) &_singleCell_region_selection_tiledb_sparse, 5},
    {"_singleCell_tiledb_query", (DL_FUNC) &_singleCell_tiledb_query, 3},
    {"_singleCell_tiledb_query_set_coordinates", (DL_FUNC) &_singleCell_tiledb_query_set_coordinates, 2},
    {"_singleCell_tiledb_query_finalize", (DL_FUNC) &_singleCell_tiledb_query_finalize, 1},
    {"_singleCell_tiledb_dim", (DL_FUNC) &_singleCell_tiledb_dim, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_singleCell(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
