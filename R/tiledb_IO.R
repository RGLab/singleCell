#' write tile db as dense matrix from dense matrix
#' 
#' @export
write_tiledb_dense <- function(mat, tiledb_dir, tiledb_attr){
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!dir.exists(tiledb_dir))
    create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol), tile_extend = c(nrow, 1))#tile by col
  
  ctx <- tiledb::Ctx()
  qry <- tiledb:::tiledb_query(ctx@ptr, tiledb_dir, "WRITE") 
  qry <- tiledb:::tiledb_query_set_layout(qry, "GLOBAL_ORDER")
  for(j in seq_len(ncol))
  {
    
    qry <- tiledb:::tiledb_query_set_buffer(qry, tiledb_attr, as.integer(mat[, j]))
    qry <- tiledb:::tiledb_query_submit(qry)
    if (tiledb:::tiledb_query_status(qry) != "COMPLETE") {
      stop("error in write query") 
    }
    # tiledb_query_reset_buffers(qry)

  }
  tiledb_query_finalize(qry) 
}

#' write tile db as sparse matrix from dense matrix
#' 
#' @export
write_tiledb_sparse <- function(mat, tiledb_dir, tiledb_attr){
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!dir.exists(tiledb_dir))
    create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol), tile_extend = c(nrow, 1), isSparse = TRUE)

  ctx <- tiledb::Ctx()
  qry <- tiledb:::tiledb_query(ctx@ptr, tiledb_dir, "WRITE") 
  qry <- tiledb:::tiledb_query_set_layout(qry, "GLOBAL_ORDER")
  for(j in seq_len(ncol))
  {
    vec <- as.integer(mat[, j])
    ridx <- which(vec>0)
    if(length(ridx) > 0)
    {
      
      qry <- tiledb:::tiledb_query_set_buffer(qry, tiledb_attr, vec)
      coords <- unlist(lapply(ridx, function(i)c(i,j)))
      
      tiledb_query_set_coordinates(qry, coords);
      
      qry <- tiledb:::tiledb_query_submit(qry)
      if (tiledb:::tiledb_query_status(qry) != "COMPLETE") {
        stop("error in write query") 
      }
      # write_tiledb(tiledb_dir, tiledb_attr, data = vec[ridx], coords = coords)
    }
    tiledb_query_finalize(qry) 
    

  }

}
