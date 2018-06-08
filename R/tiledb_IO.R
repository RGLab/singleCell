#' write tile db as dense matrix from dense matrix
#' 
#' @export
write_tiledb_dense <- function(mat, tiledb_dir, tiledb_attr, tile_extend){
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!dir.exists(tiledb_dir))
    create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol), tile_extend = tile_extend)
  
  ctx <- tiledb::Ctx()
  qry <- tiledb_query(ctx@ptr, tiledb_dir, "WRITE") 
  # qry <- tiledb:::tiledb_query_set_layout(qry, "GLOBAL_ORDER")
  qry <- tiledb:::tiledb_query_set_layout(qry, "COL_MAJOR")
 
  tile_size <- tile_extend[1] * tile_extend[2]
  j <- 1
  
  while(j <= ncol)
  {
    message(j)
    j2 <- j + tile_extend[2] - 1
    if(j2 > ncol)
      j2 <- ncol
    i <- 1
    while(i <= nrow)
    {
      i2 <- i + tile_extend[1] - 1
      if(i2 > nrow)
        i2 <- nrow
      
      
      if(is(mat , "Array"))
        vec <- extract_array(mat, list(i:i2, j:j2))
      else
        vec <- mat[i:i2, j:j2]
      
      if(length(vec) < tile_size)#pad 0 for incomplete tiles
      {
        vec1 <- matrix(0, tile_extend[1], tile_extend[2])
        vec1[seq_len(i2-i+1), seq_len(j2-j+1)] <- vec
        vec <- vec1
      }
      qry <- tiledb:::tiledb_query_set_subarray(qry, as.integer(c(i, i2, j, j2)))
      qry <- tiledb:::tiledb_query_set_buffer(qry, tiledb_attr, as.numeric(vec))
      qry <- tiledb:::tiledb_query_submit(qry)
      if (tiledb:::tiledb_query_status(qry) != "COMPLETE") {
        stop("error in write query") 
      }
      i <- i2 + 1
    }
    j <- j2 + 1
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
  qry <- tiledb_query(ctx@ptr, tiledb_dir, "WRITE") 
  qry <- tiledb:::tiledb_query_set_layout(qry, "GLOBAL_ORDER") 
  for(j in seq_len(ncol))
  {
    if(is(mat , "Array"))
      vec <- extract_array(mat, list(NULL, j))
    else
      vec <- mat[, j]
    vec <- as.numeric(vec)
    ridx <- which(vec>0)
    if(length(ridx) > 0)
    {
      qry <- tiledb:::tiledb_query_set_buffer(qry, tiledb_attr, vec[ridx])
      coords <- as.integer(unlist(lapply(ridx, function(i)c(i, j))))
      #print(paste(coords, collapse = ", "))
      #message(any(duplicated(coords)))
      tiledb_query_set_coordinates(qry, coords);
      
      qry <- tiledb:::tiledb_query_submit(qry)
      if (tiledb:::tiledb_query_status(qry) != "COMPLETE") {
        stop("error in write query") 
      }
      # write_tiledb(tiledb_dir, tiledb_attr, data = vec[ridx], coords = coords)
    }
  }
  tiledb_query_finalize(qry)
  # tiledb_consolidate();
}
