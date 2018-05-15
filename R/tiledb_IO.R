#' write tile db as sparse matrix from dense matrix
#' 
#' @export
write_tiledb_sparse <- function(mat, tiledb_dir, tiledb_attr){
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!dir.exists(tiledb_dir))
    create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol))
  
  for(j in seq_len(ncol))
  {
     vec <- mat[, j]
     ridx <- which(vec>0)
     if(length(ridx) > 0)
     {
       coords <- unlist(lapply(ridx, function(i)c(i,j)))
       write_tiledb(tiledb_dir, tiledb_attr, data = vec[ridx], coords = coords)    
     }
     
  
  }
  
}

#' The API only reads data from db by chunks (entire columns)
#' @importFrom data.table frank
#' @export
readlmdb.blobs <- function(dbfile, rindx = NULL, cindx = NULL, nGenes, compress = c("lz4", "gzip")){
  compress <- match.arg(compress)
  db <- lmdb_open(dbfile)  
  if(is.null(cindx))
  {
    cindx <- seq_len(nGenes)
    orig.order <- cindx
  }else
  {
    orig.order <- frank(cindx)
  }
  col.grps <- split.block(cindx)
  
  res <- lapply(col.grps, function(i){
    # browser()
    #fetch blobs
    cols <- mdb_get_cols(db, i)
    
    if(!is.null(rindx))
      cols <- lapply(cols, function(x)x[rindx])
    
    do.call(cbind,cols)
  })
  # browser()
  lmdb_close(db)
  do.call(cbind,res)[,orig.order]
  
  
  
  
  
}
