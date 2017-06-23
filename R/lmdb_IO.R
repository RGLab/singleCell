#' Create sqlite db from matrix
#' Each column is stored as binary blobs and indexed by column id
#' @export
writelmdb.blobs <- function(mat, dbfile, nGenes, compress = c("lz4", "gzip", "none")){
  compress <- match.arg(compress)
  if(!dir.exists(dbfile))
    dir.create(dbfile)
  
  db <- mdb_open(dbfile)  
  #determine block size
  nBlockSize <- min(1000, nGenes)
  nBlocks <- nGenes/nBlockSize
  if(nBlocks > 1)
    blocks <- cut(seq_len(nGenes), nBlocks)
  else
    blocks <- 1
  
  grps <- split(seq_len(nGenes), blocks)
  for(i in grps)
  {
     vecs <- lapply(i, function(j)mat[, j])
    
    mdb_insert_cols(db, i, vecs)   
  }
  mdb_close(db)
}

#' The API only reads data from db by chunks (entire columns)
#' @export
readlmdb.blobs <- function(dbfile, rindx = NULL, cindx = NULL, compress = c("lz4", "gzip")){
  compress <- match.arg(compress)
  db <- mdb_open(dbfile)  
  if(is.null(cindx))
  {
    cindx <- seq_len(nGenes)
    orig.order <- cindx
  }else
  {
    orig.order <- frank(cindx)
  }
  
  nTotal <- length(cindx)
  nBlockSize <- min(1000, nTotal)
  nBlocks <- nTotal/nBlockSize
  if(nBlocks > 1)
    blocks <- cut(cindx, nBlocks)
  else
    blocks <- 1
  col.grps <- split(cindx, blocks)
  res <- lapply(col.grps, function(i){
    # browser()
    #fetch blobs
    cols <- mdb_get_cols(db, cindx)
    
    if(!is.null(rindx))
      cols <- lapply(cols, function(x)x[rindx])
    
    do.call(cbind,cols)
  })
  # browser()
  do.call(cbind,res)[,orig.order]
  
  mdb_close(db)
  
  
  
}
