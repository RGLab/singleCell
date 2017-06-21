writeDB.blobs <- function(mat, dbfile, nGenes, compress = c("lz4", "gzip")){
  compress <- match.arg(compress)
  
  if(!file.exists(dbfile))
  {
    db_sqlite = dbConnect(SQLite(), dbname=dbfile)
    #write each row (gene) as a blob
    dbGetQuery(db_sqlite, "CREATE TABLE data (idx integer, chunk BLOB)")
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
      df <- list(a = i,
                 z = lapply(i, function(j){
                   
                   obj <- serialize(mat[, j], NULL)
                   if(compress == "lz4")
                     lzCompress(obj)
                   else
                     memCompress(obj)
                 })
      )
      dbSendQuery(db_sqlite, "insert into data values (:a, :z)", df)   
    }
    dbSendStatement(db_sqlite, "CREATE INDEX index_col ON data (idx);")
  }else
    db_sqlite = dbConnect(SQLite(), dbname=dbfile)
  db_sqlite
}

#The API only reads data from H5 by chunks (entire columns)
readDB.blobs <- function(db, rindx = NULL, cindx = NULL, compress = c("lz4", "gzip")){
  compress <- match.arg(compress)
  sql <- paste0("select chunk from data")
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
    cond <- c(paste0("idx IN (", paste(i, collapse = ","), ")" ))
    sub <- dbGetQuery(db, paste0(sql, " where ", cond))
    #unpack blobs
    rows <- lapply(sub[["chunk"]], function(x)
    {
      if(compress == "lz4")
        vec <- lzDecompress(x)
      else
        vec <- memDecompress(x, type = "g")
      vec <- unserialize(vec)
      if(!is.null(rindx))
        vec <- vec[rindx]
      vec
    })
    
    
    do.call(cbind,rows)
  })
  # browser()
  do.call(cbind,res)[,orig.order]
  
  
  
  
  
}
## R API for reading subsetted COO data from sqlite and converting to 2d matrix
readDB <- function(db, rindx = NULL, cindx = NULL, nCells){
  sql <- paste0("select * from data")
  
  cond <- NULL
  if(is.null(rindx))
    rindx <- seq_len(nGenes)-1 #sql use zero based array
  else
  {
    rindx <- rindx-1
    cond <- c(cond, paste0("ridx IN (", paste(rindx, collapse = ","), ")" ))
  }
  
  if(is.null(cindx))
    cindx <- seq_len(nCells)-1
  else
  {
    cindx <- cindx -1 
    cond <- c(cond, paste0("cidx IN (", paste(cindx, collapse = ","), ")" ))
  }
  
  
  cond <- paste(cond, collapse = " and ")
  
  if(!is.null(cond))
    sql <- paste0(sql, " where ", cond)
  
  res <- dbGetQuery(db, sql)
  
  nrow <- length(rindx)
  ncol <- length(cindx)
  m <- matrix(0, nrow, ncol)
  
  #convert global indx to local
  ridx <- res[["ridx"]]
  cidx <- res[["cidx"]]
  ridx <- match(ridx, rindx)
  cidx <- match(cidx, cindx)
  # browser()
  m[cbind(ridx, cidx)] <- res[["values"]]
  
  m
}
