#The API only reads data from H5 by chunks (entire columns)
h5read.chunked <- function(h5, name, index, nGenes){
  rind <- index[[1]]
  cind <- index[[2]]
  if(is.null(cind))
    cind <- seq_len(nGenes)
  nTotal <- length(cind)
  nBlockSize <- min(1000, nTotal)
  nBlocks <- nTotal/nBlockSize
  if(nBlocks > 1)
    blocks <- cut(cind, nBlocks)
  else
    blocks <- 1
  col.grps <- split(cind, blocks)
  res <- lapply(col.grps, function(i){
    sub <- h5read(h5, name, list(NULL, i))
    # browser()
    if(!is.null(rind))
      sub <- sub[rind, ]
    sub
  })
  do.call(cbind,res)
}

H5write.blocks <- function(mat, h5file, nGenes, nCells){
  if(!file.exists(h5file))
  {
    h5createFile(h5file)
    h5createDataset(h5file, "data", c(nCells, nGenes), storage.mode = "double", chunk=c(nCells, 1), level=7)
    #determine block size
    nBlockSize <- min(1000, nGenes)
    nBlocks <- nGenes/nBlockSize
    if(nBlocks > 1)
      blocks <- cut(seq_len(nGenes), nBlocks)
    else
      blocks <- 1
    
    grps <- split(seq_len(nGenes), blocks)
    #write subset of columns (avoid coerce entire mat into memory)
    for(i in grps)
      h5write(as.matrix(mat[,i, drop = F]), h5file, "data", index = list(NULL, i))
  }
}
