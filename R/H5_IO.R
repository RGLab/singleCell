#' @useDynLib singleCell
NULL

#' the API only reads data from H5 by chunks (entire columns)
#' @importFrom rhdf5 h5read
#' @export
h5read.chunked <- function(h5, name, index, ncol){
  rind <- index[[1]]
  cind <- index[[2]]
  if(is.null(cind))
    cind <- seq_len(ncol)
  nTotal <- length(cind)
  nBlockSize <- min(1000, nTotal)
  nBlocks <- nTotal/nBlockSize
  if(nBlocks > 1)
    blocks <- cut(cind, nBlocks)
  else
    blocks <- 1
  col.grps <- split(cind, blocks)
  res <- lapply(col.grps, function(i){
    sub <- h5read1(h5, name, i)
    # browser()
    if(!is.null(rind))
      sub <- sub[rind, ]
    sub
  })
  do.call(cbind,res)
}

#' Create the H5 file from matrix
#' @importFrom rhdf5 h5createFile
#' @export
H5write.blocks <- function(mat, h5file, ncol, nrow, compress = c("lz4", "gzip", "none"), nLevel = NULL){
  compress <- match.arg(compress)
  if(file.exists(h5file))
    file.remove(h5file)
  
  h5createFile(h5file)
  if(is.null(nLevel))
    nLevel <- ifelse(compress == "lz4", -1, 6)
  
  h5createDataset1(h5file, "data", rev(c(nrow, ncol)), storage_mode = "double"
                   , chunk_dims = rev(c(nrow, 1))#note that H5 dims start with col
                   , compressor = match(compress, c("lz4", "gzip", "none"))
                   , nLevel = nLevel)
  #determine block size
  nBlockSize <- min(1000, ncol)
  nBlocks <- ncol/nBlockSize
  if(nBlocks > 1)
    blocks <- cut(seq_len(ncol), nBlocks)
  else
    blocks <- 1
  
  grps <- split(seq_len(ncol), blocks)
  #write subset of columns (avoid coerce entire mat into memory)
  for(i in grps)
    h5write1(as.matrix(mat[,i, drop = F]), h5file, "data", colIndx = i)
  
}
