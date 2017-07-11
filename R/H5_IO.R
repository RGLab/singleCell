#' @import Rcpp
#' @useDynLib singleCell
NULL

#' the API only reads data from H5 by chunks (entire columns)
#' @importFrom rhdf5 h5read
#' @export
h5read.chunked <- function(h5, name, index, ncol, verbose = FALSE, ...){
  rind <- index[[1]]
  cind <- index[[2]]
  if(is.null(cind))
    cind <- seq_len(ncol)
  
  grps <- split.block(cind, ...)
  res <- lapply(seq_along(grps), function(i){
    if(verbose)
      message("reading group:", i, " out of ", length(grps))
    j <- grps[[i]]
    sub <- h5read1(h5, name, j)
    # browser()
    if(!is.null(rind))
      sub <- sub[rind, ]
    sub
  })
  do.call(cbind,res)
}

#' Create the H5 file from matrix
#' @param mat the object that is coercible to matrix through 'as.matrix' method
#' 
#' @importFrom rhdf5 h5createFile
#' @export
H5write.blocks <- function(mat, h5file, ncol, nrow, compress = c("lz4", "gzip", "none"), nLevel = NULL, verbose = FALSE, ...){
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
  grps <- split.block(seq_len(ncol), ...)
  #write subset of columns (avoid coerce entire mat into memory)
  for(i in seq_along(grps))
  {
    if(verbose)
      cat("writing group:", i, " out of ", length(grps), "\n")
    j <- grps[[i]]
    h5write1(as.matrix(mat[,j, drop = F]), h5file, "data", colIndx = j)
  }
    
  
}
