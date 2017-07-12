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
      sub <- sub[rind,, drop = FALSE ]
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

#' transpose a 2d data mattrix in H5
#' @param src the H5 file that contains the 'data' mattrix to be transposed
#' @param dest the H5 file name to be written to
#' @export
#' @importFrom rhdf5 H5Fopen H5Dopen H5Dget_space H5Sget_simple_extent_dims H5close
H5transpose <- function(src, dest, compress = c("lz4", "gzip", "none"), nLevel = NULL, verbose = FALSE, ...){
  
  fid <- H5Fopen(src)
  ds <- H5Dopen(fid, "data")
  spc <- H5Dget_space(ds)
  dims <- H5Sget_simple_extent_dims(spc)
  dims <- dims[["size"]]
  H5close()
  dims <- rev(dims) #transpose it
  ncol <- dims[2] 
  nrow <- dims[1]
  compress <- match.arg(compress)
  if(file.exists(dest))
    file.remove(dest)
  
  h5createFile(dest)
  if(is.null(nLevel))
    nLevel <- ifelse(compress == "lz4", -1, 6)
  
  h5createDataset1(dest, "data", rev(c(nrow, ncol)), storage_mode = "double"
                   , chunk_dims = rev(c(nrow, 1))#note that H5 dims start with col
                   , compressor = match(compress, c("lz4", "gzip", "none"))
                   , nLevel = nLevel)
  grps <- split.block(seq_len(ncol), ...)
  #write subset of columns (avoid coerce entire mat into memory)
  for(i in seq_along(grps))
  {
    
    j <- grps[[i]]
    if(verbose)
      cat("group:", i, " out of ", length(grps), "\n")
    #read rows from src
    if(verbose)
      cat("reading the source..")
    mat <- h5read.chunked(src, "data", list(j, NULL), nrow, ...)
    #write them as cols to dest
    if(verbose)
      cat("writing..\n")
    h5write1(t(mat), dest, "data", colIndx = j)
  }
  
  
}
