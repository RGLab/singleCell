#' @import Rcpp
#' @useDynLib singleCell
NULL

#' @export
h5read.chunked <- function(x, ...) UseMethod("h5read.chunked")

#' @export
h5read.chunked.singleCell <- function(x, name, index, ...){
  # file_by_gene <- x@file_by_gene
  #dispatch to file based on the dimension requested
  gind <- index[[1]]
  cind <- index[[2]]
  ncell <- x@nCells
  ngene <- x@nGenes
  if(is.null(cind))
    cind <- seq_len(ncell)
  if(is.null(gind))
    gind <- seq_len(ngene)
  if(length(gind) /length(cind) > ngene/ncell)
  {
    h5read.chunked(x@file_by_cell, name, index, ncell, ...)
  }else
  {
    res <- h5read.chunked(x@file_by_gene, name, rev(index), ngene, ...)
    t(res)
  }
    
} 

#' the API only reads data from H5 by chunks (entire columns)
#' @importFrom rhdf5 h5read
#' @export
h5read.chunked.character <- function(x, name, index, ncol, verbose = FALSE, mc.cores = 1, fast = TRUE, ...){
  require(parallel)
  h5 <- x
  rind <- index[[1]]
  cind <- index[[2]]
  
  if(fast&&is.null(rind))
  {
    fid <- H5Fopen(x)
    ds <- H5Dopen(fid, "data")
    spc <- H5Dget_space(ds)
    dims <- H5Sget_simple_extent_dims(spc)
    dims <- dims[["size"]]
    H5close()
    
    # ncol <- dims[2] 
    nrow <- dims[1]
  }
  if(is.null(cind))
    cind <- seq_len(ncol)
  
  if(fast)
  {
    #preallocate doesn't make sense when row subsetting
    #is requested since the full length temp buffer for the chunked vector needs to 
    #be allocated anyway and then subset it and copy to the final dest
    #thus no much saving
    if(is.null(rind))
      mat <- matrix(0, nrow = nrow, ncol = length(cind))#preallocate
    # else
    #   mat <- matrix(0, nrow = length(rind), ncol = length(cind))
      
  }
    
  
  orig.idx.grps <- split.block(cind, ...)
  new.idx.grps <- split.block(seq_len(length(cind)), ...)
  res <- lapply(seq_along(orig.idx.grps), function(i, mat){
    if(verbose)
      message("reading group:", i, " out of ", length(grps))
    orig.idx <- orig.idx.grps[[i]]
    new.idx <- new.idx.grps[[i]]
    if(fast&&is.null(rind))
      h5read2(h5, name, orig.idx, mat, new.idx)
    else
    {
        sub <- h5read1(h5, name, orig.idx)
      # browser()
      if(!is.null(rind))
        sub <- sub[rind,, drop = FALSE ]
      sub
    }
  }
  # , mc.cores = mc.cores
  , mat = mat
  )
  
  if(fast)
    mat
  else
  do.call(cbind,res)#TODO:this is a significant overhead (sometime cost 50% cpu time)and we should switch to the pre-allocate approach
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
H5transpose <- function(src, dest, col.ind = NULL, compress = c("lz4", "gzip", "none"), nLevel = NULL, verbose = FALSE, ...){
  
  fid <- H5Fopen(src)
  ds <- H5Dopen(fid, "data")
  spc <- H5Dget_space(ds)
  dims <- H5Sget_simple_extent_dims(spc)
  dims <- dims[["size"]]
  H5close()
  dims <- rev(dims) #transpose it
  ncol <- dims[2] 
  nrow <- dims[1]
  if(is.null(col.ind))
    nrow.new <- nrow
  else
    nrow.new <- length(col.ind)
  compress <- match.arg(compress)
  if(file.exists(dest))
    file.remove(dest)
  
  h5createFile(dest)
  if(is.null(nLevel))
    nLevel <- ifelse(compress == "lz4", -1, 6)
  
  h5createDataset1(dest, "data", rev(c(nrow.new, ncol)), storage_mode = "double"
                   , chunk_dims = rev(c(nrow.new, 1))#note that H5 dims start with col
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
    mat <- h5read.chunked(src, "data", list(j, col.ind), nrow, ...)
    #write them as cols to dest
    if(verbose)
      cat("writing..\n")
    h5write1(t(mat), dest, "data", colIndx = j)
  }
  
  
}
