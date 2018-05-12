#' @export
h5mat <- function(file, name = "data"){
  fid <- H5Fopen(file)
  ds <- H5Dopen(fid, name)
  spc <- H5Dget_space(ds)
  dims <- H5Sget_simple_extent_dims(spc)
  
  H5close()
  structure(
    list(file = file, name = name,dims <- dims[["size"]])
    ,class = c("h5mat", "2dmat")
  )
}


setMethod("dims", "2dmat", function(object)object[["dims"]])

#' the API only reads data from H5 by chunks 
#' @importFrom rhdf5 h5read
#' @export
read_by_col.h5mat <- function(x, name, index, ncol, verbose = FALSE, mc.cores = 1, fast = TRUE, ...){
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
