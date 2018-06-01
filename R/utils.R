#' split the vector into groups so that the disk IO will be performed block-wise
#' @param x a vector to be splitted
split.block <- function(x, block.size = 1e3){
  #determine block size
  nlen <- length(x)
  nBlockSize <- min(block.size, nlen)
  nBlocks <- ceiling(nlen/nBlockSize)
  if(nBlocks > 1)
    blocks <- cut(seq_len(nlen), nBlocks)
  else
    blocks <- 1
  
  split(x, blocks)
}

chunk_selection <- function(x, chunk_idx)UseMethod("chunk_selection")

chunk_selection.HDF5ArraySeed <- function(x, chunk_idx)
{
  extract_array(x, list(NULL, chunk_idx))
    
}

#' random slicing by reading data by chunks (entire columns)
#' @param x DelayedArray seed object that supports 'dim' and 'chunk_selection'
#' @param idx row and col index
#' @export
chunked.read<- function(x, idx, verbose = FALSE, mc.cores = 1, fast = FALSE, stats = NULL, ...){
  require(parallel)
  ridx <- idx[[1]]
  cidx <- idx[[2]]
  dims <- dim(x)
  ncol <- dims[2]
  if(fast&&is.null(ridx))
  {
    nrow <- dims[1]
  }
  if(is.null(cidx))
    cidx <- seq_len(ncol)
  
  if(fast)
  {
    #preallocate doesn't make sense when row subsetting
    #is requested since the full length temp buffer for the chunked vector needs to
    #be allocated anyway and then subset it and copy to the final dest
    #thus no much saving
    if(is.null(ridx))
      mat <- matrix(0, nrow = nrow, ncol = length(cidx))#preallocate
    # else
    #   mat <- matrix(0, nrow = length(ridx), ncol = length(cidx))

  }
  
  
  orig.idx.grps <- split.block(cidx, ...)
  new.idx.grps <- split.block(seq_len(length(cidx)), ...)
  res <- lapply(seq_along(orig.idx.grps), function(i, mat){
    if(verbose)
      message("reading group:", i, " out of ", length(grps))
    orig.idx <- orig.idx.grps[[i]]
    new.idx <- new.idx.grps[[i]]
    if(fast&&is.null(ridx))
      h5read2(h5, name, orig.idx, mat, new.idx)
    else
    {
    # browser()
    
      sub <- chunk_selection(x, orig.idx)
      if(!is.null(ridx))
        sub <- sub[ridx,, drop = FALSE ]
      if(is.null(stats))
        sub
      else
        stats(sub)
    }
  }
  # , mc.cores = mc.cores
  , mat = mat
  )
  
  if(fast)
    mat
  else
  {
    if(is.null(stats))
      do.call(cbind,res)#TODO:this is a significant overhead (sometime cost 50% cpu time)and we should switch to the pre-allocate approach
    else
      unlist(res)
  }
    
}
