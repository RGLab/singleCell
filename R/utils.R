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
