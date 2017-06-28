#' split the vector into groups so that the disk IO will be performed block-wise
split.block <- function(x){
  #determine block size
  nBlockSize <- min(1000, x)
  nBlocks <- ceiling(length(x)/nBlockSize)
  if(nBlocks > 1)
    blocks <- cut(seq_len(x), nBlocks)
  else
    blocks <- 1
  
  split(seq_len(x), blocks)
}
