#' simulate single cell data as the sparse matrix
#' This is the slow version is deprecated by simulate_sparse_mat_fast
#' @export
simulate_sparse_mat <- function(alpha, beta, nCells){
  
  pi_g <- rbeta(n = nGenes, alpha, beta)
  
  #set 30% rows to all 0s
  all_zero_row_id <- sample(nGenes, 0.3 * nGenes)
  
  m <- Matrix(0,nrow = nCells, ncol = nGenes, sparse = T)
  for(i in seq_len(nGenes))
  {
    if(!(i %in% all_zero_row_id))
    {
      col <- sample(x = c(0, 10), size = nCells,  prob = c(1-pi_g[i], pi_g[i]),replace = T)
      rowidx <- which(col !=0)
      nLen <- length(rowidx)
      col[rowidx]=runif(nLen,min=0.1,max=1)
      
      m[, i] <- col
      
    }
    
  }
  m
}

simulate_sparse_mat_v1 <- function(alpha, beta, nCells){
  
  pi_g <- rbeta(n = nGenes, alpha, beta)
  
  #set 30% rows to all 0s
  all_zero_row_id <- sample(nGenes, 0.3 * nGenes)
  
  #construct mat from csr format
  p <- vector(mode = "integer", length = nGenes+1)#row ptr
  nLen <- 0
  ptr <- 0
  x <- integer()#non zero vals
  j <- integer()#col idx
  for(i in seq_len(nGenes))
  {
    
    p[i] <- ptr
    if(!(i %in% all_zero_row_id))
    {
      row <- sample(x = c(0, 10), size = nCells,  prob = c(1-pi_g[i], pi_g[i]),replace = T)
      colidx <- which(row !=0)
      nLen <- length(colidx)
      row[colidx]=runif(nLen,min=0.1,max=1)
      
      if(nLen > 0){
        j <- c(j, colidx - 1)#add col idx (zero-based)
        x <- c(x, row[colidx]) #add non zero vals
        ptr <- ptr + nLen#mv ptr for the next row
        
      }
      
      
    }else
      nLen <- 0
    
    
  }
  # browser()
  #add the end idx for last row
  p[i+1] <- length(x)
  # browser()
  sparseMatrix(j = j, p = p, x = x, index1 = F, dims = c(nGenes, nCells))
}

# more efficient version (cells x gene)
simulate_sparse_mat_v2 <- function(alpha, beta, nCells){
  
  pi_g <- rbeta(n = nGenes, alpha, beta)
  
  #set 30% rows to all 0s
  all_zero_col_id <- sample(nGenes, 0.3 * nGenes)
  
  #construct mat from csr format
  p <- vector(mode = "integer", length = nGenes+1)#row ptr
  nLen <- 0
  ptr <- 0
  x <- integer()#non zero vals
  j <- integer()#col idx
  for(i in seq_len(nGenes))
  {
    
    p[i] <- ptr
    if(!(i %in% all_zero_col_id))
    {
      col <- sample(x = c(0, 10), size = nCells,  prob = c(1-pi_g[i], pi_g[i]),replace = T)
      rowidx <- which(col !=0)
      nLen <- length(rowidx)
      col[rowidx]=runif(nLen,min=0.1,max=1)
      
      if(nLen > 0){
        j <- c(j, rowidx - 1)#add row idx (zero-based)
        x <- c(x, col[rowidx]) #add non zero vals
        ptr <- ptr + nLen#mv ptr for the next col
        
      }
      
      
    }else
      nLen <- 0
    
    
  }
  # browser()
  #add the end idx for last row
  p[i+1] <- length(x)
  # browser()
  sparseMatrix(i = j, p = p, x = x, index1 = F, dims = c(nCells, nGenes))
}


#' more efficient version (gene x cells) by preallocating vector
#' @import Matrix
#' @export
simulate_sparse_mat_v3 <- function(alpha, beta, nCells, cpp = FALSE){
  
  pi_g <- rbeta(n = nGenes, alpha, beta)
   
  maxLen <- nCells * nGenes * alpha/(alpha + beta)#set the upper limit of vec length (it will be a little bigger than the actual non-zero items)
  #set 30% rows to all 0s
  all_zero_col_id <- sample(nGenes, 0.3 * nGenes)
  if(cpp)
    builhash(all_zero_col_id)
  #construct mat from csr format
  p <- vector(mode = "integer", length = nGenes+1)#row ptr
  nLen <- 0
  ptr <- 0
  x <- numeric(length = maxLen)#non zero vals
  j <- integer(length = maxLen)#col idx
  for(i in seq_len(nGenes))
  {
    
    p[i] <- ptr
    exist <- ifelse(cpp, inHash(i), i%in%all_zero_col_id)
    if(!exist)
    {
      col <- sample(x = c(0, 10), size = nCells,  prob = c(1-pi_g[i], pi_g[i]),replace = T)
      rowidx <- which(col !=0)
      nLen <- length(rowidx)

      if(nLen > 0){
        end <- ptr + nLen
        ind <- (ptr+1):end
        if(cpp)
        {
          setVec_i(j, ptr + 1, nLen, rowidx - 1)#add row idx (zero-based)
          setVec_d(x, ptr + 1, nLen, runif(nLen,min=0.1,max=1)) #add non zero vals  
        }else
        {
          j[ind] <- rowidx - 1#add row idx (zero-based)
          x[ind] <- runif(nLen,min=0.1,max=1) #add non zero vals  
        }
        
        ptr <- end#mv ptr for the next col
        
      }
      
      
    }else
      nLen <- 0
    
    
  }
  j <- j[1:ptr]
  x <- x[1:ptr]
  # browser()
  #add the end idx for last row
  p[i+1] <- length(x)
  # browser()
  sparseMatrix(i = j, p = p, x = x, index1 = F, dims = c(nCells, nGenes))
}
