#' write tile db as sparse matrix from dense matrix
#' 
#' @export
write_tiledb_dense <- function(mat, tiledb_dir, tiledb_attr){
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!dir.exists(tiledb_dir))
    create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol))
  
  for(j in seq_len(ncol))
  {
     write_tiledb(tiledb_dir, tiledb_attr, data = mat[, j], c(1, nrow), c(j,j))    
     
  }
  
}

#' write tile db as sparse matrix from dense matrix
#' 
#' @export
# write_tiledb_sparse <- function(mat, tiledb_dir, tiledb_attr){
#   nrow <- nrow(mat)
#   ncol <- ncol(mat)
#   if(!dir.exists(tiledb_dir))
#     create_tiledb(tiledb_dir, tiledb_attr, row_domain = c(1,nrow), col_domain = c(1, ncol))
#   
#   for(j in seq_len(ncol))
#   {
#     vec <- mat[, j]
#     ridx <- which(vec>0)
#     if(length(ridx) > 0)
#     {
#       coords <- unlist(lapply(ridx, function(i)c(i,j)))
#       write_tiledb(tiledb_dir, tiledb_attr, data = vec[ridx], coords = coords)    
#     }
#     
#     
#   }
#   
# }
