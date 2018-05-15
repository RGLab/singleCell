#' setClass("harraySeed",
#'         contains="Array",
#'         slots=c(
#'           filepath="character" #col_filepath
#'           , row_filepath = "character"
#'           , dims = "integer"
#'           , dimnames = "list"
#'         )
#' )
#' #' @importFrom tools file_path_as_absolute
#' harraySeed <- function(filepath, row_filepath, ...)
#' {
#'  
#'   filepath <- file_path_as_absolute(filepath)
#'   
#'   new("harraySeed", filepath=filepath, ...)
#' }
#' 
#' #' @import methods BiocGenerics S4Vectors IRanges DelayedArray
#' setClass("harray", contains="DelayedArray")
#' 
#' #' @export
#' harray <- function(cmat, rmat)
#' {
#'   stopifnot(is(cmat, "2dmat"))
#'   stopifnot(is(rmat, "2dmat"))
#'   stopifnot(dims(rmat) == rev(dims(cmat)))
#'   structure(
#'     list(col_mat = cmat, row_mat = rmat) 
#'     , class = c("harray")
#'   )
#' }
#' #' @export
#' read.chunked <- function(x, ...) UseMethod("read.chunked")
#' 
#' #' @export
#' read.chunked.harray <- function(x, name, index, ...){
#'   # file_by_gene <- x@file_by_gene
#'   #dispatch to file based on the dimension requested
#'   ridx <- index[[1]]
#'   cind <- index[[2]]
#'   ncell <- x@nCells
#'   ngene <- x@nGenes
#'   if(is.null(cind))
#'     cind <- seq_len(ncell)
#'   if(is.null(gind))
#'     gind <- seq_len(ngene)
#'   if(length(gind) /length(cind) > ngene/ncell)
#'   {
#'     h5read.chunked(x@file_by_cell, name, index, ncell, ...)
#'   }else
#'   {
#'     res <- h5read.chunked(x@file_by_gene, name, rev(index), ngene, ...)
#'     t(res)
#'   }
#'   
#' } 
#' 
#' 
