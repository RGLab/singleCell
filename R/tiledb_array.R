#' setClass("tiledbArraySeed",
#'          contains="Array",
#'          representation(
#'            filepath="character",       # Absolute path to the HDF5 file so the
#'            # object doesn't break when the user
#'            # changes the working directory (e.g. with
#'            # setwd()).
#'            name="character",           # Name of the dataset in the HDF5 file.
#'            dim="integer"
#'            , first_val="ANY"            # First value in the dataset.
#'            # , chunkdim="integer_OR_NULL"
#'            , sparse  = "logical"
#'            , ctx = "Ctx"
#'          )
#' )
#' 
#' is.sparse <- function(x){
#'   x@sparse
#' }
#' chunk_selection.tiledbArraySeed <- function(x, chunk_idx)
#' {
#'   nrow <- dim(x)[1]
#'   
#'   res <- lapply(chunk_idx, function(j){
#'     if(is.sparse(x))
#'       region_selection_tiledb_sparse(path(x), x@name, c(1,nrow), c(j,j), x@ctx@ptr)
#'     else
#'       region_selection_tiledb(path(x), x@name, c(1,nrow), c(j,j), x@ctx@ptr)
#'   })
#'   
#'   do.call(cbind, res)
#' }
#' 
#' 
#' #' @import methods BiocGenerics S4Vectors IRanges DelayedArray
#' setMethod("extract_array", "tiledbArraySeed", definition = function(x, index).extract_array_from_tiledbArraySeed(x, index))
#' 
#' .extract_array_from_tiledbArraySeed <- function(x, index)
#' {
#'   ans_dim <- DelayedArray:::get_Nindex_lengths(index, dim(x))
#'   if (any(ans_dim == 0L)) {
#'     ans <- x@first_val[0]
#'     dim(ans) <- ans_dim
#'   } else {
#'     ans <- chunked.read(x, index)
#'   }
#'   ans
#' }
#' 
#' 
#' setMethod("path", "tiledbArraySeed", function(object) object@filepath)
#' 
#' #' @importFrom tools file_path_as_absolute
#' .normarg_path <- function(path, what)
#' {
#'   if (!isSingleString(path))
#'     stop(wmsg(what, " must be a single string specifying the path ",
#'               "to the tiledb dir where the dataset is located"))
#'   file_path_as_absolute(path)
#' }
#' tiledbArraySeed <- function(filepath, name, type=NA)
#' {
#'   filepath <- .normarg_path(filepath, "'filepath'")
#'   if (!isSingleString(name))
#'     stop(wmsg("'name' must be a single string specifying the name ",
#'               "of the dataset in the tiledb file"))
#'   if (name == "")
#'     stop(wmsg("'name' cannot be the empty string"))
#'   if (!isSingleStringOrNA(type))
#'     stop("'type' must be a single string or NA")
#'   cfg <- tiledb:::Config()
#'   cfg["vfs.num_threads"] <- 1
#'   cfg["vfs.file.max_parallel_ops"] <- 1
#'   ctx <- tiledb::Ctx(cfg)
#'   
#'   schema_ptr <- tiledb:::tiledb_array_load(ctx@ptr, filepath)
#'   schema <- tiledb:::ArraySchema.from_ptr(schema_ptr)
#'   sparse <- tiledb:::is.sparse(schema)
#'     
#'   dim <- tiledb_dim(filepath)
#'   if (any(dim == 0L)) {
#'     if (is.na(type))
#'       stop(wmsg("This tiledb dataset is empty! Don't know how to ",
#'                 "determine the type of an empty tiledb dataset at the ",
#'                 "moment. Please use the 'type' argument to help me ",
#'                 "(see '?tiledbArray' for more information)."))
#'     first_val <- match.fun(type)(1)  # fake value
#'     if (!is.atomic(first_val))
#'       stop(wmsg("invalid type: ", type))
#'   } else {
#'     first_val <- .read_dataset_first_val(filepath, name, length(dim), ctx, sparse)
#'     detected_type <- typeof(first_val)
#'     if (!(is.na(type) || type == detected_type))
#'       warning(wmsg("The type specified via the 'type' argument (",
#'                    type, ") doesn't match the type of this tiledb ",
#'                    "dataset (", detected_type, "). Ignoring the ",
#'                    "former."))
#'   }
#'   
#'   
#'   new2("tiledbArraySeed", filepath=filepath,
#'        name=name,
#'        dim=dim,
#'        first_val=first_val
#'        ,sparse=sparse
#'        , ctx = ctx
#'        )
#' }
#' 
#' .read_dataset_first_val <- function(filepath, name, ndim, ctx, sparse)
#' {
#'   
#'   # index <- rep.int(list(1L), ndim)
#'   if(sparse)
#'     ans <- region_selection_tiledb_sparse(filepath, name, c(1,1), c(1,1), ctx@ptr)
#'   else
#'     ans <- region_selection_tiledb(filepath, name, c(1,1), c(1,1), ctx@ptr)
#' 
#'   stopifnot(length(ans) == 1L)  # sanity check
#'   ans[[1L]]  # drop any attribute
#' }
#' 
#' setClass("tiledbArray", contains="DelayedArray")
#' 
#' tiledbArray <- function(filepath, name, type=NA)
#' {
#'   if (is(filepath, "tiledbArraySeed")) {
#'     if (!(missing(name) && identical(type, NA)))
#'       stop(wmsg("tiledbArray() must be called with a single argument ",
#'                 "when passed an tiledbArraySeed object"))
#'     seed <- filepath
#'   } else {
#'     seed <- tiledbArraySeed(filepath, name, type=type)
#'   }
#'   DelayedArray(seed)
#' }
#' 
#' 
#' setMethod("DelayedArray", "tiledbArray",
#'           function(seed) new_DelayedArray(seed, Class="tiledbArray")
#' )
#' 
#' setClass("tiledbMatrix", contains=c("DelayedMatrix", "tiledbArray"))
#' 
#' ### Automatic coercion method from tiledbArray to tiledbMatrix silently returns
#' ### a broken object (unfortunately these dummy automatic coercion methods
#' ### don't bother to validate the object they return). So we overwrite it.
#' setAs("tiledbArray", "tiledbMatrix", function(from) new("tiledbMatrix", from))
#' 
#' ### The user should not be able to degrade an tiledbMatrix object to
#' ### an tiledbArray object so 'as(x, "tiledbArray", strict=TRUE)' should
#' ### fail or be a no-op when 'x' is an tiledbMatrix object. Making this
#' ### coercion a no-op seems to be the easiest (and safest) way to go.
#' setAs("tiledbMatrix", "tiledbArray", function(from) from)  # no-op
#' 
#' ### For internal use only.
#' setMethod("matrixClass", "tiledbArray", function(x) "tiledbMatrix")
#' 
#' .validate_tiledbArray <- function(x)
#' {
#'   if (!is(x@seed, "tiledbArraySeed"))
#'     return(wmsg("'x@seed' must be an tiledbArraySeed object"))
#'   TRUE
#' }
#' 
#' setValidity2("tiledbArray", .validate_tiledbArray)
#' 
#' setAs("ANY", "tiledbMatrix",
#'       function(from) as(as(from, "tiledbArray"), "tiledbMatrix")
#' )
