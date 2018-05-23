setClass("fstArraySeed",
         contains="Array",
         representation(
           filepath="character",       # Absolute path to the HDF5 file so the
           # object doesn't break when the user
           # changes the working directory (e.g. with
           # setwd()).
           name="character",           # Name of the dataset in the HDF5 file.
           dim="integer"
           , first_val="ANY"            # First value in the dataset.
           # , chunkdim="integer_OR_NULL"
         )
)

chunk_selection.fstArraySeed <- function(x, chunk_idx)
{
  res <- read_fst(path(x), paste0("V",chunk_idx))
  res <- as.matrix(res)
  dimnames(res) <- NULL
  res
}

.extract_array_from_fstArraySeed <- function(x, index)
{
  ans_dim <- DelayedArray:::get_Nindex_lengths(index, dim(x))
  if (any(ans_dim == 0L)) {
    ans <- x@first_val[0]
    dim(ans) <- ans_dim
  } else {
    ans <- chunked.read(x, index)
  }
  ans
}

#' @import methods BiocGenerics S4Vectors IRanges DelayedArray
setMethod("extract_array", "fstArraySeed", .extract_array_from_fstArraySeed)

setMethod("path", "fstArraySeed", function(object) object@filepath)

#' @importFrom tools file_path_as_absolute
.normarg_path <- function(path, what)
{
  if (!isSingleString(path))
    stop(wmsg(what, " must be a single string specifying the path ",
              "to the fst dir where the dataset is located"))
  file_path_as_absolute(path)
}

fst_dim <- function(filepath){
  meta <- metadata_fst(filepath)
  as.integer(c(meta$nrOfRows, length(meta$columnBaseTypes)))
  
}

fstArraySeed <- function(filepath, type=NA)
{
  filepath <- .normarg_path(filepath, "'filepath'")
  # if (!isSingleString(name))
  #   stop(wmsg("'name' must be a single string specifying the name ",
  #             "of the dataset in the fst file"))
  # if (name == "")
  #   stop(wmsg("'name' cannot be the empty string"))
  # if (!isSingleStringOrNA(type))
  #   stop("'type' must be a single string or NA")
  dim <- fst_dim(filepath)
  if (any(dim == 0L)) {
    if (is.na(type))
      stop(wmsg("This fst dataset is empty! Don't know how to ",
                "determine the type of an empty fst dataset at the ",
                "moment. Please use the 'type' argument to help me ",
                "(see '?fstArray' for more information)."))
    first_val <- match.fun(type)(1)  # fake value
    if (!is.atomic(first_val))
      stop(wmsg("invalid type: ", type))
  } else {
    first_val <- .read_dataset_first_val_fst(filepath)
    detected_type <- typeof(first_val)
    if (!(is.na(type) || type == detected_type))
      warning(wmsg("The type specified via the 'type' argument (",
                   type, ") doesn't match the type of this fst ",
                   "dataset (", detected_type, "). Ignoring the ",
                   "former."))
  }
  # chunkdim <- h5chunkdim(filepath, name)
  new2("fstArraySeed", filepath=filepath,
       name="name",
       dim=dim,
       first_val=first_val
       # ,chunkdim=chunkdim
       )
}

.read_dataset_first_val_fst <- function(filepath)
{
  # index <- rep.int(list(1L), ndim)
  ans <- read_fst(filepath, "V1", 1, 1)

  stopifnot(length(ans) == 1L)  # sanity check
  ans[[1L]]  # drop any attribute
}

setClass("fstArray", contains="DelayedArray")

fstArray <- function(filepath, name, type=NA)
{
  if (is(filepath, "fstArraySeed")) {
    if (!(missing(name) && identical(type, NA)))
      stop(wmsg("fstArray() must be called with a single argument ",
                "when passed an fstArraySeed object"))
    seed <- filepath
  } else {
    seed <- fstArraySeed(filepath, name, type=type)
  }
  DelayedArray(seed)
}


setMethod("DelayedArray", "fstArray",
          function(seed) new_DelayedArray(seed, Class="fstArray")
)

setClass("fstMatrix", contains=c("DelayedMatrix", "fstArray"))

### Automatic coercion method from fstArray to fstMatrix silently returns
### a broken object (unfortunately these dummy automatic coercion methods
### don't bother to validate the object they return). So we overwrite it.
setAs("fstArray", "fstMatrix", function(from) new("fstMatrix", from))

### The user should not be able to degrade an fstMatrix object to
### an fstArray object so 'as(x, "fstArray", strict=TRUE)' should
### fail or be a no-op when 'x' is an fstMatrix object. Making this
### coercion a no-op seems to be the easiest (and safest) way to go.
setAs("fstMatrix", "fstArray", function(from) from)  # no-op

### For internal use only.
setMethod("matrixClass", "fstArray", function(x) "fstMatrix")

.validate_fstArray <- function(x)
{
  if (!is(x@seed, "fstArraySeed"))
    return(wmsg("'x@seed' must be an fstArraySeed object"))
  TRUE
}

setValidity2("fstArray", .validate_fstArray)

setAs("ANY", "fstMatrix",
      function(from) as(as(from, "fstArray"), "fstMatrix")
)
