devtools::load_all()
# library(pryr) 
library(HDF5Array)
library(microbenchmark)

path <- "/loc/no-backup/mike/shared"
path <- file.path(path, "1M_neurons")
data_scale <- ""
# data_scale <- "_sub"
h5gz_gene <- file.path(path, paste0("gz_chunk_by_gene", data_scale, ".h5"))

# library(bigmemory)
ind <- 1:100
block.size <- 10
h5seed <- HDF5ArraySeed(h5gz_gene, name = "data")
h5array <- HDF5Array(h5seed)
dim(h5array)

tiledb_dir <- file.path(path, paste0("tiledb_dense_by_col", data_scale))
tiledb_sparse_dir <- file.path(path, paste0("tiledb_sparse_by_col", data_scale))


#create tiledb
# rsize <- 1e4
# csize <- 27998
# idx <- list(1:rsize, 1:csize)
# a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# a <- extract_array(h5seed, idx)
# a <- extract_array(h5seed, list(1:1000, 1:1000))
# object_size(a)


system(paste("du -sh ", h5gz_gene))
system(paste("du -sh ", tiledb_dir))
system(paste("du -sh ", tiledb_sparse_dir))


cfg <- tiledb:::Config()
num_threads <- 4
cfg["vfs.num_threads"] <- num_threads
cfg["vfs.file.max_parallel_ops"] <- num_threads
ctx <- tiledb::Ctx(cfg)
size <- 1e3
idx <- list(1:size, 1:size)

# fileid = H5Fopen(h5gz_gene)
# dsid <- H5Dopen(fileid, "data")
# spaceid <- H5Dget_space(dsid)
# H5Sclose(spaceid)
#' ##native API calls (continous block IO)
microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F),
   # a <- extract_array(h5seed, idx),
  
  b <- h5read_region(h5gz_gene, "data",  c(1,size), c(1,size)),
  c <- region_selection_tiledb(tiledb_dir, "count", c(1,size), c(1,size), ctx@ptr)
  ,d <- region_selection_tiledb_sparse(tiledb_sparse_dir, "count", c(1,size), c(1,size), ctx@ptr)
  , times = 2)
all.equal(b,c,d)
# H5Dclose(dsid)
# H5Fclose(fileid)
#' ## generic API chunked.read (capable of both continuous and non-continuous indexing)
tileseed <- tiledbArraySeed(tiledb_dir, "count")
tilesparseseed <- tiledbArraySeed(tiledb_sparse_dir, "count")
dim(tileseed)
size <- 1e3
idx <- list(1:size, 1:size)

#' ## continuous indexing through chunked.read
microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F),
  a <- chunked.read(h5seed, idx, block.size = block.size),
  # b <- extract_array(h5seed, idx),
  c <- chunked.read(tileseed, idx, block.size = block.size),
  d <- chunked.read(tilesparseseed, idx, block.size = block.size)
  , times = 5)
all.equal(a,c)

#' ## random slicing through chunked.read
set.seed(1)
size <- 1e2
nrow <- nrow(h5seed)
ncol <- ncol(h5seed)
idx <- list(sample(nrow, size), sample(ncol, size))

microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F),
  a <- chunked.read(h5seed, idx, block.size = block.size),
  b <- extract_array(h5seed, idx),
  c <- chunked.read(tileseed, idx, block.size = block.size),
  d <- chunked.read(tilesparseseed, idx, block.size = block.size)
  , times = 5)
all.equal(a,c)

library(profvis)
profvis(extract_array(h5seed, idx))

#' ## common DelayedArray operation (colSums and rowSums)
tilearray <- tiledbArray(tileseed)
tilesparsearray <- tiledbArray(tilesparseseed)
cidx <- sample(ncol, 50)
microbenchmark(
  a <- colSums(h5array[, cidx])
, b <- colSums(tilearray[, cidx])
, c <- colSums(tilesparsearray[, cidx])
, d <- chunked.read(h5seed, idx, block.size = block.size, stats = matrixStats::colSums2)
, e <- chunked.read(tilesparseseed, idx, block.size = block.size, stats = matrixStats::colSums2)
, times = 5)
all.equal(a,b,c,d,e)

ridx <- sample(nrow, 1e2)
microbenchmark(
  a <- rowSums(h5array[ridx, cidx])
  , b <- rowSums(tilearray[ridx, cidx])
  , c <- rowSums(tilesparsearray[ridx, cidx])
  , times = 5)
all.equal(a,b,c)
