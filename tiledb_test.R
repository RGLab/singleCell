devtools::load_all()
library(pryr) 
library(HDF5Array)
library(microbenchmark)

path <- "/loc/no-backup/mike/shared"
path <- file.path(path, "1M_neurons")
h5gz_gene <- file.path(path, "gz_chunk_by_gene_sub.h5")

# library(bigmemory)
ind <- 1:100
block.size <- 10
h5seed <- HDF5ArraySeed(h5gz_gene, name = "data")
h5array <- HDF5Array(h5seed)
dim(h5array)
system(paste("du -sh ", h5gz_gene))

#create tiledb
rsize <- 1e4
csize <- 27998
idx <- list(1:rsize, 1:csize)
a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# a <- extract_array(h5seed, idx)
# a <- extract_array(h5seed, list(1:1000, 1:1000))
object_size(a)

tiledb_dir <- file.path(path, "tiledb_dense_by_col")
if (dir.exists(tiledb_dir)) {
 unlink(tiledb_dir, recursive = TRUE) 
}

tiledb_sparse_dir <- file.path(path, "tiledb_sparse_by_col")
if (dir.exists(tiledb_sparse_dir)) {
 unlink(tiledb_sparse_dir, recursive = TRUE) 
}

# write_tiledb_dense(h5array, tiledb_dir, "count")
write_tiledb_dense(a, tiledb_dir, "count")
tiledb_dim(tiledb_dir)
system(paste("du -sh ", tiledb_dir))

write_tiledb_sparse(a, tiledb_sparse_dir, "count")
tiledb_dim(tiledb_sparse_dir)
system(paste("du -sh ", tiledb_sparse_dir))


# tileseed <- tiledbArraySeed(tiledb_dir, "count")
# tilearray <- tiledbArray(tileseed)

size <- 1e3
idx <- list(1:size, 1:size)
microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F),
  b <- extract_array(h5seed, idx),
  # c <- region_selection_tiledb(tiledb_dir, "count", c(1,size), c(1,size)),
  d <- region_selection_tiledb_sparse(tiledb_sparse_dir, "count", c(1,size), c(1,size))
  , times = 5)
all.equal(b,d)
