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

#create tiledb
# rsize <- 1e4
# csize <- 27998
# idx <- list(1:rsize, 1:csize)
# a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# a <- extract_array(h5seed, idx)
a <- extract_array(h5seed, list(1:1000, 1:1000))
object_size(a)

tiledb_dir <- file.path(path, "tiledb_dense_by_col")
write_tiledb_dense(a, tiledb_dir, "count")
tiledb_dim(tiledb_dir)
system(paste("du -sh ", tiledb_dir))



# tileseed <- tiledbArraySeed(tiledb_dir, "count")
# tilearray <- tiledbArray(tileseed)

size <- 5e2
idx <- list(1:size, 1:size)
microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
  
   b <- extract_array(h5seed, idx)
  # , c <- extract_array(fstseed, idx)
  # , c <- fst.tbl[ridx, cidx]
  
  , c <- region_selection_tiledb(tiledb_dir, "count", c(1,size), c(1,size))

  , times = 1)
all.equal(c,b)

