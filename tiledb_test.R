# library(singleCell)
# Sys.setenv(HDF5_PLUGIN_PATH="/home/wjiang2/rglab/workspace/h5lz/src/Release/")
path <- "/loc/no-backup/mike/shared"
path <- file.path(path, "1M_neurons")

# h5lz_cell <- file.path(path, "lz_chunk_by_cell_sub.h5")
h5gz_gene <- file.path(path, "gz_chunk_by_gene_sub.h5")
library(microbenchmark)
# library(bigmemory)
ind <- 1:100
#   sample(1e4, 100)
block.size <- 10
# 
# microbenchmark(
#   a <- h5read.chunked(h5lz_cell, "data", list(NULL, ind), block.size = block.size, fast = F)
#   ,b <- h5read.chunked(h5lz_cell, "data", list(NULL, ind), block.size = block.size, fast = T)
#   , times = 1)
# 
# microbenchmark(
# a <- h5read.chunked(h5gz_cell, "data", list(NULL, ind), block.size = block.size, fast = T)
# ,b <- h5read.chunked(h5lz_cell, "data", list(NULL, ind), block.size = block.size, fast = T)
# , times = 10)
#   all.equal(a,b)
# 
# library(profvis)
# profvis(a <- h5read.chunked(h5gz_cell, "data", list(NULL, ind), block.size = block.size))
# profvis(b <- h5read.chunked(h5lz_cell, "data", list(NULL, ind), block.size = block.size))


# b <- h5read.chunked(h5lz_cell, "data", list(ind, ind), block.size = block.size, fast = F)

# a <- b[1:10, 1:10]
library(pryr) 
library(HDF5Array)
h5seed <- HDF5ArraySeed(h5gz_gene, name = "data")
h5array <- HDF5Array(h5seed)
dim(h5array)

#create tiledb
rsize <- 1e4
csize <- 27998
idx <- list(1:rsize, 1:csize)
# a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# a <- extract_array(h5seed, idx)
a <- extract_array(h5seed, list(1:1000, 1:1000))
object_size(a)

tiledb_dir <- file.path(path, "tiledb_dense_by_col")
write_tiledb_dense(a, tiledb_dir, "count")
tiledb_dim(tiledb_dir)
system(paste("du -sh ", tiledb_dir))



tileseed <- tiledbArraySeed(tiledb_dir, "count")
tilearray <- tiledbArray(tileseed)

#fst
# library(fst)
# 
# fstfile <- file.path(path, "fst_10k.fst")
# 
# aa <- as.data.frame(a)
# object_size(aa)
# write.fst(aa, fstfile, compress = 100)
# fstseed <- fstArraySeed(fstfile)
#region selection
size <- 1e3
idx <- list(1:size, 1:size)
microbenchmark(
  # a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
  
   b <- extract_array(h5seed, idx)
  # , c <- extract_array(fstseed, idx)
  # , c <- fst.tbl[ridx, cidx]
  
  , c <- region_selection_tiledb(tiledb_dir, "count", c(1,size), c(1,size))

  , times = 1)
all.equal(c,b)

file.size(h5gz_gene)/10^6
file.size(fstfile)/10^6


ridx <- sample(seq_len(rsize), 1000)
cidx <- sample(seq_len(csize), 1000)
idx <- list(ridx, cidx)
microbenchmark(
    a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# ,   b <- chunked.read(h5seed, idx, block.size = block.size)
  # , c <- extract_array(h5seed, idx)
  , c <- chunked.read(fstseed, idx, block.size = block.size)
  # , c <- as.matrix(fst.tbl[ridx, cidx])
  # , d <- chunked.read(tileseed, idx, block.size = block.size)
  
  , times = 1)


all.equal(c,a)
library(profvis)

profvis(d <- fst.tbl[ridx, cidx])
