devtools::load_all()
# library(pryr) 
library(HDF5Array)
# library(microbenchmark)

path <- "/loc/no-backup/mike/shared"
path <- file.path(path, "1M_neurons")
# data_scale <- ""
data_scale <- "_sub"
# chunk_shape <- "chunk_by_gene"
# chunk_shape <- "1k_1k"
chunk_shape <- "1k_1k"
h5gz_gene <- file.path(path, paste0("gz_", chunk_shape, data_scale, ".h5"))


# library(bigmemory)
# ind <- 1:100
# block.size <- 10
h5seed <- HDF5ArraySeed(h5gz_gene, name = "data")
h5array <- HDF5Array(h5seed)
dim(h5array)

chunk_shape <- "1k_1k"
# chunk_shape <- "by_col"

tiledb_dir <- file.path(path, paste0("tiledb_dense_", chunk_shape, data_scale))
tiledb_sparse_dir <- file.path(path, paste0("tiledb_sparse_", chunk_shape, data_scale))

print_schema(tiledb_dir)

#create tiledb
# rsize <- 1e4
# csize <- 27998
# idx <- list(1:rsize, 1:csize)
# a <- h5read.chunked(h5gz_gene, "data", idx, block.size = block.size, fast = F)
# a <- extract_array(h5seed, idx)
# a <- extract_array(h5seed, list(1:1000, 1:1000))
# object_size(a)


if (dir.exists(tiledb_dir)) {
 unlink(tiledb_dir, recursive = TRUE)
}
# if (dir.exists(tiledb_sparse_dir)) {
#  unlink(tiledb_sparse_dir, recursive = TRUE) 
# }
# 
# # write_tiledb_dense(h5array, tiledb_dir, "count")

cfg <- tiledb:::Config()
num_threads <- 4
cfg["vfs.num_threads"] <- num_threads
cfg["vfs.file.max_parallel_ops"] <- num_threads


write_tiledb_dense(h5array[1:1e2, 1:1e2], tiledb_dir, "count", c(20, 20))
ctx <- tiledb::Ctx(cfg)
tilearray<- tiledb::TileArray.load(ctx, tiledb_dir)

all.equal(extract_array(h5array, list(1:90, 1:1e2)), tilearray[1:90, 1:1e2])
# 
# write_tiledb_sparse(h5seed, tiledb_sparse_dir, "count")



