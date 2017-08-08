#' @export
setClass("singleCell", slots = c(file_by_gene = "character"
                                 , file_by_cell = "character"
                                 , nGenes = "integer"
                                 , nCells = "integer"))
#' @export
singleCell <- function(file1, file2)
{
  file_by_gene <- file1
  fid <- H5Fopen(file_by_gene)
  ds <- H5Dopen(fid, "data")
  spc <- H5Dget_space(ds)
  dims <- H5Sget_simple_extent_dims(spc)
  dims <- dims[["size"]]
  H5close()
  new("singleCell", file_by_gene = file1, file_by_cell = file2, nGenes = dims[2], nCells = dims[1])
}
