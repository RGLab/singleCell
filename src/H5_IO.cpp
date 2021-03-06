#include <Rcpp.h>
#include <hdf5.h>
using namespace Rcpp;

#define TRUE            1
#define FALSE           0

#define MSG_SIZE       1024
#define H5Z_FILTER_LZ4        32004

herr_t my_hdf5_error_handler(unsigned n, const H5E_error2_t *err_desc, void *client_data)
{
  char                maj[MSG_SIZE];
  char                min[MSG_SIZE];
  
  const int		indent = 4;
  
  if(H5Eget_msg(err_desc->maj_num, NULL, maj, MSG_SIZE)<0)
    return -1;
  
  if(H5Eget_msg(err_desc->min_num, NULL, min, MSG_SIZE)<0)
    return -1;
  
  REprintf("%*s error #%03d: in %s(): line %u\n",
           indent, "", n, err_desc->func_name, err_desc->line);
  REprintf("%*smajor: %s\n", indent*2, "", maj);
  REprintf("%*sminor: %s\n", indent*2, "", min);
  
  return 0;
}

/*
 * customize the printing function so that it print to R error console
 * also raise the R error once the error stack printing is done
 */
herr_t custom_print_cb(hid_t estack, void *client_data)
{
  hid_t estack_id = H5Eget_current_stack();//copy stack before it is corrupted by my_hdf5_error_handler
  H5Ewalk2(estack_id, H5E_WALK_DOWNWARD, my_hdf5_error_handler, client_data);
  H5Eclose_stack(estack_id);
  Rcpp::stop("hdf Error");
  return 0;
  
}
// [[Rcpp::plugins(hdf5)]]
// [[Rcpp::export]]
void h5createDataset1(std::string h5file
                        , std::string ds_name
                        , std::vector<int> dims
                        , std::string storage_mode
                        , std::vector<int> chunk_dims, int compressor, int nLevel)
{  
  hid_t fileid = H5Fopen(h5file.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
  /* Create the data space for the 2d mat. */

  hsize_t _dims[2];
  for(int i = 0; i < 2; i++)
    _dims[i] = dims[i];
  hid_t dataspace = H5Screate_simple(2, _dims, NULL);
  
  hid_t dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
  hid_t status;
  unsigned        filter_config;
      
  if(compressor == 1)
  {
    const unsigned int cd_values[1] = {nLevel};     /* lz4 default is 3 */
    status = H5Pset_filter (dcpl_id, H5Z_FILTER_LZ4, H5Z_FLAG_MANDATORY, (size_t)1, cd_values);
    /* 
     * Check that filter is registered with the library now.
     * If it is registered, retrieve filter's configuration. 
     */
    // htri_t avail = H5Zfilter_avail(H5Z_FILTER_LZ4);
    // if (avail) {
    //   status = H5Zget_filter_info (H5Z_FILTER_LZ4, &filter_config);
    //   if ( (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) && 
    //        (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) 
    //     printf ("lz4 filter is available for encoding and decoding.\n");
    // }  
  }
  else if(compressor == 2)
    status = H5Pset_deflate (dcpl_id, nLevel);//default zlib/gzip
  
  //set it to use chunking
  hsize_t _chunk_dims[2];
  for(int i = 0; i < 2; i++)
    _chunk_dims[i] = chunk_dims[i];
  H5Pset_chunk(dcpl_id, 2, _chunk_dims);
  
  
   
  
  /* Create the 2d mat. */
  hsize_t dataset = H5Dcreate2(fileid, ds_name.c_str(), H5T_IEEE_F32LE_g, dataspace,
                       H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
  H5Pclose(dcpl_id);
  H5Dclose(dataset);
  
  H5Sclose(dataspace);

  H5Fclose(fileid);
  
}

// [[Rcpp::export]]
bool h5write1(Rcpp::NumericMatrix data, std::string filename, std::string ds_name, std::vector<int> colIndx) {

	double *mat = REAL(data.get__());

	int nCol = colIndx.size();
  int nRow = data.rows();

    
	/*
	 * Open the file and the dataset.
	 */

	herr_t      status;
	hid_t  fileid, dataset,dataspace, memspace;
	H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)custom_print_cb, NULL);
	
	
	fileid = H5Fopen(filename.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
	dataset = H5Dopen2(fileid, ds_name.c_str(), H5P_DEFAULT);
	dataspace = H5Dget_space(dataset);    /* dataspace handle */
	

		/*
		 * Define the memory dataspace.
		 */
		hsize_t 	dimsm[2]; //dimenstions
		dimsm[0] = nCol;
		dimsm[1] = nRow;
		memspace = H5Screate_simple(2,dimsm,NULL);

		/*
		 * Define hyperslab in the dataset.
		 */
		hsize_t      count[2];              /* size of the hyperslab in the file */
		hsize_t      offset[2];             /* hyperslab offset in the file */
		hsize_t      count_in[2];          /* size of the hyperslab in memory */
		hsize_t      offset_in[2];         /* hyperslab offset in memory */

		/*
		 * write subsets
		 */
		int colStart = colIndx[0] -1; //convert from R to C indexing
		offset[0] = colStart; //start from colStart-th channel
		offset[1] = 0; //start from the first event

		count[0]  = nCol;
		count[1]  = nRow; //get all events


		status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
		/*
		 * Define memory hyperslab.
		 */
		offset_in[0] = 0;//start from 1st column
		offset_in[1] = 0;//start from 0th event


		count_in[0]  = nCol;
		count_in[1]  = nRow; //all events

		status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_in, NULL, count_in, NULL);
		/*
		 * write data to hyperslab in the file from memory .
		 */
		status = H5Dwrite(dataset, H5T_NATIVE_DOUBLE, memspace, dataspace, H5P_DEFAULT, mat);


	/*
	 * Close/release resources.
	 */
	H5Dclose(dataset);

	H5Sclose(dataspace);
	H5Sclose(memspace);

	H5Fclose(fileid);




	return status >= 0;


}

// [[Rcpp::export]]
NumericVector h5read1(std::string filename, std::string ds_name, std::vector<int> colIndx) 
{
     
	
	herr_t      status;
	
	
	hid_t fileid = H5Fopen(filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
	hid_t dataset = H5Dopen2(fileid, "data", H5P_DEFAULT);
	hid_t dataspace = H5Dget_space(dataset);    /* dataspace handle */
	
	hsize_t dims[2];
	status  = H5Sget_simple_extent_dims(dataspace, dims, NULL); //get dimensions of datset
	unsigned nTotal = dims[0];//get total number of cols
	unsigned nrow = dims[1];//get total number of cols
	
	
	
	/*
	 * Define the memory dataspace.
	 */
	int ncol = colIndx.size();
	Rcpp::NumericVector mat(nrow * ncol);
	double * data_out = REAL(mat.get__());
	hsize_t 	dimsm[2]; //dimenstions
	dimsm[0] = ncol;
	dimsm[1] = nrow;
	hid_t memspace = H5Screate_simple(2,dimsm,NULL);
	
	
	/*
	 * Define hyperslab in the dataset.
	 */
	hsize_t      count[2];              /* size of the hyperslab in the file */
	hsize_t      offset[2];             /* hyperslab offset in the file */
	hsize_t      count_out[2];          /* size of the hyperslab in memory */
	hsize_t      offset_out[2];         /* hyperslab offset in memory */
	
	int i;
	for(i = 0; i < ncol; i++){
	  int colStart = colIndx.at(i) - 1;
	  if(colStart >= nTotal)
	    Rcpp::stop("H5read error!col index exceeds the boundary.");
	  offset[0] = colStart; //start from colStart-th channel
	  offset[1] = 0; //start from the first event
	  
	  count[0]  = 1;//get one channel
	  count[1]  = nrow; //get all events
	  
	  
	  status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL,
                                count, NULL);
	  
	  
	  /*
	   * Define memory hyperslab.
	   */
	  offset_out[0] = i;//start from ith column
	  offset_out[1] = 0;//start from 0th event
	  
	  count_out[0]  = 1;//one channel
	  count_out[1]  = nrow; //all events
	  status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_out, NULL,
                                count_out, NULL);
	  
	  /*
	   * Read data from hyperslab in the file into the hyperslab in
	   * memory .
	   */
	  status = H5Dread(dataset, H5T_NATIVE_DOUBLE, memspace, dataspace,
                    H5P_DEFAULT, data_out);
	  
	}
	

  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Sclose(memspace);
  H5Fclose(fileid);
  
  Rcpp::IntegerVector rdims(2);
  rdims[0] = nrow;
  rdims[1]=  ncol;
  mat.attr("dim") = rdims;
  return mat;
}

// [[Rcpp::export]]
void h5read2(std::string filename, std::string ds_name, std::vector<int> src_colIndx
                        , NumericMatrix mat, std::vector<int> dest_colIndx) 
{
  
  
  herr_t      status;
  
  
  hid_t fileid = H5Fopen(filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
  hid_t dataset = H5Dopen2(fileid, "data", H5P_DEFAULT);
  hid_t dataspace = H5Dget_space(dataset);    /* dataspace handle */
	  
	  hsize_t dims[2];
	  status  = H5Sget_simple_extent_dims(dataspace, dims, NULL); //get dimensions of datset
	  unsigned nTotal = dims[0];//get total number of cols
	  unsigned nrow = dims[1];//get total number of cols
	  
	  double * data_out = REAL(mat.get__());
	  
	  
	  /*
	  * Define the memory dataspace.
	  */
	  int ncol = src_colIndx.size();
	  
	  
	  hsize_t 	dimsm[2]; //dimenstions
	  dimsm[0] = mat.ncol();
	  dimsm[1] = mat.nrow();
	  // Rcout <<  dimsm[0] << std::endl;
	  hid_t memspace = H5Screate_simple(2,dimsm,NULL);
	  if(dimsm[1] != nrow)
	    Rcpp::stop("src and dest do not have the same nrow!");
	  /*
	  * Define hyperslab in the dataset.
	  */
	  hsize_t      count[2];              /* size of the hyperslab in the file */
	  hsize_t      offset[2];             /* hyperslab offset in the file */
	  hsize_t      count_out[2];          /* size of the hyperslab in memory */
	  hsize_t      offset_out[2];         /* hyperslab offset in memory */
	  // double * data_out;
	  int i;
	  for(i = 0; i < ncol; i++){
	    int colStart = src_colIndx.at(i) - 1;
	    int dest_colStart = dest_colIndx.at(i) - 1;
	    if(colStart >= nTotal)
	      Rcpp::stop("H5read error!col index exceeds the src boundary.");
	    if(dest_colStart >= dimsm[0])
	      Rcpp::stop("H5read error!col index exceeds the dest boundary.");
	    
	    offset[0] = colStart; //start from colStart-th channel
	    offset[1] = 0; //start from the first event
	    
	    count[0]  = 1;//get one channel
	    count[1]  = nrow; //get all events
	    
	    
	    status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL,
                                  count, NULL);
	    
	    
	    /*
	    * Define memory hyperslab.
	    */
	    // if(nrow == mat.nrow())//no row subsetting requested
	    // {
	      // data_out = &(mat[nrow*dest_colStart]);// use dest as output buffer directly
	    // }
	    // else
	    // {
	    //   
	    //   data_out = ;//otherwise,create the temp buffer to receive the data
	    // }
	    //  
	    // Rcout << dest_colStart << std::endl;
	    offset_out[0] = dest_colStart;//start from ith column
	    offset_out[1] = 0;//start from 0th event
	    
	    count_out[0]  = 1;//one channel
	    count_out[1]  = nrow; //all events
	    status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_out, NULL,
                                  count_out, NULL);
	    
	    /*
	     * Read data from hyperslab in the file into the hyperslab in
	     * memory .
	     */
	    status = H5Dread(dataset, H5T_NATIVE_DOUBLE, memspace, dataspace,
                      H5P_DEFAULT, data_out);
	    
	  }
	  
	  
	  H5Dclose(dataset);
	  H5Sclose(dataspace);
	  H5Sclose(memspace);
	  H5Fclose(fileid);
	  
	  
}
