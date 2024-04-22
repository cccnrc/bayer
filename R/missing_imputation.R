#' Operate missing imputation on a data.frame object
#'
#' This function operates a missing imputation using mice::futuremice and will return
#' an imputed data.frame.
#' It is important to specify "id" column name as this will significantly slow down the analysi
#' if not passed. "table" allows to specify a path to write down the imputed data.frame
#'
#' @param data: the input data.frame to perform analysis on
#' @param id: column name of the ID column. This is important to indicate to the function as imputing on ID column will significantly slow down the imputation. (optional)
#' @param table: the full path of the file to write down the iumputed table (optional)
#' @param table_sep: the field separator to use for the file to write down the iumputed table (defaults to '\t')
#' @param cores: the number of processor cores to parallelize analysis. If null the function will use all available cores (optional)
#' @param imp_num: The number of desired imputated datasets, "m" mice::futuremice() option (defaults to 5)
#' @param seed: A scalar to be used as the seed value for the mice algorithm, "seed" mice::futuremice() option (optional)
#' @param maxit: A scalar giving the maximum number of iterations, "maxit" mice::futuremice() option (defaults to 500)
#' @param parallelseed: A scalar to be used to obtain reproducible results, "parallelseed" mice::futuremice() option (optional)
#' @return A data.frame object with imputed missing values. The ID column (if passed) will be the first column in the returned data.frame
#' @export
missing_imputation <- function(
  data,
  id = NULL,
  table = NULL,
  table_sep = '\t',
  cores = NULL,
  imp_num = 5,
  seed = NA,
  maxit = 50,
  parallelseed = 500 ){
  DB_INPUT <- data
  if ( base::is.null(id) ) {
    base::cat( ' \n ID column not indicated in the data.frame. If an ID column is present this will significantly slow down the imputation. Please check. \n ' )
    base::warning( ' ID column not indicated in the data.frame. If an ID column is present this will significantly slow down the imputation. Please check. \n ' )
    DBI <- DB_INPUT
  } else {
    if ( id %in% base::colnames( DB_INPUT ) ) {
      id_index <- base::which( base::colnames( DB_INPUT ) == id )
      DBI <- DB_INPUT[, -id_index ]
      base::cat( ' \n specified ID column: "', id, '" found in the data frame. Removing from imputation. \n ', sep = '' )
    } else {
      base::stop( ' \n specified ID column: "', id, '" not found in the data frame. Please check. \n ', sep = '' )
    }
  }
  imputed_col_num <- base::length( base::colnames(DBI) )
  base::cat( ' - imputing data frame on ', imputed_col_num, ' columns ... \n ', sep = '' )
  ### if user did not specified cores use all
  if ( base::is.null(cores) ) {
    cores <- parallel::detectCores()
  }
  base::suppressWarnings(IDB <- mice::futuremice( DBI, seed = seed, maxit = maxit, parallelseed = parallelseed, n.core = cores ))
  DBIM <- mice::complete( IDB )
  ### if ID removed now reattach it
  if ( ! base::is.null(id) ) {
    DB1 <- cbind( DB_INPUT[,id_index], DBIM )
    colnames( DB1 )[1] <- 'ID'
  } else {
    DB1 <- DBIM
  }
  ### if user asked write down imputed table
  if ( ! base::is.null(table) ) {
    write.table( DB1, table, sep=table_sep, row.names=F, quote=F )
  }
  return(DB1)
}
