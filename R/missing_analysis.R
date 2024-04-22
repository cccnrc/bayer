#' Operate a summary of missing in the data.frame object passed
#'
#' This function operates a summary of missing distribution in the data.frame object passed
#' and returns a naniar::gg_miss_upset() plot showing missing distribution
#'
#' @param data: the input data.frame to perform analysis on
#' @param plot: naniar plot returned (defaults to FALSE)
#' @param stdout: summary printed to stdout (defaults to TRUE)
#' @return A naniar::gg_miss_upset() plot of the missing distribution in the database
#' @export
missing_analysis <- function( data, plot = FALSE, stdout = TRUE ){
  DB_INPUT <- data
  ### get total number of NA
  NA_TOT <- base::sum( base::is.na( DB_INPUT ) )
  if ( NA_TOT == 0 ) {
    base::stop(' no missing in data.frame \n')
  }
  ### get number of rows with any NA
  NA_ROW_TOT <- base::length( base::which( base::rowSums( base::is.na(DB_INPUT))>0))
  ### get row numbers with any NA
  NA_ROW_INDEX <- base::which(base::rowSums(base::is.na(DB_INPUT))>0)
  ### get number of columns with any NA
  NA_COL_TOT <- base::length(base::which(base::colSums(base::is.na(DB_INPUT))>0))
  ### get column numbers with any NA
  NA_COL_INDEX <- base::colnames(DB_INPUT)[base::which(base::colSums(base::is.na(DB_INPUT))>0)]
  if ( base::isTRUE(stdout) ) {
    base::cat('-----------------------------------------\n')
    base::cat('  --> total missing (num):\t', NA_TOT ,'\n')
    base::cat('  --> total missing (row):\t', NA_ROW_TOT ,'\n')
    base::cat('     --> row missing (index):\t', base::paste(NA_ROW_INDEX, collapse = ' - ') ,'\n')
    base::cat('  --> total missing (col):\t', NA_COL_TOT ,'\n')
    base::cat('     --> col missing (names):\t', base::paste(NA_COL_INDEX, collapse = ' - ') ,'\n')
    base::cat('-----------------------------------------\n')
  }
  if ( base::isTRUE(plot) ) {
    ### plot missing
    PM <- naniar::gg_miss_upset(DB_INPUT, nsets = naniar::n_var_miss(DB_INPUT))
    return(PM)
  }
}
