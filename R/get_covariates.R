#' Extract covariates from a regression formula
#'
#' This function extract covriates from a regression formula: y ~ x1 + x2 + x3
#'
#' @param formula: string of the regression formula
#' @return A vector with covatiates of the formula
get_covariates <- function( formula )
{
  formula <- base::as.character( formula )[1]
  ### check if formula contains a single ~
  outcome_delimiter_num <- stringr::str_count( formula, "\\~" )
  if ( outcome_delimiter_num > 1 ) {
    base::stop('\n formula contains more than one "~" \n')
  } else if ( outcome_delimiter_num == 0 ) {
    base::stop('\n formula does not contain any "~" \n')
  }
  formula_split_0 <- base::unlist(base::strsplit( formula, "\\~" ))
  formula_outcome <- formula_split_0[1]
  formula_covariates <- formula_split_0[2]
  ### check if formula contains multiple covariates
  covariate_delimiter_num <- stringr::str_count( formula_covariates, "\\+" )
  if ( covariate_delimiter_num > 0 ) {
    ### split covariates (if multiple)
    formula_covariates_vector <- base::unlist( base::strsplit( formula_covariates, "\\+" ) )
  } else {
    formula_covariates_vector <- formula_covariates
  }
  ### trim whitespaces from single covariate
  cov_trim_vector <- base::vector()
  for ( i in 1:base::length( formula_covariates_vector ) )
  {
    cov_input <- formula_covariates_vector[i]
    cov_trim <- base::gsub("[[:space:]]", "", cov_input)
    cov_trim_vector <- base::c( cov_trim_vector, cov_trim )
  }
  return( cov_trim_vector )
}
