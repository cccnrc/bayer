#' Extract posterior name as passed to a brms::brm() fitted model for a variable of interest
#'
#' This function extract posterior distriution column name for a variable of interest given
#' characteristics (group, dpar, prefix, level)
#'
#' @param var_name: the column name of the variable of interest. It must be a variable passed in the model
#' @param var_factor: the level of interest of the variable passed as "var_name" (optional)
#' @param var_prior_prefix: the prior prefix of the variable passed as "var_name" (optional)
#' @param var_prior_dpar: the prior dpar of the variable passed as "var_name" (optional)
#' @param var_group: the group of the variable passed as "var_name" (optional)
#' @return The name of the variable as specified in the model
compose_fit_name <- function(
  var_name,
  model_fit_names,
  var_prior_prefix = NULL,
  var_prior_dpar = NULL,
  var_group = NULL,
  var_factor = NULL
)
{
  # cat( var_name, ' - factor: ', var_factor, ' - group: ', var_group, ' - dpar: ', var_prior_dpar, '\n', sep = '' )
  if ( ! base::is.null(var_factor)) {
    if ( base::nchar(var_factor) > 0 ) {
      var_name <- base::paste( var_name, var_factor, sep = '' )
    }
  }
  var_prefix <- var_prior_prefix
  if ( ! base::is.null(var_group)) {
    if ( base::nchar(var_group) > 0 ) {
      var_prefix <- base::paste( var_prior_prefix, '_', var_group, sep = '' )
    ### if no group and no dpar are passed, use prior_prefix and var only
    } else {
      if ( ! base::is.null(var_prior_dpar)) {
        if ( base::nchar(var_prior_dpar) == 0 ) {
          var_name <- base::paste( var_prior_prefix, '_', var_name, sep = '' )
          ### check results in model fit names
          var_found_index <- base::which(base::grepl( var_name, model_fit_names, fixed = TRUE ))
          if ( base::length(var_found_index) != 1  ) {
            stop( ' cannot find any (or multiple) name in model$fit corresponding to passed: ', var_name, '\n', sep = '')
          }
          return(model_fit_names[var_found_index])
        }
      }
    }
  }
  ### check results in model fit names
  var_found_index <- base::which(base::grepl( base::paste( '_', var_name, sep = '' ), model_fit_names, fixed = TRUE ))
  if ( base::length(var_found_index) < 1  ) {
    stop( ' cannot find any name in model$fit corresponding to passed: ', var_name, '\n', sep = '')
  }
  var_found <- model_fit_names[var_found_index]
  if ( ! base::is.null(var_prior_dpar)) {
    if ( base::nchar(var_prior_dpar) > 0 ) {
      var_found_index <- base::which(base::grepl( base::paste( '_', var_prior_dpar, '_', sep = ''), var_found, fixed = TRUE ))
      if ( base::length(var_found_index) < 1  ) {
        stop( ' cannot find any name in model$fit corresponding to passed: ', var_name, '\n', sep = '')
      }
      var_found <- var_found[var_found_index]
    }
  }
  var_found_index2 <- base::which(base::grepl( base::paste(var_prefix, '_', sep = ''), var_found, fixed = TRUE ))
  var_found2 <- var_found[var_found_index2]
  if ( base::length(var_found2) > 1 ) {
    stop( ' found multiple model$fit names associated with passed var: ', base::paste(var_found2, collapse = ' - '), '\n', sep = '' )
  }
  return(var_found2)
}
