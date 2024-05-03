#' Extract and transform regression coefficients for a variable of interest in the model
#'
#' This function extract and transform regression coefficients for a variable of interest
#' within a brms::fit() model
#'
#' @param model: a brms::brm() fitted model
#' @param var: the column name of the variable of interest. It must be a variable passed in the model
#' @param multinomial_level: the level of interest in the outcome variable if a multinomial model was fitted (optional)
#' @return A list() object composed of "$coef": the variable coefficient (transformed); "$err": the variable coefficien error (trnasformed); "$low95": the variable coefficient low 95% CI (transformed); "$high95": the variable coefficient high 95% CI (transformed)
#' @export
extract_coeff <- function(
  model,
  var,
  multinomial_level = NULL
 )
{
  multinomial_reference <- multinomial_level
  ### extract coefficients tab
  model_tab <- brms::fixef( model )
  ### extract variable of interest category passed
  if ( ! var %in% base::colnames( model$data ) ) {
    base::stop('\n ', var, ' not found in model \n', sep = '')
  }
  VV0 <- model$prior$coef
  var_index <- 0
  for ( i in 1:length(VV0) )
  {
    if ( base::grepl( var, VV0[i], fixed = TRUE ) ) {
      var_index <- i
    }
  }
  var_factor <- base::gsub( var, "", VV0[ var_index ] )
  ### if family is multinomial some differences apply
  if ( model$family$family == 'multinomial' ) {
    ### if user did not specify a level interested in use the first you get
    if ( base::is.null(multinomial_reference) ) {
      prior_prefix <- base::paste( model$prior$dpar[ var_index ], '_', sep = '' )
      base::warning( '\n no level specified for a multinomial model. Using: "', base::gsub( "mu", "", model$prior$dpar[ var_index ] ), '" to plot posterior results \n  - tip: use "multinomial_level" option to plot result for a specific factor \n' )
      multinomial_reference_used <- base::gsub( "mu", "", model$prior$dpar[ var_index ] )
    } else {
      if ( multinomial_reference %in% base::unique(base::gsub( "mu", "", model$prior$dpar )) ) {
        prior_prefix <- base::paste( 'mu', multinomial_reference, '_', sep = '' )
        multinomial_reference_used <- multinomial_reference
      } else {
        base::stop( '\n multinomial_level specified: "', multinomial_reference, '" not found in model \n  - tip: either you typed it wrong or it was used as reference level \n' )
      }
    }
  } else {
    prior_prefix <- base::paste( model$prior$class[ var_index ], '_', sep = '' )
  }
  ### if var actually had a factor appended
  if ( base::nchar(var_factor) > 0 ) {
    if ( model$family$family == 'multinomial' ) {
      var_tab_name <- base::paste( prior_prefix, var, var_factor, sep = '' )
    } else {
      var_tab_name <- base::paste( var, var_factor, sep = '' )
    }
  } else {
    var_tab_name <- var
  }
  ### extract from model coefficients
  if ( ! var_tab_name %in% base::rownames( model_tab ) ) {
    base::stop( var, ' not found in model, please check!' )
  }
  model_tab_var <- model_tab[ base::which( base::rownames( model_tab ) == var_tab_name ), ]
  var_est_coef <- model_tab_var[['Estimate']]
  var_err_coef <- model_tab_var[['Est.Error']]
  var_q1_coef <- model_tab_var[['Q2.5']]
  var_q3_coef <- model_tab_var[['Q97.5']]
  ### present results based on model family and link function
  if ( model$family$family == 'cox' & model$family$link == 'log' ) {
    base::cat( '\n - formula:\t', base::as.character(model$formula)[1], '\n', sep = '' )
    base::cat( ' - family:\t', model$family$family, ' (', model$family$link, ')', '\n', sep = '' )
    base::cat( ' - variable:\t', var, '\n', sep = '' )
    base::cat( ' - hazard r.:\t', base::exp(var_est_coef), '\n', sep = '' )
    base::cat( ' - low 95% CI:\t', base::exp(var_q1_coef), '\n', sep = '' )
    base::cat( ' - high 95% CI:\t', base::exp(var_q3_coef), '\n\n', sep = '' )
  } else if ( model$family$link == 'logit' ) {
    base::cat( '\n - formula:\t', base::as.character(model$formula)[1], '\n', sep = '' )
    base::cat( ' - family:\t', model$family$family, ' (', model$family$link, ')', '\n', sep = '' )
    base::cat( ' - variable:\t', var, '\n', sep = '' )
    if ( model$family$family == 'multinomial' ) {
      base::cat( ' - reference:\t', multinomial_reference_used, '\n', sep = '' )
    }
    base::cat( ' - odds r.:\t', base::exp(var_est_coef), '\n', sep = '' )
    base::cat( ' - low 95% CI:\t', base::exp(var_q1_coef), '\n', sep = '' )
    base::cat( ' - high 95% CI:\t', base::exp(var_q3_coef), '\n\n', sep = '' )
  } else if ( model$family$link == 'identity' ) {
    base::cat( '\n - formula:\t', base::as.character(model$formula)[1], '\n', sep = '' )
    base::cat( ' - family:\t', model$family$family, ' (', model$family$link, ')', '\n', sep = '' )
    base::cat( ' - variable:\t', var, '\n', sep = '' )
    base::cat( ' - coeff:\t', var_est_coef, '\n', sep = '' )
    base::cat( ' - low 95% CI:\t', var_q1_coef, '\n', sep = '' )
    base::cat( ' - high 95% CI:\t', var_q3_coef, '\n\n', sep = '' )
  } else if ( model$family$family == 'poisson' & model$family$link == 'log' ) {
    base::cat( '\n - formula:\t', base::as.character(model$formula)[1], '\n', sep = '' )
    base::cat( ' - family:\t', model$family$family, ' (', model$family$link, ')', '\n', sep = '' )
    base::cat( ' - variable:\t', var, '\n', sep = '' )
    base::cat( ' - risk r.:\t', base::exp(var_est_coef), '\n', sep = '' )
    base::cat( ' - low 95% CI:\t', base::exp(var_q1_coef), '\n', sep = '' )
    base::cat( ' - high 95% CI:\t', base::exp(var_q3_coef), '\n\n', sep = '' )
  }
  out_list <- list( 'coef' = var_est_coef, 'err' = var_err_coef, 'low95' = var_q1_coef, 'high95' = var_q3_coef )
  return( out_list )
}
