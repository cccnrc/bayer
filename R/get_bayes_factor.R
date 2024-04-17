#' Extract bayes factor comparing a model and the same model without the variable of interest
#'
#' This function extract bayes factor through brms::bayes_factor() comparing a model with the
#' variable of interest and a null model without the variable of interest.
#' To speed up the process and avoid re-sampling using default compiler "stanr", if sampling
#' was operated with "cmdstanr", a compiled but not sampled "stanr" model can be passed to
#' skip this passage. Otherwise the function will compile it for you.
#' User can also simply pass the name of the variable of interest and the function will operate
#' all the rest. But this will take significantly longer as model_null need to be compiled and
#' sampled. Same for model_null_stanr.
#'
#' @param model: a brms::brm() model compiled without "rstan"
#' @param model_stanr: same "model" compiled with "rstan"
#' @param model_null: a brms::brm() null model compiled without "rstan"
#' @param model_stanr: same "model_null" compiled with "rstan"
#' @param var: instead of passing model and model_null the variable of interest can be passed. This will take longer as model_null will be compiled
#' @param silent: brms::bayes_factor() option
#' @param verbose: brms::bayes_factor() option
#' @param log: brms::bayes_factor() option
#' @return A list object composed of: 'bf': brms::bayes_factor() object; 'rstan': model compiled with rstan; 'null': model_null brms::brm() object; 'null_rstan': model_null compiled with rstan
#' @export
get_bayes_factor <- function(
                        model,
                        model_null = NULL,
                        model_stanr = NULL,
                        model_null_stanr = NULL,
                        var = NULL,
                        silent = TRUE,
                        verbose = FALSE,
                        log = FALSE
                      )
{
  OUT_LIST <- list( 'bf' = NULL, 'rstan' = NULL, 'null' = NULL, 'null_rstan' = NULL )
  ### check that user passed model_null or var
  if ( base::is.null( model_null ) & base::is.null( var ) ) {
    base::stop('\n Either "model_null" or "var" must be specified \n')
  }
  ### if user did not pass the null model or model_stanr or model_null_stanr
  if ( base::is.null( model_null ) | base::is.null( model_stanr ) | base::is.null( model_null_stanr ) ) {
    ### if var not passed (e.g. it passed model_null directly but not model_null_stanr or model_stanr)
    if ( base::is.null( var ) ) {
      ### extract variable comparing model and model_null formula
      model_formula <- base::as.character( model$formula )[1]
      model_formula_covariates <- get_covariates( model_formula )
      ### if model_null is passed just compile rstan and null_rstan if needed
      if ( ! base::is.null( model_null ) ) {
        ### extract variable comparing model and model_null formula
        model_null_formula <- base::as.character( model_null$formula )[1]
        model_null_formula_covariates <- get_covariates( model_null_formula )
        var <- base::setdiff( model_formula_covariates, model_null_formula_covariates )
        ### check it was actually a single covariate differing between model and model_null
        if ( base::length(var) > 1 ) {
          stop(' "model" and "model_null" covariates differ for more than a single element: ', paste( var, collapse = " - " ), '\n', sep = '' )
        }
        ### compile rstan if not passed
        if ( base::is.null( model_stanr ) ) {
          if ( model$backend != 'rstan' ) {
            model_stanr <- recompile_stanr( model )
          } else {
            model_stanr <- model
          }
        }
        ### compile null_rstan if not passed
        if ( base::is.null( model_null_stanr ) ) {
          if ( model$backend != 'rstan' ) {
            model_null_stanr <- recompile_stanr( model_null )
          } else {
            model_null_stanr <- model_null
          }
        }
      }
    ### if model null is not passed compile it
    } else if ( base::is.null( model_null ) ) {
      null_compiled <- compile_null_model( model = model, var = var, stanr = TRUE, model_stanr = model_stanr, model_null_stanr = model_null_stanr )
      model_null <- null_compiled[['null']]
      model_stanr <- null_compiled[['stanr']]
      model_null_stanr <- null_compiled[['null_stanr']]
    }
  }
  ### make fast comparison using stanr compiling and non-stanr sampling
  BF <- compare_bf( model, model_stanr, model_null, model_null_stanr, silent = silent, verbose = verbose, log = log )
  out_list <- list( 'bf' = BF, 'rstan' = model_stanr, 'null' = model_null, 'null_rstan' = model_null_stanr )
  return( out_list )
}
