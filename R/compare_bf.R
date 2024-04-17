#' Extract bayes factor directly comparing model compiled with "rstan" and faster sampling with non-rstan
#'
#' This function extract model compiled with "rstan" and sampling with other mothods
#' to immediately extract bayes factor
#'
#' @param model: a brms::brm() model compiled without "rstan"
#' @param model_stanr: same "model" compiled with "rstan"
#' @param model_null: a brms::brm() null model compiled without "rstan"
#' @param model_stanr: same "model_null" compiled with "rstan"
#' @param silent: brms::bayes_factor() option
#' @param verbose: brms::bayes_factor() option
#' @param log: brms::bayes_factor() option
#' @return A brms::bayes_factor() object from models comparison
compare_bf <- function( model, model_stanr, model_null, model_null_stanr, silent = TRUE, verbose = FALSE, log = FALSE )
{
  if ( as.character( model$formula )[1] != as.character( model_stanr$formula )[1] ) {
    stop( '\n ---- model and model_stanr have different formulas! Cannot compare \n' )
  }
  if ( as.character( model_null$formula )[1] != as.character( model_null_stanr$formula )[1] ) {
    stop( '\n ---- model_null and model_null_stanr have different formulas! Cannot compare \n' )
  }
  if ( model_stanr$backend != 'rstan' ) {
    base::stop( '\n -- model_stanr was compiled with a backend= which is not "rstan" \n')
  }
  if ( model_null_stanr$backend != 'rstan' ) {
    base::stop( '\n -- model_null_stanr was compiled with a backend= which is not "rstan" \n')
  }
  ### extract bayes factor
  OUT <- utils::capture.output(BF <- brms::bayes_factor(
                                        bridgesampling::bridge_sampler(
                                          samples = model$fit,
                                          stanfit_model = model_stanr$fit
                                        ),
                                        bridgesampling::bridge_sampler(
                                          samples = model_null$fit,
                                          stanfit_model = model_null_stanr$fit
                                        ), silent = silent, verbose = verbose, log = log )
  )
  return( BF )
}
