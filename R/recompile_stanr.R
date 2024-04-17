#' Convert a model compiled without "rstan" to "rstan"
#'
#' This function recompile a model compiled without "stanr" default compiler
#' with "stanr", in order to speed up get_bayes_factor()
#'
#' @param model: a brms::brm() model
#' @return A brms::brm() model compiled with the default compiler "stanr"
recompile_stanr <- function( model )
{
  if ( model$backend == "rstan" ) {
    stop( '\n model already compiled with "stanr" \n' )
  } else {
    cat( '\n recompiling model originally compiled with "', model$backend, '" with "stanr" \n - this will take a while ... \n\n', sep = '' )
  }
  ### recompile model with default "rstan" and no sampling
  OUT <- utils::capture.output(model_stanr <- brms::brm(formula = as.formula(model$formula),
                                                          prior = model$priors,
                                                          data = model$data,
                                                          family = model$family,
                                                          chains = 0,
                                                          iter = 0,
                                                          save_pars = brms::save_pars( all = TRUE ),
                                                          control = model$stan_args$control
                                                      ))
  return( model_stanr )
}
