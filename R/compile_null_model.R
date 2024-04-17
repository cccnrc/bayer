#' Compile a null model taking out a variable of interest in the passed model
#'
#' This function compile and sample a null model which is a brms::brm() identical to the passed "model"
#' without the variable of interest ("var"), in order to use the null model for further comparison and
#' identify the actual role of the variable of interest in model accuracy and power.
#'
#' @param model: a brms::brm() fitted model
#' @param var: instead of passing model and model_null the variable of interest can be passed. This will take longer as model_null will be compiled (optional)
#' @param stanr: if the model was compiled with a different backend option than default "rstan", if set to "TRUE" another model and model_null will be compiled with "rstan". This is important to speed up further comparison as in case of get_bayes_factor() (defaults to FALSE)
#' @param model_stanr: user can directly pass the model compiled with "rstan" (optional)
#' @param model_stanr: user can directly pass the model_null compiled with "rstan" (optional)
#' @param stdout: print a summary of comparison to stdout (default to TRUE)
#' @return A list() object composed of: 'stanr': the passed model compiled with rstan (if asked); 'null': model_null brms::brm() fitted object; 'null_rstan': model_null compiled with rstan (if asked)
compile_null_model <- function(
  model,
  var,
  stanr = FALSE,
  model_stanr = NULL,
  model_null_stanr = NULL,
  stdout = FALSE )
{
  base::cat( '\n - comparing model with a null model without variable:\t', var, ' \n', sep = '' )
  model_formula <- base::as.character( model$formula )[1]
  model_formula_covariates <- get_covariates( model_formula )
  ### extract outcome
  formula_split_0 <- base::unlist(base::strsplit( model_formula, "\\~" ))
  formula_outcome <- formula_split_0[1]
  ### check var is in formula covariates and extract it
  if ( var %in% model_formula_covariates ) {
    null_model_formula_covariates <- model_formula_covariates[ - base::which( model_formula_covariates == var ) ]
    if ( base::length(null_model_formula_covariates) < 1 ) {
      null_model_formula_covariates <- '1'
    }
    model_null_formula <- base::paste( formula_outcome, '~', base::paste( null_model_formula_covariates, collapse = ' + ' ), sep = ' ' )
  } else {
    base::stop('\n - variable: ', var, ' not found in model formula. \n')
  }
  ### if stanr model is needed compiled it
  if ( base::isTRUE(stanr) ) {
    ### if user specified "cmdstanr" as model backend fasten up the sampling: https://discourse.mc-stan.org/t/how-to-do-bridge-sampling-and-calculate-bayes-factor-with-brms-with-cmdstanr-backend/18873/10
    if ( model$backend != 'rstan' ) {
      ### if user did not pass model_stanr directly
      if ( base::is.null(model_stanr) ) {
        base::cat( ' --- model seems to be compiled with "cmdstanr", recompiling input model with "rstan" and sampling with "cmdstanr" to speed up \n\n' )
        ### recompile model with default "rstan" and no sampling
        model_stanr <- recompile_stanr( model )
      } else {
        ### check it was actually compiled with rstan
        if ( model_stanr$backend != 'rstan' ) {
          base::stop( '\n -- model_stanr was compiled with a backend= which is not "rstan" \n')
        }
        ### check model and model_stanr have same exact formula
        if ( base::as.character( model$formula )[1] != base::as.character( model_stanr$formula )[1] ) {
          base::stop( '\n ---- model and model_stanr have different formulas! Cannot compare \n' )
        }
      }
    ### if model is already compiled with rstan just use it
    } else {
      model_stanr <- model
    }
  }
  ### if user specified threads model_null can be probably run in parallel
  cores <- base::getOption("mc.cores", 1)
  model_threads <- model$threads$threads
  brm_chains <- 4
  if ( ! base::is.null( model_threads ) ) {
    cores <- model_threads
    brm_chains <- model_threads
  }
  if ( model$backend != 'rstan' ) {
    OUT <- utils::capture.output(model_null <- brms::brm(formula = stats::as.formula(model_null_formula),
                                                          prior = model$priors,
                                                          data = model$data,
                                                          family = model$family,
                                                          chains = brm_chains,
                                                          cores = cores,
                                                          sig_figs = 10,
                                                          threads = brms::threading( model_threads ),
                                                          backend = model$backend,
                                                          save_pars = brms::save_pars( all = TRUE ),
                                                          control = model$stan_args$control
                                                      ))
  } else {
    OUT <- utils::capture.output(model_null <- brms::brm(formula = stats::as.formula(model_null_formula),
                                                          prior = model$priors,
                                                          data = model$data,
                                                          family = model$family,
                                                          chains = brm_chains,
                                                          cores = cores,
                                                          threads = brms::threading( model_threads ),
                                                          backend = model$backend,
                                                          save_pars = brms::save_pars( all = TRUE ),
                                                          control = model$stan_args$control
                                                      ))
  }
  ### if stanr model_null is needed compile it
  if ( base::isTRUE(stanr) ) {
    ### if user did pass model_null_stanr directly check it was compiled with rstan
    if ( ! base::is.null(model_null_stanr) ) {
      if ( model_null_stanr$backend != 'rstan' ) {
        base::stop( '\n -- model_null_stanr was compiled with a backend= option which is not "rstan" \n')
      }
    ### if user did not pass model_null_stanr check model_null backend and in case recompile
    } else {
      if ( model_null$backend != 'rstan' ) {
        model_null_stanr <- recompile_stanr( model_null )
      ### if model null is already compiled with stanr
      } else {
        model_null_stanr <- model_null
      }
    }
  }
  out_list <- base::list( 'null' = model_null, 'stanr' = model_stanr, 'null_stanr' = model_null_stanr )
}
