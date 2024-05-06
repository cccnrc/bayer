#' Multinomial Regression Model Bayesian analysis
#'
#' This function compile a Multinomial model and returns a model with posterior probabilities
#' based on the outcome specified in the input data.frame. The model that is fitted will be
#' compiled as a brms::brm() brms::multinomial() family with several parameters
#' adaptable based on user preferences. Priors can be specified with "priors" option
#' and weighted regression can be performed thorugh "weights" option. To fasten up the
#' analysis "cmdstanr" package can be used, but it needs to be installed and configured.
#'
#'
#' @param data: the data.frame input object
#' @param outcome: the column name of the outcome variable (please note: it must be complete, no NA allowed)
#' @param reference: the reference level to use for the outcome variable (converted to 1) (optional)
#' @param covariates: vector of model covariate column names (optional)
#' @param cores: the number of processor cores to parallelize analysis. It requires cmdstanr installed and configured. brm recommends setting the ‘mc.cores’ option to be as many processors as the hardware and RAM allow (optional)
#' @param priors: the vector with prior probability distribution for the Bayesian analysis (optional). These are "brmsprior" objects created by "brms::set_prior()" or related functions and combined using the "c()" method or the "+" operator. See also "brms::default_prior" for more help. brm() prior option
#' @param weights: the column name of the weights (e.g. IPTW) to apply to the analysis (optional)
#' @param brm_chains: number of Markov chains used in brm() (defaults to 4)
#' @param brm_threads: number of threads to use in within-chain parallelization: brm() threads option (optional)
#' @param brm_sig_figs: number of decimal digits to store the posterior draws (defaults to 10)
#' @param brm_iter: number of total iterations per chain (including warmup; defaults to 2000): brm() iter option
#' @param brm_thin: thinning rate. Must be a positive integer. Set ‘thin > 1’ to save memory and computation time if "brm_iter" is large: brm() thin option
#' @param brm_warmup: a positive integer specifying number of warmup (aka burnin) iterations. This also specifies the number of iterations used for stepsize adaptation, so warmup draws should not be used for inference. The number of warmup should not be larger than "brm_iter" and the default is "brm_iter / 2": brm() warmup option
#' @param brm_seed: the seed for random number generation to make results reproducible. If NA (the default), "Stan" will set the seed randomly: brm() seed option
#' @param brm_adapt_delta: brm() control factor to regulate the number of divergent transitions that cause a bias in the obtained posterior draws. This should be generally 0.8 to 0.999: brm() control option (default to 0.999). See stan for more details
#' @param brm_max_treedepth: brm() control factor to regulate the number of divergent transitions that cause a bias in the obtained posterior draws. This should be generally >=10: brm() control option (defaults to 10). See stan for more details
#' @param print: print intermediate passages resume to stdout (defaults to FALSE)
#' @return A brms::brm model with Bayesian posterior draws
#' @export
bayer_multinomial <- function(
                  data,
                  outcome,
                  reference = NULL,
                  covariates = NULL,
                  cores = NULL,
                  priors = NULL,
                  weights = NULL,
                  brm_chains = NULL,
                  brm_threads = NULL,
                  brm_sig_figs = 10,
                  brm_iter = 2000,
                  brm_thin = 1,
                  brm_warmup = base::floor( brm_iter / 2 ),
                  brm_seed = NA,
                  brm_adapt_delta = 0.999,
                  brm_max_treedepth = 10,
                  print = FALSE )
{
  ### check event_var exists in data.frame
  if ( ! outcome %in% base::colnames(data) ) {
    base::stop( '\n ', outcome, 'not found in dataframe \n', sep = ' ' )
  }
  ### must remove all NAs from event_var
  data2 <- data[ !base::is.na(data[[ outcome ]]), ]
  data2_num <- base::nrow( data2 )
  data_num <- base::nrow( data )
  if ( base::isTRUE(stdout) ) {
    base::cat('  --> ', data_num - data2_num, ' samples removed due to missing in event variable' )
  }
  ### convert outcome
  data2[[ outcome ]] <- brms_outcome_var( data2[[ outcome ]], positive = reference, family = 'multinomial' )
  ### convert to matrix for multinomial in brm()
  outcome_matrix <- stats::model.matrix( ~ base::get(outcome) - 1,  data = data2 )
  base::colnames( outcome_matrix ) <- base::levels( data2[[outcome]] )
  #base::colnames( outcome_matrix ) <- base::paste( outcome, ':', base::levels( data2[[outcome]] ), sep = '' )
  outcome_matrix_colnames <- base::colnames( outcome_matrix )
  data2[[ outcome ]] <- base::with(data2, base::cbind(outcome_matrix))
  # data2 <- base::cbind( data2, outcome_matrix )
  # base::print( head(data2) )
  # base::print( data2[[outcome]] )
  ### if covariates not passed use 1
  if ( base::is.null( covariates ) ) {
    covariates <- '1'
  }
  FF1 <- riptw::get_formula(
                  outcome = outcome,
                  covariates = covariates,
                  weights = weights,
                  bayesian = TRUE
                )
  ### add trials as the multinomial formula requires them
  FF1 <- brms::update_adterms( FF1, ~ trials(1) )
  # base::print( FF1 )
  ### set model family
  brms_family <- 'multinomial'
  brms_family_link <- 'logit'
  ### check cmdstanr package is installed and properly configured
  check_cmdstanr_out <- check_cmdstanr_error()
  if ( "cmdstanr" %in% base::rownames( utils::installed.packages()) ) {
    if ( check_cmdstanr_out == 0 ) {
      ### if user did not specified cores use all
      if ( base::is.null(cores) ) {
        cores <- parallel::detectCores()
      }
      ### if user did not specified brm_chains use cores
      if ( base::is.null( brm_chains ) ) {
        brm_chains <- cores
      }
      ### if user did not specified brm_threads use cores
      if ( base::is.null( brm_threads ) ) {
        brm_threads <- cores
      }
      base::cat( '\n  "cmdstanr" is available in this R environment and seems properly configured \n  running in parallel with: ', cores, ' cores \n\n', sep = '' )
      OUT <- utils::capture.output(model <- brms::brm(formula = FF1,
                                                      prior = priors,
                                                      data = data2,
                                                      family = brms::brmsfamily( brms_family, brms_family_link),
                                                      chains = brm_chains,
                                                      cores = cores,
                                                      backend = "cmdstanr",
                                                      threads = brms::threading( brm_threads ),
                                                      sig_figs = brm_sig_figs,
                                                      thin = brm_thin,
                                                      iter = brm_iter,
                                                      warmup = brm_warmup,
                                                      seed = brm_seed,
                                                      save_pars = brms::save_pars( all = TRUE ),
                                                      control = base::list( adapt_delta = brm_adapt_delta, max_treedepth = brm_max_treedepth )
                                                  ))
    } else if ( check_cmdstanr_out == 1 ) {
      base::cat( '\n "cmdstanr" is available in this R environment but it does not seems to be properly configured \n - tip: did you ever run cmdstanr::install_cmdstan() ? \n proceeding without parallelization, this will take a while ... \n\n' )
      base::warning( 'cmdstanr does not seems to be properly configured \n' )
      ### if user did not specified cores use all
      if ( base::is.null(cores) ) {
        cores <- base::getOption("mc.cores", 1)
      }
      ### if user did not specified brm_chains use cores
      if ( base::is.null( brm_chains ) ) {
        brm_chains <- 4
      }
      ### if user did not specified brm_threads use cores
      if ( base::is.null( brm_threads ) ) {
        brm_threads <- base::getOption("brms.threads", NULL)
      }
      OUT <- utils::capture.output(model <- brms::brm(formula = FF1,
                                                    prior = priors,
                                                    data = data2,
                                                    family = brms::brmsfamily( brms_family, brms_family_link ),
                                                    chains = brm_chains,
                                                    cores = cores,
                                                    iter = brm_iter,
                                                    thin = brm_thin,
                                                    warmup = brm_warmup,
                                                    seed = brm_seed,
                                                    save_pars = brms::save_pars(all = TRUE),
                                                    control = base::list( adapt_delta = brm_adapt_delta, max_treedepth = brm_max_treedepth )
                                                  ))
    }
  } else {
    ### if user did not specified cores use all
    if ( base::is.null(cores) ) {
      cores <- base::getOption("mc.cores", 1)
    }
    ### if user did not specified brm_chains use cores
    if ( base::is.null( brm_chains ) ) {
      brm_chains <- 4
    }
    base::cat( '\n Warning message: \n  "cmdstanr" is not available in this R environment \n  proceeding without parallelization, this will take a while ... \n  - tip: install and confiure cmdstanr with cmdstanr::install_cmdstan() \n\n' )
    base::warning( 'cmdstanr is not available in this R environment \n\n' )
    OUT <- utils::capture.output(model <- brms::brm(formula = FF1,
                                                    prior = priors,
                                                    data = data2,
                                                    family = brms::brmsfamily( brms_family, brms_family_link ),
                                                    chains = brm_chains,
                                                    cores = cores,
                                                    iter = brm_iter,
                                                    thin = brm_thin,
                                                    warmup = brm_warmup,
                                                    seed = brm_seed,
                                                    save_pars = brms::save_pars(all = TRUE),
                                                    control = base::list( adapt_delta = brm_adapt_delta, max_treedepth = brm_max_treedepth )
                                                  ))
  }
  return( model )
}

check_cmdstanr_error <- function()
{
  base::tryCatch({
    file1 <- base::file.path( cmdstanr::cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
    out0 <- utils::capture.output( out1 <- cmdstanr::cmdstan_model(file1) )
    return(0)
  }, warning = function(w){
    return(2)
  }, error = function(e){
    return(1)
  })
}
