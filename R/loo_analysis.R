#' Calculates Leave-One-Out Cross-Validation (LOO) from a brms::brm() fitted model for a variable of interest in the model
#'
#' This function calculates LOO comparing a brms::brm() fitted model ("model") with the variable
#' of interest and a null model identical to the passed one without the variable of interest.
#' This function will also calculate Pareto K values to hel identify obsrvations with issues.
#' User can either pass the variable of interest name ("var") OR the null model directly ("model_null").
#' In this case, analysis will take longer as the null model needs to be compiled and sampled.
#'
#' Interpreting LOO results: When interpreting the results, a significant difference in LOO
#' (considering the uncertainty in the estimates) implies that one model provides a better fit
#' to the data than the other.
#' If the model with the variable of interest has a significantly lower LOO, it suggests that
#' including that variable improves the model's predictive accuracy. However, if there's no
#' clear difference, it may indicate that this specific variable doesn't contribute significantly
#' to the model in terms of predictive power.
#' If returned "mean" is negative it means "model" fit better.
#'
#' Interpret the Pareto k Values: The Pareto k values help you identify observations with potential issues.
#' The values are generally interpreted as follows:
#'   - k < 0.5: The observation does not pose any issue for LOO.
#'   - 0.5 ≤ k < 0.7: The observation might be slightly influential; interpret LOO results with caution.
#'   - 0.7 ≤ k < 1: The observation is highly influential and might affect the reliability of the LOO estimate.
#'   - k ≥ 1: The observation is very influential and the LOO results are likely unreliable for this observation.
#'
#'
#' @param model: a brms::brm() fitted model
#' @param model_null: a brms::brm() fitted model without the variable of interest (optional)
#' @param var: instead of passing model and model_null the variable of interest name can be passed. This will take longer as model_null will be compiled (optional)
#' @param stdout: print a summary of comparison to stdout (default to TRUE)
#' @return A list() object composed of "mean": the mean LOO difference value between model and model_nulll (if negative "model" fits better); "se": the standard error of the LOO value between model and model_nulll; "p": the p-value of LOO comparison between model and model_nulll
#' @export
loo_analysis <- function( model, model_null = NULL, var = NULL, stdout = TRUE )
{
  if ( base::is.null(model_null) & base::is.null(var) ) {
    base::stop('\n either "var" or "model_null" must be passed \n')
  }
  ### if user passed "var" and not model_null compile it (stanr not needed for this analysis)
  if ( base::is.null(model_null) ) {
    null_compiled <- compile_null_model( model = model, var = var, stanr = FALSE, model_stanr = NULL, model_null_stanr = NULL )
    model_null <- null_compiled[['null']]
  }
  MODEL_INPUT0 <- model
  MODEL_INPUT1 <- model_null
  ########## Leave-One-Out Cross-Validation (LOO)
  LOO0 <- brms::loo(MODEL_INPUT0)
  LOO0null <- brms::loo(MODEL_INPUT1)
  # Compare LOO
  LOO0_comparison <- brms::loo_compare(LOO0, LOO0null)
  # print(LOO0_comparison)
  ### check which model is in the last row
  last_row <- base::rownames(LOO0_comparison)[ base::length(base::rownames(LOO0_comparison)) ]
  ### if model null was on last row model is better if mean value is negative
  if ( last_row == 'MODEL_INPUT1' ) {
    better_model <- TRUE
  ### if model was on last row model is better if mean value is negative
  } else {
    better_model <- FALSE
  }
  # Calculate degrees of freedom
  NUM_OBS <- base::length(LOO0$diagnostics$pareto_k)
  COMP_DF <- NUM_OBS + NUM_OBS - 2
  # Difference in means and standard error
  COMP_MEAN <- base::abs(LOO0_comparison[[2]])
  COMP_SE <- base::abs(LOO0_comparison[[4]])
  # Calculate t-value
  COMP_T_VALUE <- COMP_MEAN / COMP_SE
  # Calculate p-value
  COMP_P_VALUE <- 2 * stats::pt( - base::abs(COMP_T_VALUE), COMP_DF)
  ### An important part is to compare Pareto K of each subject in order ot see if we have problems
  pareto_k_LOO0 <- LOO0$diagnostics$pareto_k
  pareto_k_LOO0null <- LOO0null$diagnostics$pareto_k
  PK_05 <- base::length( base::which( pareto_k_LOO0 >= 0.5 & pareto_k_LOO0 < 0.7 ))
  PK_05null <- base::length( base::which( pareto_k_LOO0null >= 0.5 & pareto_k_LOO0 < 0.7 ))
  PK_07 <- base::length( base::which( pareto_k_LOO0 >= 0.7 & pareto_k_LOO0 < 1 ))
  PK_07null <- base::length( base::which( pareto_k_LOO0null >= 0.7 & pareto_k_LOO0 < 1 ))
  PK_10 <- base::length( base::which( pareto_k_LOO0 > 1))
  PK_10null <- base::length( base::which( pareto_k_LOO0null > 1))
  PK_05_perc <- base::round( PK_05 / base::length( pareto_k_LOO0) * 100 , 2 )
  PK_07_perc <- base::round( PK_07 / base::length( pareto_k_LOO0) * 100 , 2 )
  PK_10_perc <- base::round( PK_10 / base::length( pareto_k_LOO0) * 100 , 2 )
  PK_05null_perc <- base::round( PK_05null / base::length( pareto_k_LOO0null) * 100 , 2 )
  PK_07null_perc <- base::round( PK_07null / base::length( pareto_k_LOO0null) * 100 , 2 )
  PK_10null_perc <- base::round( PK_10null / base::length( pareto_k_LOO0null) * 100 , 2 )
  ### return results based on which was the best model
  if ( base::isTRUE(better_model) ) {
    loo_mean <- LOO0_comparison[[2]]
  } else {
    loo_mean <- -LOO0_comparison[[2]]
  }
  if ( base::isTRUE( stdout ) ) {
    ### output summary
    base::cat( '----------------------------------------------------------------------------\n' )
    base::cat( '  -> LOO mean difference:\t', loo_mean, '\n', sep = '' )
    base::cat( '  -> LOO difference SE:\t\t', COMP_SE, '\n', sep = '' )
    base::cat( '  -> LOO difference P:\t\t', COMP_P_VALUE, '\n', sep = '' )
    base::cat( '  -> Pareto K 0.5 - 0.7:\t', PK_05, ' (', PK_05_perc, '%)\n', sep = '' )
    base::cat( '  -> Pareto K 0.7 - 1:\t\t', PK_07, ' (', PK_07_perc, '%)\n', sep = '' )
    base::cat( '  -> Pareto K over 1:\t\t', PK_10, ' (', PK_10_perc, '%)\n', sep = '' )
    base::cat( '  -> Null P-K 0.5 - 0.7:\t', PK_05null, ' (', PK_05null_perc, '%)\n', sep = '' )
    base::cat( '  -> Null P-K 0.7 - 1:\t\t', PK_07null, ' (', PK_07null_perc, '%)\n', sep = '' )
    base::cat( '  -> Null P-K over 1:\t\t', PK_10null, ' (', PK_10null_perc, '%)\n', sep = '' )
    base::cat( '----------------------------------------------------------------------------\n' )
  }
  SUMMARY_OUT <- list(
    'mean' = loo_mean,
    'se' = COMP_SE,
    'p' = COMP_P_VALUE,
    'pk05' = PK_05,
    'pk07' = PK_07,
    'pk1' = PK_10 )
  return( SUMMARY_OUT )
}
