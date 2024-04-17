#' Calculates Watanabe-Akaike Information Criterion (WAIC) from a brms::brm() fitted model for a variable of interest in the model
#'
#' This function calculates WAIC comparing a brms::brm() fitted model ("model") with the variable
#' of interest and a null model identical to the passed one without the variable of interest.
#' User can either pass the variable of interest name ("var") OR the null model directly ("model_null").
#' In this case, analysis will take longer as the null model needs to be compiled and sampled.
#'
#' Interpreting WAIC results: When interpreting the results, a significant difference in WAIC
#' (considering the uncertainty in the estimates) implies that one model provides a better fit
#' to the data than the other.
#' If the model with the variable of interest has a significantly lower WAIC, it suggests that
#' including that variable improves the model's predictive accuracy. However, if there's no
#' clear difference, it may indicate that this specific variable doesn't contribute significantly
#' to the model in terms of predictive power.
#'
#' @param model: a brms::brm() fitted model
#' @param model_null: a brms::brm() fitted model without the variable of interest (optional)
#' @param var: instead of passing model and model_null the variable of interest name can be passed. This will take longer as model_null will be compiled (optional)
#' @param stdout: print a summary of comparison to stdout (default to TRUE)
#' @return A list() object composed of "mean": the mean WAIC difference value between model and model_nulll; "se": the standard error of the WAIC value between model and model_nulll; "p": the p-value of WAIC comparison between model and model_nulll
#' @export
waic_analysis <- function( model, model_null = NULL, var = NULL, stdout = TRUE )
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
  WAIC0 <- brms::waic(MODEL_INPUT0)
  WAIC0null <- brms::waic(MODEL_INPUT1)
  # Compare WAIC
  WAIC0_comparison <- brms::loo_compare(WAIC0, WAIC0null)
  ### check which model is in the last row
  last_row <- base::rownames(WAIC0_comparison)[ base::length(base::rownames(WAIC0_comparison)) ]
  ### if model null was on last row model is better if mean value is negative
  if ( last_row == 'MODEL_INPUT1' ) {
    better_model <- TRUE
  ### if model was on last row model is better if mean value is negative
  } else {
    better_model <- FALSE
  }
  ### compute values
  NUM_OBS <- base::attributes( WAIC0 )$dims[2]
  COMP_DF <- NUM_OBS + NUM_OBS - 2
  # Difference in means and standard error
  COMP_MEAN <- base::abs(WAIC0_comparison[[2]])
  COMP_SE <- base::abs(WAIC0_comparison[[4]])
  # Calculate t-value
  COMP_T_VALUE <- COMP_MEAN / COMP_SE
  # Calculate p-value
  COMP_P_VALUE <- 2 * stats::pt( - base::abs(COMP_T_VALUE), COMP_DF )
  ### return results based on which was the best model
  if ( base::isTRUE(better_model) ) {
    waic_mean <- WAIC0_comparison[[2]]
  } else {
    waic_mean <- -WAIC0_comparison[[2]]
  }
  if ( base::isTRUE(stdout) ) {
    ### output summary
    cat( '----------------------------------------------------------------------------\n' )
    cat( '  -> WAIC mean difference:\t', waic_mean, '\n', sep = '' )
    cat( '  -> WAIC difference SE:\t', COMP_SE, '\n', sep = '' )
    cat( '  -> WAIC difference P:\t\t', COMP_P_VALUE, '\n', sep = '' )
    cat( '----------------------------------------------------------------------------\n' )
  }
  SUMMARY_OUT <- list(
    'mean' = WAIC0_comparison[[2]],
    'se' = COMP_SE,
    'p' = COMP_P_VALUE )
  return( SUMMARY_OUT )
}
