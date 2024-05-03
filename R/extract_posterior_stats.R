#' Extract posterior summary statistics given the vector of posterior draws of a specific variable inside a brms::brm() fitted model
#'
#' This function extract summary statistics given the vector of posterior draws of a
#' variable inside a fitted brms::brm() model
#'
#' @param post: a vector of posterior draws
#' @return A data.frame() object with the summary statistics for the varibale
#' @export
extract_posterior_stats <- function(
  post
)
{
  ### extract summary statistics for posteriors
  var_fit <- post
  var_mean <- base::mean( var_fit, na.rm = TRUE )
  var_sd <- stats::sd( var_fit, na.rm = TRUE )
  var_se <- stats::sd( var_fit, na.rm = TRUE ) / base::sqrt( base::length( var_fit ) )
  var_median <- stats::median( var_fit, 0.75 )
  var_q1 <- stats::quantile( var_fit, 0.25 )
  var_q3 <- stats::quantile( var_fit, 0.75 )
  var_min <- base::min( var_fit )
  var_max <- base::max( var_fit )
  var_mode <- bayestestR::map_estimate( var_fit )
  var_posterior_stat <- base::list(
    'mean' = var_mean,
    'sd' = var_sd,
    'se' = var_se,
    'median' = var_median,
    'q1' = var_q1,
    'q3' = var_q3,
    'min' = var_min,
    'max' = var_max,
    'map' = base::as.numeric(var_mode),
    'eti_95l' = base::as.numeric(bayestestR::eti( var_fit )[2]),
    'eti_95h' = base::as.numeric(bayestestR::eti( var_fit )[3]),
    'hdi_95l' = base::as.numeric(bayestestR::hdi( var_fit )[2]),
    'hdi_95h' = base::as.numeric(bayestestR::hdi( var_fit )[3]),
    'rope' = base::as.numeric(bayestestR::rope(var_fit))
  )
  var_posterior_stat_df <- base::as.data.frame(var_posterior_stat)
  return( var_posterior_stat_df )
}
