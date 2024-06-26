#' Extract posterior draws from a brms::brm() fitted model for a variable of interest in the model
#'
#' This function extract posterior log-risk and trace distribution for the variable
#' of interest specified in the "var" argument (which must be a covariate of the model)
#' and returns a data.frame object with posterior draws ("data"); the posterior log-risk
#' plot ("risk_plot"); the posterior trace plot for the variable of interest ("trace_plot")
#' and both these plots combined ("combined_plot")
#'
#' @param model: a brms::brm() fitted model
#' @param var: the column name of the variable of interest. It must be a variable passed in the model
#' @param var_level: the level of interest of the variable passed as "var" (optional, if not specified all levels will be plotted)
#' @param inverse: in case passed outcome variable is a binomial factor, user can ask to plot values relative to the factor that the model used as reference (defaults to FALSE)
#' @param multinomial_level: the level of interest in the outcome variable if a multinomial model was fitted (optional)
#' @param plot_xlim: x-axis limit values for the risk_plot in ggplot2::xlim format (e.g. "c(-0.5, 1.5)") (optional)
#' @param palette_risk: the color palette to use for the posterior log-risk distribution plot (defaults to "OrRd")
#' @param palette_trace: the color palette to use for the posterior trace plot (defaults to "PuOr")
#' @param font_size: the font size to use for the risk and trace plot (defaults to 16)
#' @param font_family: the font family to use for the risk and trace plot (defaults to "sans-serif")
#' @return A list() object composed of "$data": the data.frame object with posterior draws; "risk_plot": the posterior log-risk distribution plot for the variable of interest as a ggplot2() object; "trace_plot": the posterior trace plot for the variable of interest as a ggplot2() object; "combined_plot": the "risk_plot" and "trace_plot" combined as ggplot2() object
extract_posterior_old <- function(
  model,
  var,
  var_level = NULL,
  inverse = FALSE,
  multinomial_level = NULL,
  plot_xlim = NULL,
  palette_risk = 'OrRd',
  font_family = "sans-serif",
  font_size = 16,
  palette_trace = 'PuOr' )
{
  multinomial_reference <- multinomial_level
  PALETTE <- palette_risk
  PALETTE2 <- palette_trace
  ### this returns the posterior as a data.frame object, I have a column for each variable passed
  #   and 3 more columns: .chain, .iteration, .draw that represent single chain, iteration and draw for each row
  M00df <- brms::as_draws_df( model )
  PLOT_DB <- M00df
  ### extract variable of interest categories passed
  if ( ! var %in% base::colnames( model$data ) ) {
    base::stop('\n ', var, ' not found in model \n', sep = '')
  }
  model_prior_coefficients <- model$prior$coef
  var_index <- 0
  for ( i in 1:length(model_prior_coefficients) )
  {
    if ( base::grepl( var, model_prior_coefficients[i], fixed = TRUE ) ) {
      var_index <- i
      break
    }
  }
  var_factor <- base::gsub( var, "", model_prior_coefficients[ var_index ] )
  ### if family is multinomial some differences apply as outcome is referred to a particular category
  if ( model$family$family == 'multinomial' ) {
    ### if user did not specify a level interested in use the first you get
    if ( base::is.null(multinomial_reference) ) {
      prior_prefix <- base::paste( model$prior$class[ var_index ], '_', model$prior$dpar[ var_index ], '_', sep = '' )
      base::warning( '\n no level specified for a multinomial model. Using: "', base::gsub( "mu", "", model$prior$dpar[ var_index ] ), '" to plot posterior results \n  - tip: use "multinomial_level" option to plot result for a specific factor \n' )
    } else {
      if ( multinomial_reference %in% base::unique(base::gsub( "mu", "", model$prior$dpar )) ) {
        prior_prefix <- base::paste( model$prior$class[ var_index ], '_mu', multinomial_reference, '_', sep = '' )
      } else {
        base::stop( '\n multinomial_level specified: "', multinomial_reference, '" not found in model \n  - tip: either you typed it wrong or it was used as reference level \n' )
      }
    }
  } else {
    prior_prefix <- base::paste( model$prior$class[ var_index ], '_', sep = '' )
  }
  ### if var actually had a factor appended
  if ( base::nchar(var_factor) > 0 ) {
    COL_NAME <- base::paste( prior_prefix, var, var_factor, sep = '' )
  } else {
    COL_NAME <- base::paste( prior_prefix, var, sep = '' )
  }
  # how many are >0 (log risk ratio)
  LRR <- base::round( base::length( base::which(M00df[[COL_NAME]] > 0) ) / base::length(M00df[[COL_NAME]]) * 100, 2 )
  ### if user wants to plot the reference factor of a categorical binomial variable
  if ( base::isTRUE(inverse) ) {
    P0 <- ggplot2::ggplot(PLOT_DB, ggplot2::aes(x = -base::get(COL_NAME), group = .chain, colour = .chain + 5)) +
            ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)), lwd = 1 )
  } else {
    P0 <- ggplot2::ggplot(PLOT_DB, ggplot2::aes(x = base::get(COL_NAME), group = .chain, colour = .chain + 5)) +
            ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)), lwd = 1 )
  }
  P0 <- P0 + ggplot2::scale_fill_distiller(palette = PALETTE) + ggplot2::scale_color_distiller(palette = PALETTE)
  ### add the overall density plot
  ### if user wants to plot the reference factor of a categorical binomial variable
  if ( base::isTRUE(inverse) ) {
    P0 <- P0 + ggplot2::geom_density( data = PLOT_DB, ggplot2::aes( x = -base::get(COL_NAME), y = ggplot2::after_stat(scaled), group = 1, colour = 0, fill = 10 ), lwd = 2, alpha = 0.25 )
    var_levels <- base::levels(model$data[[var]])
    if ( base::length(var_levels) > 2 ) {
      stop( ' identified more than 2 levels for the outcome variable:', var_levels, ', to use "inverse" option variable must have 2 levels only! \n' )
    }
    var_level_index <- base::which( var_levels == var_factor )
    var_inverse <- var_levels[ -var_level_index ]
    if ( ! base::is.null(plot_xlim) ) {
      P0 <- P0 + ggplot2::annotate("text", x=min(plot_xlim) + 0.2, y = 1, label = base::paste( var, ': ', var_inverse, ' risk: ', 100-LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
    } else {
      P0 <- P0 + ggplot2::annotate("text", x=min(-PLOT_DB[[(COL_NAME)]]) + 0.2, y = 1, label = base::paste( var, ': ', var_inverse, ' risk: ', 100-LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
    }
  } else {
    P0 <- P0 + ggplot2::geom_density( data = PLOT_DB, ggplot2::aes( x = base::get(COL_NAME), y = ggplot2::after_stat(scaled), group = 1, colour = 0, fill = 10 ), lwd = 2, alpha = 0.25 )
    if ( ! base::is.null(plot_xlim) ) {
      P0 <- P0 + ggplot2::annotate("text", x=min(plot_xlim) + 0.2, y = 1, label = base::paste( var, ': ', var_factor, ' risk: ', LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
    } else {
      P0 <- P0 + ggplot2::annotate("text", x=min(PLOT_DB[[(COL_NAME)]]) + 0.2, y = 1, label = base::paste( var, ': ', var_factor, ' risk: ', LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
    }
  }
  P0 <- P0 + ggplot2::geom_vline( xintercept = 0, linetype="dashed", color = "red", size = 1 ) +
              ggplot2::labs( x = "Log risk ratio", y = "Scaled Density", title = "Posterior log-risk distribution" ) +
              ggplot2::theme_classic() +
              ggplot2::scale_y_continuous(labels = scales::percent) +
              ggplot2::theme(legend.position="none", text = ggplot2::element_text(size=font_size,  family=font_family))
  if ( ! base::is.null(plot_xlim) ) {
    P0 <- P0 + ggplot2::scale_x_continuous(limits = plot_xlim)
  }
  ### plot iteration trace
  P1 <- ggplot2::ggplot(PLOT_DB, ggplot2::aes( x = .iteration, y = base::get(COL_NAME), group = base::factor(.chain), colour = base::factor(.chain) )) +
        ggplot2::geom_line( alpha = 0.6 ) +
        ggplot2::scale_colour_brewer( name = "Chain", palette = PALETTE2 )
  # P1 <- P1 + scale_fill_distiller(palette = PALETTE2) + scale_color_distiller(palette = PALETTE2)
  P1 <- P1 +
            ggplot2::labs( y = "Log risk ratio", x = "Iteration", title = "Posterior trace plot" ) +
            ggplot2::theme_classic() +
            ggplot2::theme(text = ggplot2::element_text(size=font_size,  family=font_family)) +
            ggplot2::guides(color = ggplot2::guide_legend(override.aes = base::list(size = 8, linewidth = 3)))
  PC <- base::suppressWarnings( ggpubr::ggarrange( P0, P1, nrow = 2 ))
  OUT_LIST <- list( 'risk_plot' = P0, 'trace_plot' = P1, 'data' = M00df, 'combined_plot' = PC )
  return( OUT_LIST )
}
