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
#' @param var_level_sigma: the level of interest of the variable passed as "var" inside sigma covariates (optional, if not specified all levels will be plotted)
#' @param inverse: in case passed outcome variable is a binomial factor, user can ask to plot values relative to the factor that the model used as reference (defaults to FALSE)
#' @param transformed: in case link family is not "identity", if set to FALSE it back transform posterior to original scale (defaults to TRUE)
#' @param multinomial_level: the level of interest in the outcome variable if a multinomial model was fitted (optional)
#' @param plot_xlim: x-axis limit values for the risk_plot in ggplot2::xlim format (e.g. "c(-0.5, 1.5)") (optional)
#' @param plot_risk_word: the word to use for variable posterior effect value in plotted text (defaults to "posterior")
#' @param palette_risk: the color palette to use for the posterior log-risk distribution plot (defaults to "OrRd")
#' @param palette_trace: the color palette to use for the posterior trace plot (defaults to "PuOr")
#' @param font_size: the font size to use for the risk and trace plot (defaults to 16)
#' @param font_family: the font family to use for the risk and trace plot (defaults to "sans-serif")
#' @return A list() object composed of "$data": the data.frame object with posterior draws; "risk_plot": the posterior log-risk distribution plot for the variable of interest as a ggplot2() object; "trace_plot": the posterior trace plot for the variable of interest as a ggplot2() object; "combined_plot": the "risk_plot" and "trace_plot" combined as ggplot2() object
#' @export
extract_posterior <- function(
  model,
  var,
  var_level = NULL,
  var_level_sigma = FALSE,
  inverse = FALSE,
  transformed = TRUE,
  multinomial_level = NULL,
  plot_xlim = NULL,
  plot_risk_word = 'posterior',
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
  var_index <- vector()
  var_levels <- vector()
  var_class <- vector()
  var_group <- vector()
  var_dpar <- vector()
  for ( i in 1:length(model_prior_coefficients) )
  {
    if ( base::grepl( var, model_prior_coefficients[i], fixed = TRUE ) ) {
      var_index <- c( var_index, i )
      var_class <- c( var_class, model$prior$class[i] )
      var_group <- c( var_group, model$prior$group[i] )
      var_dpar <- c( var_dpar, model$prior$dpar[i] )
      var_levels <- c( var_levels, base::gsub( var, "", model_prior_coefficients[i] ) )
    }
  }
  ### use first level as default level
  var_level_index <- 1
  var_factor <- var_levels[ var_level_index ]
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
    prior_prefix <- base::paste( var_class[ var_level_index ], '_', sep = '' )
  }
  ### if var has multiple factors create plots and data for each one
  names_vector <- base::vector()
  plot_list <- base::list()
  for ( i in 1:length(var_levels) ) {
    prior_prefix <- var_class[i]
    var_prior_dpar <- var_dpar[i]
    var_factor <- var_levels[i]
    if ( base::nchar(var_factor) > 0 ) {
      if ( base::nchar(var_prior_dpar) > 0 ) {
        COL_NAME <- base::paste( prior_prefix, '_', var_prior_dpar, '_', var, var_factor, sep = '' )
      } else {
        COL_NAME <- base::paste( prior_prefix, '_', var, var_factor, sep = '' )
      }
    } else {
      if ( base::nchar(var_prior_dpar) > 0 ) {
        COL_NAME <- base::paste( prior_prefix, '_', var_prior_dpar, '_', var, sep = '' )
      } else {
        COL_NAME <- base::paste( prior_prefix, '_', var, sep = '' )
      }
    }
    names_vector <- c( names_vector, COL_NAME )
    ### extract variable posterior from the model
    fit_df <- base::as.data.frame(model$fit)
    var_fit <- fit_df[[COL_NAME]]
    ### based on link function used (for main family and sigma too) need to change returned value: if "identity" values are returned as such()
    link_main <- model$family$link
    if ( (link_main == "log") | ( link_main == "logit" ) | ( var_prior_dpar == "sigma" ) ) {
      if ( base::isFALSE(transformed) ) {
        var_fit <- base::exp( var_fit )
      }
    }
    ### extract summary statistics for posteriors
    var_posterior_stat_df <- extract_posterior_stats( post = var_fit )
    rownames(var_posterior_stat_df) <- COL_NAME
    PLOT_DB[[COL_NAME]] <- var_fit
    # PLOT_DB[[COL_NAME]] <- var_fit
    # how many are >0 (log risk ratio)
    LRR <- base::round( base::length( base::which(PLOT_DB[[COL_NAME]] > 0) ) / base::length(PLOT_DB[[COL_NAME]]) * 100, 2 )
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
        P0 <- P0 + ggplot2::annotate("text", x=min(plot_xlim) + 0.2, y = 1, label = base::paste( var, ': ', var_inverse, ' ', plot_risk_word, ': ', 100-LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
      } else {
        P0 <- P0 + ggplot2::annotate("text", x=min(-PLOT_DB[[(COL_NAME)]]) + 0.2, y = 1, label = base::paste( var, ': ', var_inverse, ' ', plot_risk_word, ': ', 100-LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
      }
    } else {
      P0 <- P0 + ggplot2::geom_density( data = PLOT_DB, ggplot2::aes( x = base::get(COL_NAME), y = ggplot2::after_stat(scaled), group = 1, colour = 0, fill = 10 ), lwd = 2, alpha = 0.25 )
      if ( ! base::is.null(plot_xlim) ) {
        P0 <- P0 + ggplot2::annotate("text", x=min(plot_xlim) + 0.2, y = 1, label = base::paste( var, ': ', var_factor, ' ', plot_risk_word, ': ', LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
      } else {
        P0 <- P0 + ggplot2::annotate("text", x=min(PLOT_DB[[(COL_NAME)]]) + 0.2, y = 1, label = base::paste( var, ': ', var_factor, ' ', plot_risk_word, ': ', LRR, '%', sep = '' ), family = ggplot2::theme_get()$text[["family"]], size = ggplot2::theme_get()$text[["size"]]/2.5 )
      }
    }
    P0 <- P0 + ggplot2::geom_vline( xintercept = 0, linetype="dashed", color = "red", size = 1 ) +
    ggplot2::labs( x = base::paste("Log ", plot_risk_word, " ratio", sep = ''), y = "Scaled Density", title = "Posterior log distribution" ) +
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
    ggplot2::labs( y = base::paste("Log ", plot_risk_word, " ratio", sep = ''), x = "Iteration", title = "Posterior trace plot" ) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size=font_size,  family=font_family)) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = base::list(size = 8, linewidth = 3)))
    PC <- base::suppressWarnings( ggpubr::ggarrange( P0, P1, nrow = 2 ))
    plot_list[[ base::length(plot_list) + 1 ]] <- base::list( 'risk_plot' = P0, 'trace_plot' = P1, 'data' = M00df, 'combined_plot' = PC )
    ### combine posterior statistics
    if ( i > 1 ) {
      posterior_stat_df <- base::rbind(posterior_stat_df, var_posterior_stat_df)
    } else {
      posterior_stat_df <- var_posterior_stat_df
    }
  }
  ### if user specified a precise level return that only
  if ( ! base::is.null( var_level ) ) {
    if ( var_level %in% var_levels ) {
      var_levels_index <- base::which( var_levels == var_level )
      ### if user asked for sigma value of that precise level return this
      if ( base::isTRUE(var_level_sigma) ) {
        var_levels_dpar_index <- base::which( var_dpar == "sigma" )
        var_level_index <- base::unique( var_levels_index[ var_levels_index %in% var_levels_dpar_index ])
      } else {
        var_levels_dpar_index <- base::which( var_dpar == "" )
        var_level_index <- base::unique( var_levels_index[ var_levels_index %in% var_levels_dpar_index ])
      }
      OUT_LIST <- plot_list[[var_level_index]]
      OUT_LIST[['stats']] <- posterior_stat_df[var_level_index,]
    } else {
      base::stop( ' level: ', var_level, ' not found in var levels. Please check! \n', sep = '' )
    }
  ### if not asked for a specific level returns all levels (each named based on factor and sigma)
  } else {
    OUT_LIST <- plot_list
    names(OUT_LIST) <- names_vector
    OUT_LIST[['stats']] <- posterior_stat_df
  }
  return( OUT_LIST )
}
