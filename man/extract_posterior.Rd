% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_posterior.R
\name{extract_posterior}
\alias{extract_posterior}
\title{Extract posterior draws from a brms::brm() fitted model for a variable of interest in the model}
\usage{
extract_posterior(
  model,
  var,
  var_level = NULL,
  var_level_sigma = FALSE,
  inverse = FALSE,
  transformed = TRUE,
  multinomial_level = NULL,
  plot_xlim = NULL,
  plot_risk_word = "posterior",
  palette_risk = "OrRd",
  font_family = "sans-serif",
  font_size = 16,
  palette_trace = "PuOr"
)
}
\arguments{
\item{model:}{a brms::brm() fitted model}

\item{var:}{the column name of the variable of interest. It must be a variable passed in the model}

\item{var_level:}{the level of interest of the variable passed as "var" (optional, if not specified all levels will be plotted)}

\item{var_level_sigma:}{the level of interest of the variable passed as "var" inside sigma covariates (optional, if not specified all levels will be plotted)}

\item{inverse:}{in case passed outcome variable is a binomial factor, user can ask to plot values relative to the factor that the model used as reference (defaults to FALSE)}

\item{transformed:}{in case link family is not "identity", if set to FALSE it back transform posterior to original scale (defaults to TRUE)}

\item{multinomial_level:}{the level of interest in the outcome variable if a multinomial model was fitted (optional)}

\item{plot_xlim:}{x-axis limit values for the risk_plot in ggplot2::xlim format (e.g. "c(-0.5, 1.5)") (optional)}

\item{plot_risk_word:}{the word to use for variable posterior effect value in plotted text (defaults to "posterior")}

\item{palette_risk:}{the color palette to use for the posterior log-risk distribution plot (defaults to "OrRd")}

\item{palette_trace:}{the color palette to use for the posterior trace plot (defaults to "PuOr")}

\item{font_size:}{the font size to use for the risk and trace plot (defaults to 16)}

\item{font_family:}{the font family to use for the risk and trace plot (defaults to "sans-serif")}
}
\value{
A list() object composed of "$data": the data.frame object with posterior draws; "risk_plot": the posterior log-risk distribution plot for the variable of interest as a ggplot2() object; "trace_plot": the posterior trace plot for the variable of interest as a ggplot2() object; "combined_plot": the "risk_plot" and "trace_plot" combined as ggplot2() object
}
\description{
This function extract posterior log-risk and trace distribution for the variable
of interest specified in the "var" argument (which must be a covariate of the model)
and returns a data.frame object with posterior draws ("data"); the posterior log-risk
plot ("risk_plot"); the posterior trace plot for the variable of interest ("trace_plot")
and both these plots combined ("combined_plot")
}
