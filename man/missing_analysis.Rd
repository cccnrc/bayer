% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_analysis.R
\name{missing_analysis}
\alias{missing_analysis}
\title{Operate a summary of missing in the data.frame object passed}
\usage{
missing_analysis(data, plot = FALSE, stdout = TRUE)
}
\arguments{
\item{data:}{the input data.frame to perform analysis on}

\item{plot:}{naniar plot returned (defaults to FALSE)}

\item{stdout:}{summary printed to stdout (defaults to TRUE)}
}
\value{
A naniar::gg_miss_upset() plot of the missing distribution in the database
}
\description{
This function operates a summary of missing distribution in the data.frame object passed
and returns a naniar::gg_miss_upset() plot showing missing distribution
}
