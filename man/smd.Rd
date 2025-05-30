% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smd.R
\name{smd}
\alias{smd}
\title{Get SMD for Variables in a Data Frame
Returns pairwise standardized mean differences (SMDs) for all variables
in the data frame.}
\usage{
smd(
  data,
  grp,
  wts = NULL,
  nonnormal = NULL,
  abs = TRUE,
  denom = "unweighted",
  digits = 3
)
}
\arguments{
\item{data}{A data frame}

\item{grp}{A character vector of length 1 specifying grouping variable}

\item{wts}{A non-negative numeric vector of weights.}

\item{nonnormal}{A character vector specifying variables to be treated as
non-normal}

\item{abs}{Logical specifying whether to return absolute SMD}

\item{denom}{Character vector specifying how to calculate denominator}

\item{digits}{Integer specifying number of decimal places to be used.}
}
\value{
A list with two elements. First element holds SMDs for numeric variables,
and second element holds SMDs for factor variables.
}
\description{
Get SMD for Variables in a Data Frame
Returns pairwise standardized mean differences (SMDs) for all variables
in the data frame.
}
\examples{
smd(iris, "Species")
smd(warpbreaks, "tension")
}
