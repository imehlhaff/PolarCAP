% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.PolarCAP.R
\name{get.PolarCAP}
\alias{get.PolarCAP}
\title{Retrieve PolarCAP Data}
\usage{
get.PolarCAP(
  countries = NA,
  years = NA,
  type = c("ideology", "affect"),
  value.only = FALSE,
  include.se = FALSE
)
}
\arguments{
\item{countries}{a character vector of countries to be retrieved. See Details.}

\item{years}{a numeric vector of years to be retrieved.}

\item{type}{a character vector indicating which polarization estimates should be returned. Must be
\code{"ideology"}, \code{"affect"}, or both.}

\item{value.only}{a logical indicating whether \code{get.PolarCAP()} should return a data frame of
results (\code{FALSE}, the default) or a single estimate as a scalar (\code{TRUE}).}

\item{include.se}{a logical indicating whether standard errors should be returned. Defaults to
\code{FALSE}.}
}
\value{
If \code{value.only = FALSE}, \code{get.PolarCAP()} returns a data frame with columns
corresponding to country names, country ISO3 codes, years, polarization estimates for the
polarization type(s) given in \code{type}, and associated standard errors (if
\code{include.se = TRUE}). If \code{value.only = TRUE}, \code{get.PolarCAP()} returns a scalar
polarization estimate for the polarization type given in \code{type}.
}
\description{
Retrieves PolarCAP data for defined countries and years. Returns data in wide format. For tidy
format, use \code{\link{melt.PolarCAP}()}.
}
\details{
Ideally, country names passed to \code{countries} would be ISO 3166-1 alpha-3 country codes
(case-insensitive). However, \code{get.PolarCAP()} will accept country names in almost any language or
format and attempt to convert them to ISO3 codes by calling \code{\link{to.ISO3}()}.
\code{get.PolarCAP()} will alert the user to any country names still unrecognized after this
conversion and return results only for those which are recognized.
}
\examples{
get.PolarCAP("USA", c(2018, 2019), "ideology", include.se = TRUE)
get.PolarCAP("USA", c(2018, 2019), c("ideology", "affect"), include.se = TRUE)

countries <- rep(c("MEX", "USA"), each = 2)
years <- rep(c(2018, 2019), 2)
data <- as.data.frame(cbind(countries, years))

data$ideology1 <- apply(data, 1, function(x) get.PolarCAP(x[1], x[2], type = "ideology",
value.only = TRUE))
data

}
