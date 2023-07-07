#' Retrieve Tidy PolarCAP Data
#'
#' Retrieves PolarCAP data for defined countries and years. Returns data in tidy format. For wide
#' format, or to return a polarization estimate as a scalar, use \code{\link{get.PolarCAP}()}.
#'
#' @details
#' Ideally, country names passed to \code{countries} would be ISO 3166-1 alpha-3 country codes
#' (case-insensitive). However, \code{melt.PolarCAP()} will accept country names in almost any language or
#' format and attempt to convert them to ISO3 codes by calling \code{\link{to.ISO3}()}.
#' \code{melt.PolarCAP()} will alert the user to any country names still unrecognized after this
#' conversion and return results only for those which are recognized.
#'
#' @param countries a character vector of countries to be retrieved. See Details.
#' @param years a numeric vector of years to be retrieved.
#' @param type a character vector indicating which polarization estimates should be returned. Must be
#' \code{"ideology"}, \code{"affect"}, or both.
#' @param include.se a logical indicating whether standard errors should be returned. Defaults to
#' \code{FALSE}.
#'
#' @return a data frame with columns corresponding to country names, country ISO3 codes, years,
#' polarization types given in \code{type}, polarization estimates, and associated standard errors (if
#' \code{include.se = TRUE}).
#'
#' @examples
#' melt.PolarCAP("USA", c(2018, 2019), "ideology", include.se = TRUE)
#' melt.PolarCAP("USA", c(2018, 2019), c("ideology", "affect"), include.se = TRUE)
#'
#' @import countrycode tidyr
#' @export

melt.PolarCAP <- function(countries = NA, years = NA, type = c("ideology", "affect"),
                          include.se = FALSE) {

  data <- get.PolarCAP(countries, years, type, include.se = TRUE)

  data_est <- data[,c("country", "country_code", "year", type, "notes")]
  data_est <- pivot_longer(data_est, cols = all_of(type), names_to = "type", values_to = "estimate")

  if (include.se) {
    data_se <- data[,c("country", "country_code", "year", paste(type, "_se", sep = ""))]
    data_se <- pivot_longer(data_se, cols = all_of(paste(type, "_se", sep = "")), names_to = "type",
                            values_to = "se")
    out <- cbind(data_est, data_se)
  }

  else {
    out <- data_est
  }

  if (include.se) {
    out <- out[,c("country", "country_code", "year", "type", "estimate", "se", "notes")]
  }

  else {
    out <- out[,c("country", "country_code", "year", "type", "estimate", "notes")]
  }

  out
}
