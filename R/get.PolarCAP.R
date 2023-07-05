#' Retrieve PolarCAP Data
#'
#' Retrieves PolarCAP data for defined countries and years. Returns data in wide format. For tidy
#' format, use \code{\link{melt.PolarCAP}()}.
#'
#' @details
#' Ideally, country names passed to \code{countries} would be ISO 3166-1 alpha-3 country codes
#' (case-insensitive). However, \code{get.PolarCAP()} will accept country names in almost any language or
#' format and attempt to convert them to ISO3 codes by calling \code{\link{to.ISO3}()}.
#' \code{get.PolarCAP()} will alert the user to any country names still unrecognized after this
#' conversion and return results only for those which are recognized.
#'
#' @param countries a character vector of countries to be retrieved. See Details.
#' @param years a numeric vector of years to be retrieved.
#' @param type a character vector indicating which polarization estimates should be returned. Must be
#' \code{"ideology"}, \code{"affect"}, or both.
#' @param value.only a logical indicating whether \code{get.PolarCAP()} should return a data frame of
#' results (\code{FALSE}, the default) or a single estimate as a scalar (\code{TRUE}).
#' @param include.se a logical indicating whether standard errors should be returned. Defaults to
#' \code{FALSE}.
#'
#' @return If \code{value.only = FALSE}, \code{get.PolarCAP()} returns a data frame with columns
#' corresponding to country names, country ISO3 codes, years, polarization estimates for the
#' polarization type(s) given in \code{type}, and associated standard errors (if
#' \code{include.se = TRUE}). If \code{value.only = TRUE}, \code{get.PolarCAP()} returns a scalar
#' polarization estimate for the polarization type given in \code{type}.
#'
#' @examples
#' get.PolarCAP("USA", c(2018, 2019), "ideology", include.se = TRUE)
#' get.PolarCAP("USA", c(2018, 2019), c("ideology", "affect"), include.se = TRUE)
#'
#' countries <- rep(c("MEX", "USA"), each = 2)
#' years <- rep(c(2018, 2019), 2)
#' data <- as.data.frame(cbind(countries, years))
#'
#' data$ideology1 <- apply(data, 1, function(x) get.PolarCAP(x[1], x[2], type = "ideology",
#' value.only = TRUE))
#' data <- rowwise(data) %>% mutate(ideology2 = get.PolarCAP(countries, years, type = "ideology",
#' value.only = TRUE))
#' data
#'
#' @import countrycode
#' @export

get.PolarCAP <- function(countries = NA, years = NA, type = c("ideology", "affect"),
                         value.only = FALSE, include.se = FALSE) {
  #load data
  data <- PolarCAP

  #filter for desired countries and years
  if (all(is.na(countries))) {
    warning("No countries provided. Returning results for all available countries.")
    countries <- unique(data$country_code)
  }

  if (all(is.na(years))) {
    warning("No years provided. Returning results for all available years.")
    years <- unique(data$year)
  }

  #convert countries to ISO3 country codes if necessary
  countries <- to.ISO3(countries)

  #convert years to numeric if necessary
  if (!is.numeric(years)) {
    years <- as.numeric(years)
  }

  #throw warnings if desired countries or years are not in dataset
  for (country in countries) {
    if (!country %in% unique(data$country_code)) {
      warning(paste("No PolarCAP data for country:", country))
    }
  }

  for (year in years) {
    if (!year %in% unique(data$year)) {
      warning(paste("No PolarCAP data for year:", year))
    }
  }

  #throw error if polarization type is not recognized
  for (entry in type) {
    if (!entry %in% c("ideology", "affect")) {
      stop('type must be either "ideology", "affect", or both')
    }
  }

  data_cy <- data[data$country_code %in% countries & data$year %in% years,]

  #select desired polarization type
  if (include.se) {
    data_cy <- data_cy[,c("country", "country_code", "year", type, paste(type, "_se", sep = ""),
                          "notes")]
  }

  else {
    data_cy <- data_cy[,c("country", "country_code", "year", type, "notes")]
  }

  #throw warning if using value.only = TRUE with both polarization types
  if (length(type) == 2 & value.only) {
    warning("Using value.only = TRUE with both types of polarization. Output will be vector, not scalar.")
  }

  #throw warning if using value.only = TRUE with include.se = TRUE
  if (value.only & include.se) {
    warning("Using value.only = TRUE with include.se = TRUE. Output will not include standard errors. Set value.only = FALSE to obtain a data frame with estimates and standard errors.")
  }

  #return desired output
  if (value.only) {
    out <- as.numeric(unlist(data_cy[,c(type)]))
    out
  }

  else {
    out <- data_cy
    out
  }
}
