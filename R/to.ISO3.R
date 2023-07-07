#' Convert Country Names for PolarCAP Retrieval
#'
#' Checks if requested countries are formatted correctly for PolarCAP and attempts to convert them to
#' ISO 3166-1 alpha-3 country codes if not.
#'
#' @param countries a character vector of countries to be checked (case-insensitive).
#'
#' @return a character vector of length equal to that of \code{countries}.
#'
#' @examples
#' to.ISO3(c("ALB", "aus", "united states"))
#'
#' @import countrycode
#' @export

to.ISO3 <- function(countries) {

  data <- PolarCAP

  out <- c()

  for (country in countries) {
    if (is.na(country)) {
      out <- c(out, country)
    }

    else {
      country_grep <- grepl(country, unique(data$country_code), ignore.case = TRUE)

      if (any(country_grep)) {
        country_up <- toupper(country)
        out <- c(out, country_up)
      }

      else {
        country_iso3 <- countryname(country, destination = "iso3c", warn = FALSE)

        if (!is.na(country_iso3)) {
          out <- c(out, country_iso3)
        }

        else {
          out <- c(out, country)
        }
      }
    }
  }
  out
}
