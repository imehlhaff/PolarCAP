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
  #load data
  data <- PolarCAP

  #create vector to save final country codes
  out <- c()

  #assign entry to final country codes if ISO3 country in PolarCAP, otherwise search for it and replace it with result
  for (country in countries) {
    #if entry is NA, assign NA to final vector
    if (is.na(country)) {
      out <- c(out, country)
    }

    #otherwise, attempt to convert to ISO3
    else {
      #check if entry is ISO3 country code in PolarCAP
      country_grep <- grepl(country, unique(data$country_code), ignore.case = TRUE)

      #capitalize and assign to final vector if is ISO3 country code in PolarCAP
      if (any(country_grep)) {
        country_up <- toupper(country)
        out <- c(out, country_up)
      }

      #otherwise, search for country code and assign to final vector
      else {
        #attempt to convert to standard country name
        country_iso3 <- countryname(country, destination = "iso3c", warn = FALSE)

        #if successful, assign to final vector
        if (!is.na(country_iso3)) {
          out <- c(out, country_iso3)
        }

        #otherwise, assign original entry to final vector
        else {
          out <- c(out, country)
        }
      }
    }
  }
  out
}
