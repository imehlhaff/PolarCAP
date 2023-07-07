
# PolarCAP

<!-- badges: start -->
[![R-CMD-check](https://github.com/imehlhaff/PolarCAP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/imehlhaff/PolarCAP/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Distributes data from the Polarization in Comparative Attitudes Project. Helper functions enable data retrieval in wide and tidy formats for user-defined countries and years. Provides support for case-insensitive country names in many languages.

## Installation

There are two ways to easily install this package in R. To install the stable version released to CRAN, install as normal:

```
install.packages("PolarCAP")
library(PolarCAP)
```

To install the most recent development version, first ensure you have the latest version of ```devtools``` installed:

```{r}
install.packages("devtools")
library(devtools)
```

Then, run the following code:

```{r}
devtools::install_github("imehlhaff/PolarCAP")
```

## Citation

To cite ```PolarCAP``` in publications and working papers, please use:

Mehlhaff, Isaac D. *Mass Polarization across Time and Space*, book manuscript (August 2023).

For BibTeX users:

```
@unpublished{Mehlhaff2023,
  type = {Book {{Manuscript}}},
  title = {Mass {{Polarization}} across {{Time}} and {{Space}}},
  author = {Mehlhaff, Isaac D.},
  year = {2023},
  month = aug,
  address = {{The University of North Carolina at Chapel Hill}}
}
```

## Usage

Most users of this package will be primarily interested in using it to easily retrieve polarization estimates for a defined set of countries and years. For example, let us take the countries and years generated by the following:

``` r
countries <- rep(c("AUS", "MEX", "USA"), each = 2)
years <- rep(c(2018, 2019), 3)
data <- as.data.frame(cbind(countries, years))
```

### Retrieve Data Frame

One way to retrieve polarization estimates for the countries and years generated above is to pass the vectors of countries and years to ```get.PolarCAP()``` or ```melt.PolarCAP()```, which return wide and tidy data frames, respectively:

```{r}
get.PolarCAP(countries = countries, years = years, type = c("ideology", "affect"))
melt.PolarCAP(countries = countries, years = years, type = c("ideology", "affect"))
```

Standard errors of polarization estimates can be retrieved by adding the optional ```include.se``` argument, which defaults to ```FALSE```:

```{r}
get.PolarCAP(countries = countries, years = years, type = c("ideology", "affect"), include.se = TRUE)
melt.PolarCAP(countries = countries, years = years, type = c("ideology", "affect"), include.se = TRUE)
```

### Retrieve Individual Values

Another way to retrieve polarization estimates for the same set of countries and years is to return only the scalar polarization estimate associated with each country-year. This feature is particularly useful if a user has an imbalanced time series, would prefer to avoid merging data frames, or is working within a ```dplyr``` pipeline:

```{r}
data$ideology1 <- apply(data, 1, function(x) get.PolarCAP(countries = x[1], years = x[2], type = "ideology", value.only = TRUE))
data <- rowwise(data) %>% mutate(ideology2 = get.PolarCAP(countries = countries, years = years, type = "ideology", value.only = TRUE))
data
```

### A Note on Country Identifiers

The countries defined above are in ISO 3166-1 alpha-3 country code format. This is the preferred input for ```*.PolarCAP()```. However, these functions will accept country names in almost any language or format and attempt to convert them to ISO3 codes by calling ```to.ISO3()```. ```*.PolarCAP()``` will alert the user to any country names still unrecognized after this conversion and return results only for those which are recognized. In addition, country codes are case-insensitive. Thus, the following all return the same output:

```{r}
get.PolarCAP(countries = c("AUS", "MEX", "USA"), years = 2019, type = "ideology")
get.PolarCAP(countries = c("aus", "MEX", "united states"), years = 2019, type = "ideology")
get.PolarCAP(countries = c("aus", "MEX", "Stati Uniti"), years = 2019, type = "ideology")
```

