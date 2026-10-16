# snail

<!-- badges: start -->
<!-- badges: end -->

The **snail** package provides tools for retrieving, processing, and visualizing weather forecasts in R.

## Installation

Install the development version from GitHub using `pak`:

```r
# install.packages("pak")
pak::pak("Myllykoski/snail")
```

## Example

Load the package and explore a weather forecast:

```r
library(snail)

forecast <- fetch_forecast(latitude = 60.2665, longitude = 24.896)
time <- format(Sys.time(), "%y-%m-%d %H:%M")
plot_forecast(data = forecast, title = paste("Weather forecast for Kuninkaantammi, Helsinki at", time))
```

## Weather data

Weather forecast data are retrieved from the [MET Weather API](https://api.met.no/), provided by the Norwegian Meteorological Institute (MET Norway).

Please credit the source when redistributing or presenting data obtained through the API:

> Weather data from MET Norway.

See the [MET Norway licensing and attribution policy](https://docs.api.met.no/doc/License.html) for the applicable data licensing terms.

The `snail` package is an independent R package and is not an official MET Norway product.

## License

The `snail` R package is distributed under the MIT license. This license applies to the package's code, not automatically to data retrieved from external sources.
