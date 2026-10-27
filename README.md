# snail

<!-- badges: start -->
<!-- badges: end -->

The **snail** package provides tools for visualizing weather forecasts in R.

The package takes its name from traditional nursery rhymes in which snails are invoked as weather prophets or rain-makers.
For an overview of this tradition and its references, see [The snail in nursery rhymes](https://conchsoc.org/interests/cochlea-liberum.php),
published by the Conchological Society of Great Britain and Ireland.

## Installation

Install the development version from GitHub using `pak`:

```r
# install.packages("pak")
pak::pak("Myllykoski/snail")
```

## Usage

The **snail** package provides three functions for working with weather forecasts:

- `fetch_forecast()` retrieves forecast data.
- `forecast()` preprocesses forecast data and predicts rain probabilities.
- `plot_forecast()` visualizes forecast data, optionally performing the preceding steps automatically.

### Retrieve a forecast

Use `fetch_forecast()` to retrieve raw forecast data for a location.

```r
library(snail)

data <- fetch_forecast(
  latitude = 65.0,
  longitude = 25.5,
  location = "Oulu"
)
```

### Predict rain probabilities

Use `forecast()` to preprocess the data and predict rain probabilities,

```r
pred <- forecast(
  data = data
)
```

The predicted probabilities are stored in the `rain.p` column by default. Use the `p_column` argument to specify a different column name.

### Plot a forecast

Use `plot_forecast()` to visualize forecast data. Supply previously processed data to avoid retrieving and processing the forecast again.

```r
plot_forecast(
  data = pred
)
```

Alternatively, let `forecast()` (or `plot_forecast()`) retrieve and process the forecast automatically:

```r
plot_forecast(
  latitude = 65.0,
  longitude = 25.5,
  location = "Oulu"
)
```

For details on the available arguments and returned values, see the documentation for [`fetch_forecast()`], [`forecast()`], and [`plot_forecast()`].



## Precipitation probability

Probability is estimated by a GLM model created by the author.
Precipitation observations are retrieved from
Finnish Meteorological Institute's open data using the `fmi2` package created by Joona Lehtomäki and
forecasts from MET as described above. 
Training, testing and proofing data consists ~120 weather stations all over in Finland.

See a summary:
```r
library(snail)

precipitation_model()
```


## Weather data

Weather forecast data are retrieved from the [MET Weather API](https://api.met.no/), provided by the Norwegian Meteorological Institute (MET Norway).

Please credit the source when redistributing or presenting data obtained through the API:

> Weather data from MET Norway.

See the [MET Norway licensing and attribution policy](https://docs.api.met.no/doc/License.html) for the applicable data licensing terms.

The `snail` package is an independent R package and is not an official MET Norway product.



## License

The `snail` R package is distributed under the MIT license. This license applies to the package's code, not automatically to data retrieved from external sources.
