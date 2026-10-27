# snail 0.1.1

* Added snail rhymes. Call snail_rhyme() to study them.
* Documentation updated and expanded.
* Added a list of weather stations used in precipitation model development.
  Call stations() to get the list.
* Plot_forecast uses now the user-defined p_column if available.
* A new function precipitation_pb added.
* rain_version() was replaced with precipitation_pb_summary()

* Enhancements on forecast plot
  * Removed 1000 from air pressure label.
  * Added wind direction labels.
  * Corrected time grid for daylight-saving transitions.
  * Added a small tick to indicate forecast positions.
  * Changed axis grids.
  * Scaled wind direction between 0 and 360 degrees and added breaks whenever 0 or 360 are crossed.
  * Vectorized location parameters to allow more than 1 location per function call.


# snail 0.1.0

* Initial release.
* Fetch and visualize weather forecasts.
* Provide rain-probability predictions using the included model.
