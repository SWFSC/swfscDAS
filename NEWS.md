# swfscDAS 0.4.0

* Changed `das_within_strata` to `das_intersects_strata` because it uses the geometric binary predicate `st_intersects` rather than `st_within`


# swfscDAS 0.3.0

* `das_process` now lets the user add the DateTime, Lat, and Lon information to "?" and numeric (1:8) events using the 'add.dtll.sight' argument. This enables the user to filter the processed DAS data by date/time or coordinates, if desired

* For consistency with species code columns, the column name 'SpProb' is now 'SpCodeProb' in `das_sight` output

* In `das_sight`, species-specific group sizes are now calculated as the mean of the product of the school estimate and the corresponding species percentage (for each estimate), rather than the product of the mean of the school estimate and the mean of the corresponding species percentage.

* `das_effort_sight` now has an argument that lets the user use the low group size estimate in place of an `NA` best group size estimate 

* Changed `das_chop_equal` to `das_chop_equallength` for naming consistency. This change should not affect users, as it does not affect any of the syntax required when using `das_effort`

* Added `das_within_strata` for checking if individual points, such as event points or segment midpoints, are within user-provided strata

* The "DAS_data_join" vignette demonstrates how to join external data, such as from DAT files, to processed DAS data

* Where applicable, error, warning, and informational messages now report both the file name and line number. This makes the messages more useful when working with concatenated DAS objects with multiple DAS files.


# swfscDAS 0.2.0

## General 

* Added 'Data9', 'Data10', and 'Data12' columns to the output of `das_read` and `das_process` to account for all recorded sighting information. These data are now extracted as 'CalibSchool', 'PhotosAerial', and 'Biopsy' from "S" events in `das_sight`, and checked in `das_check`

* Added 'SpdKt' (ship speed in knots) and 'WindSpdKt' (wind speed in knots) to the output of `das_process`. The sources for these data are also checked in `das_check`

* Renamed items in effort list output of `das_effort` and `das_effort_sight`: 'siteinfo' is now 'sightinfo', and segdata columns indicating the number of sightings or animals on a segment are now formatted as 'description'_'species code', e.g. "nSI_075"

## `das_sight` changes

* Fixed a bug when processing "G" and "g" events

* Added columns for high and low (in addition to best) estimates of school size

* Renamed columns to follow a consistent format - see `das_sight` documentation for a complete description of the various return format outputs

* Added a "complete" return format, which has a row for every observer estimate for each sighting

* Users can now use a `das_sight` argument to provide the event code(s) by which they wish to filter the function output


# swfscDAS 0.1.0

* Initial version, presented to NMFS users
