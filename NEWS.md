# swfscDAS 0.2 (in development)

## General 

* Added 'Data9', 'Data10', and 'Data12' columns to the output of `das_read` and `das_process` to account for all recorded sighting information. These data are now extracted as 'CalibSchool', 'PhotosAerial', and 'Biopsy' from "S" events in `das_sight`, and checked in `das_check`

* Added 'SpdKt' (ship speed in knots) and 'WindSpdKt' (wind speed in knots) to the output of `das_process`. The sources for these data are also checked in `das_check`

* Renamed items in effort list output of `das_effort` and `das_effort_sight`: 'siteinfo' is now 'sightinfo', and segdata columns indicating the number of sighitngs or animals on a segment are now formatted as 'description'_'species code', e.g. "nSI_075"

## `das_sight` changes

* Fixed a bug when processing "G" and "g" events

* Added columns for high and low (in addition to best) estimates of school size

* Renamed columns to follow a consistent format - see `das_sight` documentation for a complete descrption of the various return format outputs

* Added a "complete" return format, which has a row for every observer estimate for each sighting

* Users can now use a `das_sight` argument to provide the event code(s) by which they wish to filter the function output


# swfscDAS 0.1

* Initial version, presented to NMFS users
