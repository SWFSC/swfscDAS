# swfscDAS 0.2 (in development)

* Added 'Data9', 'Data10', and 'Data12' columns to the output of `das_read` and `das_process` to account for all recorded sighting information. These data are now extracted as 'CalibSchool', 'PhotosAerial', and 'Biopsy' from "S" events in `das_sight`, and checked in `das_check`

* Added 'SpdKt' (ship speed in knots) and 'WindSpdKt' (wind speed in knots) to the output of `das_process`. The sources for these data are also checked in `das_check`

* Renamed items in effort list output of `das_effort` and `das_effort_sight`: 'siteinfo' is now 'sightinfo', and segdata columns indicating the number of sighitngs or animals on a segment are now formatted as 'description'_'species code', e.g. "nSI_075"

* Fixed a bug in `das_sight` when processing "G" and "g" events


# swfscDAS 0.1

* Initial version, presented to NMFS users
