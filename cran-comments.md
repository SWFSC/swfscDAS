## Resubmission
This is a second resubmission. In this version I have:
* The das_format_pdf example is uncommented (sorry) and uses interactive() as in base:files examples
* Made small documentation fixes

This is a resubmission. In this version I have:
* Changed a function name (see news), hence the version bump from 0.3.0 to 0.4.0
* Explained 'DAS' in the DESCRIPTION text
* Replaced \dontrun{} with \donttest{}, or unwrapped examples where appropriate
* Ensured that all examples and vignettes use only 1 core

## Release summary
Initial release (0.3.0)

## Test environments
* Windows 10, R 4.0.1 (local)
* win-builder (devel and release)
* OS X, R 4.0.1 (local)
* ubuntu 14.04.5 (on travis-ci.com, R devel, oldrel, and release)

## R CMD check results
There were no ERRORs, WARNINGs

There was one NOTE on win-builder: 

* "Possibly mis-spelled words in DESCRIPTION: DAS, SWFSC, das"

DAS and SWFSC are acronyms explained in DESCRIPTION, while .das refers to a file type

## Downstream dependencies
No downstream dependencies
