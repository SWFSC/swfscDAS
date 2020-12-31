## Release summary
Bug fixes and feature additions

## Test environments
* Windows 10, R 4.0.3 (local)
* win-builder (devel and release)
* OS X, R 4.0.3 (local)
* ubuntu 16.04 (on travis-ci.com, R devel, oldrel, and release)

## R CMD check results
There were no ERRORs, WARNINGs

There was one NOTE on win-builder: 

* "Possibly mis-spelled words in DESCRIPTION: DAS, SWFSC, das"

DAS and SWFSC are acronyms explained in DESCRIPTION, while .das refers to a file type

## Downstream dependencies
I have also run R CMD check on downstream dependencies of swfscDAS 
(https://github.com/smwoodman/checkresults/blob/master/swfscDAS/r-release). 
All packages passed
