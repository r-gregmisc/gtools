#!Rscript
# 
# Run revdep_check() to run R CMD check on all reverse dependencies of this package,
# utilizing the number of (virtual) cores for the number of concurrent workers.
#
# Run this script in a terminal to perform the checks, and then use 
#  - revdepcheck::revdev_summary() to get a status report
#  - revdep_details(revdep = "pkg") to see the details for a specific package
#

revdepcheck::revdep_check(num_workers=parallel::detectCores(), bioc=TRUE )

