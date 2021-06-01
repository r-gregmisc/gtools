#!Rscript
# 
# Run revdep_check() to run R CMD check on all reverse dependencies of this package,
# utilizing the number of (virtual) cores for the number of concurrent workers.
#

script_path <- file.path(gtools::script_path(), "..")
setwd(script_path)
getwd()

while (TRUE) 
{
  rs <- revdepcheck::revdep_summary() 
  print(rs)
  
  if(!askYesNo("Refresh?")) break()
}
