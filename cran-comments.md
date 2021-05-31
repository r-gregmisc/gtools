# cran-comments.md

This submission was generated using the devtools and revdepcheck packages, and was built and checked via Rhub and winbuilder.

It passes R CMD check on linux-release, linux-devel, and Windows-release.   (Windows-devel currently fails due to the lack of BioConductor support for R 4.2.)

`devtools::check` shows one *expected* NOTE:  

> N  checking R code for possible problems (4s)
>    Found the following possibly unsafe calls:
>    File ‘gtools/R/unByteCode.R’:
>      unlockBinding(name, env = env)

This file provides three functions:
# nByteCode(fun)
# assignEdgewise(name, env, value)
# unByteCodeAssign(fun)

The purpose of these functions is to allow a byte coded function to be converted back into a fully interpreted function as a temporary work around for issues in byte-code interpretation.  This requires the use of `unlockBinding` and has been part of the gtools package for a number of years.

