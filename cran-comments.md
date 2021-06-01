# cran-comments.md

This submission was generated using the `devtools` and `revdepcheck` packages, and was built and checked via `Rhub` and `winbuilder`.

It passes `R CMD check` on rhub:linux-release, rhub:linux-devel, rhub:Windows-release, and win-builder:Windows-devel.   (rhub:Windows-devel currently fails prior to building `gtools` due to the lack of BioConductor support for R 4.2.)

`devtools::check` shows one *expected* NOTE:  

> N  checking R code for possible problems (4s)
>    Found the following possibly unsafe calls:
>    File ‘gtools/R/unByteCode.R’:
>      unlockBinding(name, env = env)

This use of `unlockBinding` is key to the functions provided by this file:
`unByteCode(fun)`, `assignEdgewise(name, env, value)`, and `unByteCodeAssign(fun)` whose purpose is to allow a byte coded function to be converted back into a fully interpreted function as a temporary work around for issues in byte-code interpretation.  This requires the use of `unlockBinding` and has been part of the gtools package for a number of years.

