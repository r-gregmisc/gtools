gtools 3.9.4
-------------------------

- correct `stars.pval` code/doc mismatch (GH #13)
- remove spurious `browser()` call in `lastAdd`

gtools 3.9.3 - 2022-07-08
-------------------------

- maintainer switch to Ben Bolker 
- removed `assignEdgewise`/`unByteCodeAssign` (uses CRAN-deprecated `unlockBindings()` call)

gtools 3.9.2 - 2021-06-03
-------------------------

Bug fixes:

- Fix missing man page and export for `combinations` and `permutations`.

Behind the scenes:

- Fixed more spelling/typographical errors, mostly in `NEWS.md`.

- Speed up `checkRversion` by removing checks for versions 2.x and 3.x.

gtools 3.9.1 - 2021-06-01
-------------------------

Bug fixes:

- Use valid HTTP request for example in `setTCPNoDelay` to prevent
  errors when running tests.

Behind the scenes:

- Fixed numerous spelling/typographical errors.

- Update obsolete http URLs to https 

gtools 3.9.0 - 2021-05-31
-------------------------

New functions:

- New `script_file` and `script_path` functions to obtain the 
  directory or full path to the currently executing script.

- New 'stat_mode` function to calculate the statistical mode (most
  frequently occurring value).

- New `capwords` function to apply title capitalization rules to a
  character vector.
  
- Move `baseOf` from `gplots` as requested by Steffen Möller. #2 

New parameters:

- Add `scientific` parameter to `mixedsort` and `mixedorder` to 
  control whether numbers in scientific notation are recognized. 
  Resolved #7.
  
- Enhance `invalid` to detect `try-error` objects. #6

Bug fixes:

- Add support for R version 4 to `checkRVersion`.  Resolved #5.

- Correct bug in `lastAdd` by explicitly checking for a `.Last` of
  mode function.

Behind the scenes:

- Modernize package code by using `roxygen2` for documentation and 
  managing the NAMESPACE.

- Modernize C function registration. 

- Replace http URLs with `https` and resolve broken links.

- Add github actions to automated testing

- Use pkgdown to generate HTML documentation.

- Use `styler` package to standardize R code formatting.


gtools 3.8.2 - 2020-03-23
-------------------------

Minor changes to support R 4.0

gtools 3.8.1 - 2018-06-21
-------------------------

Behind the scenes:

- Remove softlinks per request from Uwe Ligges

gtools 3.8.0 - 2018-06-20
-------------------------

New functions:

- spit_path(): converts a file path into a vector of path components
- baseOf(): Transform an integer to an array of base-n digits

Behind the scenes:

- Update C calls to use correct 'PACKAGE=' parameter.
- Explicitly register C routines used by the package
- Update link for taxise::taxize_capwords in gtools::capwords man page
- Corrections to typographical errors

gtools 3.7.0 - 2017-06-14
-------------------------

New functions:

- Add capwords() function to apply standard capitalization rules
  to a character string.

Enhancements:

- Add 'con' argument to ask() to allow specification of the
  connection to query for input.  For use under RStudio, use
  ask(..., con=file('stdin')).

- R/na.replace.R, man/na.replace.Rd: na.replace() now
	accepts a function to provide the replacement value.

- smartbind() has a new argument 'list' to pass a list of data frames,
  /instead of/in addition to/ data frames as arguments.

- Internal changes to bring code up to current CRAN guidelines.

Bug Fixes:

- smartbind() now works properly with empty column names

- Correct error in smartbind() when column types don't
	match.

- Fix bug in smartbind's handling of factor levels.

- Improve assignment of default names in smartbind().

- loadedPackages() to return data silently so that the results
	don't get printed twice.


gtools 3.5.0 - 2015-04-28
-------------------------

New Functions:

- New roman2int() function to convert roman numerals to integers
  without the range restriction of utils::as.roman().

- New asc() and chr() functions to convert between ASCII codes and
  characters. (Based on the 'Data Debrief' blog entry for 2011-03-09
  at http://datadebrief.blogspot.com/2011/03/ascii-code-table-in-r.html).

- New unByteCode() and unByteCodeAssign() functions to convert a
  byte-code function to an interpreted code function.

- New assignEdgewise() function for making assignments into locked
  environments. (Used by unByteCodeAssign().)

Enhancements:

- mixedsort() and mixedorder() now have arguments 'decreasing',
  'na.last', and 'blank.last' arguments to control sort ordering.

- mixedsort() and mixedorder() now support Roman numerals via the
  arguments 'numeric.type', and 'roman.case'.  (Request by David
  Winsemius, suggested code changes by Henrik Bengtsson.)

- speed up mixedorder() (and hence mixedsort()) by moving
  suppressWarnings() outside of lapply loops. (Suggestion by Henrik
  Bengtsson.)

- The 'q' argument to quantcut() now accept an integer
  indicating the number of equally spaced quantile groups to
  create. (Suggestion and patch submitted by Ryan C. Thompson.)

Bug fixes:

- Removed stray browser() call in smartbind().

- ddirichlet(x, alpha) was incorrectly returning NA when for any i,
  x[i]=0 and alpha[i]=1.  (Bug report by John Nolan.)

Other changes:

- Correct typographical errors in package description.


gtools 3.4.2 - 2015-04-06
-------------------------

New features:

- New function loadedPackages() to display name, version, and path of
  loaded packages (package namespaces).

- New function: na.replace() to replace missing values within a
  vector with a specified value.`

Bug fixes:

- Modify keywords() to work properly in R 3.4.X and later.


gtools 3.4.1 - 2014-05-27
-------------------------

Bug fixes:

- smartbind() now converts all non-atomic type columns (except factor)
  to type character instead of generating an opaque error message.

Other changes:

- the argument to ASCIIfy() is now named 'x' instead of 'string'.

- minor formatting changes to ASCIIfy() man page.

gtools 3.4.0 - 2014-04-14
-------------------------

New features:

- New ASCIIfy() function to converts character vectors to ASCII
  representation by escaping them as \x00 or \u0000 codes.
  Contributed by Arni Magnusson.


gtools 3.3.1 - 2014-03-01
-------------------------

Bug fixes:

- 'mixedorder' (and hence 'mixedsort') not properly handling
  single-character strings between numbers, so that '1a2' was being
  handled as a single string rather than being properly handled as
  c('1', 'a', '2').



gtools 3.3.0 - 2014-02-11
-------------------------

New features:

- Add the getDependencies() function to return a list of dependencies
  for the specified package(s).  Includes arguments to control whether
  these dependencies should be constructed using information from
  locally installed packages ('installed', default is TRUE), available
  CRAN packages ('available', default is TRUE) and whether to include
  base ('base', default=FALSE) and recommended ('recommended', default
  is FALSE) packages.

Bug fixes:

- binsearch() was returning the wrong endpoint & value when the found
  value was at the upper endpoint.

gtools 3.2.1 - 2014-01-13
-------------------------

Bug fixes:

- Resolve circular dependency with gdata


gtools 3.2.0 - 2014-01-11
-------------------------

New features:

- The keywords() function now accepts a function or function name as
  an argument and will return the list of keywords associated with the
  named function.

- New function stars.pval() which will generate p-value significance
  symbols ('***', '**', etc.)

Bug fixes:

- R/mixedsort.R: mixedorder() was failing to correctly handle numbers
  including decimals due to a faulty regular expression.

Other changes:

- capture() and sprint() are now defunct.


gtools 3.1.1 - 2013-11-06
-------------------------

Bug fixes:

- Fix problem with mixedorder/mixedsort when there is zero or one
  elements in the argument vector.


gtools 3.1.0 - 2013-09-22
-------------------------

Major changes:

- The function 'addLast()' (deprecated since gtools 3.0.0) is no
  longer available, and has been marked defunct.

Bug fixes:

- Modified 'mixedorder()' to use Use 'suppressWarnings() instead of
  'options(warn=-1)'.  This will avoid egregious warning messages when
  called from within a nested environment, such as when run from
  within 'knitr'


gtools 3.0.0 - 2013-07-06
-------------------------

Major changes:

- The function 'addLast()' has been deprecated because it directly
  manipulates the global environment, which is expressly prohibited by
  the CRAN policies.

- A new function, 'lastAdd()' has been created to replace 'addLast()'.
  The name has been changed because the two functions require
  different syntax. 'addLast()' was used like this:

    byeWorld <- function() cat("\nGoodbye World!\n")
    addLast(byeWorld)

  The new 'lastAdd()' function is used like this:

    byeWorld <- function() cat("\nGoodbye World!\n")
    .Last <- lastAdd(byeWorld)

Bug fixes:

- Update checkRVersion() to work with R version 3.0.0 and later.

Other changes:

- Remove cross-reference to (obsolete?) `moc` package

- The function 'assert()' (deprecated since gtools 2.5.0) is no longer
  available and has been marked defunct.

gtools 2.7.1 - 2013-03-17
-------------------------

Bug fixes:

- smartbind() was not properly handling factor columns when the first
  data frame did not include the relevant column.

gtools 2.7.0 - 2012-06-19
-------------------------

New features:

- smartbind() has a new 'sep' argument to allow specification of the
  character(s) used to separate components of constructed column names

- smartbind() has a new 'verbose' argument to provide details on how
  columns are being processed

Bug fixes:

- smartbind() has been enhanced to improve handling of factor and
  ordered factor columns.


gtools 2.6.2 - 2011-09-28
-------------------------

New features:

- Add 'fill' argument to smartbind() to specify a value to use for
  missing entries.

gtools 2.6.1
------------

New features:

- Add newVersionAvailable() function to compare running and latest
  available R versions.

- Add keywords() function to show $RHOME/doc/KEYWORDS file

Bug fixes:

- Correct windows make flags as suggested by Brian Ripley.

- Update Greg's email address and fix Rd syntax errors


gtools 2.5.0
------------

New features:

- Add checkRVersion() function to determine if a newer version of R is
  available.

- Deprecated assert() in favor of base::stopifnot

Bug fixes:

- Fix bug in binsearch() identified by 2.6.0 R CMD CHECK

Other changes:

- Improve text explanation of how defmacro() and strmacro() differ from
  function().

- Update definitions of odd() and even() to use modulus operator
  instead of division.

gtools 2.4.0
------------

- Add binsearch() function, previously in the genetics() package.


gtools 2.3.1
------------

- Add ask() function to prompt the user and collect a single response.


gtools 2.3.0
------------

- Update email address for Greg

- Add new 'smartbind' function, which combines data frames
  efficiently, even if they have different column names.

gtools 2.2.3
------------

 - setTCPNoDelay now compiles & works properly on Windows


gtools 2.2.2
------------

 - src/setTCPNoDelay.c: Add C source code for setTCPNoDelay.

 - NAMESPACE: Add UseDynLib to NAMESPACE so the shared library gets
   properly loaded.

 - Updated Greg's email address.

gtools 2.2.1
------------

  - New function 'addLast' that adds functions to R's .Last() so
    that they will be executed when R is terminating.

  - New function setTCPNoDelay() that allows the TCP_NODELAY flag to
    be changed on socket objects.

gtools 2.1.0
------------

 - Added assert.R (and documentation)

 - Added the defmacro() function, extracted from Lumley T. "Programmer's Niche:
   Macros in {R}", R News, 2001, Vol 1, No. 3, pp 11--13,
   \url{http://CRAN.R-project.org/doc/Rnews/}

 - Added DESCRIPTION and removed DESCRIPTION.in

