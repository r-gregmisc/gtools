
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtools

The `gtools` R package provides functions to assist in R programming,
including:

-   assist in developing, updating, and maintaining R and R packages
    (`ask`, `checkRVersion`, `getDependencies`, `keywords`, `scat`),
-   calculate the logit and inverse logit transformations (`logit`,
    `inv.logit`),
-   test if a value is missing, empty or contains only NA and NULL
    values (`invalid`),
-   manipulate R’s .Last function (`addLast`),
-   define macros (`defmacro`),
-   detect odd and even integers (`odd`, `even`),
-   convert strings containing non-ASCII characters (like single quotes)
    to plain ASCII (`ASCIIfy`),
-   perform a binary search (`binsearch`),
-   sort strings containing both numeric and character components
    (`mixedsort`),
-   create a factor variable from the quantiles of a continuous variable
    (`quantcut`),
-   enumerate permutations and combinations (`combinations`,
    `permutation`),
-   calculate and convert between fold-change and log-ratio
    (`foldchange`, `logratio2foldchange`, `foldchange2logratio`),
-   calculate probabilities and generate random numbers from Dirichlet
    distributions (`rdirichlet`, `ddirichlet`),
-   apply a function over adjacent subsets of a vector (`running`),
-   modify the TCP\_NODELAY (`de-Nagle`) flag for socket objects,
-   efficient `rbind` of data frames, even if the column names
    don`t match (`smartbind\`),
-   generate significance stars from p-values (`stars.pval`),
-   convert characters to/from ASCII codes (`asc`, `chr`),
-   convert character vector to ASCII representation (`ASCIIfy`).

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the released version of gtools from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gtools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("r-gregmisc/gtools")
```
