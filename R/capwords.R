#' Capitalize Words for Titles
#'
#' This function capitalizes words for use in titles
#'
#' This function separates the provided character string into separate words
#' using \code{sep} as the word separator.  If \code{firstonly==TRUE}, it then
#' capitalizes the first letter the first word, otherwise (the default), it
#' capitalizes the first letter of every word.  If \code{AP==TRUE}, it then
#' un-capitalizes words in the Associated Press's (AP) list of prepositions and
#' conjunctions should not be capitalized in titles.  Next, it capitalizes the
#' first word. It then re-joins the words using the specified separator.
#'
#' If \code{preserveMixed==TRUE}, words with an upper-case letter appearing
#' after a lower-case letter will not be changed (e.g. "iDevice").
#'
#' @param s character string to be processed
#' @param strict Logical, remove all additional capitalization.
#' @param AP Logical, apply the Associated Press (AP) rules for prepositions
#' and conjunctions that should not be capitalized in titles.
#' @param onlyfirst Logical, only capitalize the first word.
#' @param preserveMixed Logical, preserve the capitalization mixed-case words
#' containing an upper-case letter after a lower-case letter.
#' @param sep Character string, word separator
#' @return A character scalar containing the capitalized words.
#' @author Gregory R. Warnes \email{greg@@warnes.net} based on code from the
#' \code{\link[base]{chartr}} manual page, and
#' \code{\link[taxize]{taxize_capwords}} in the taxize package.
#' @seealso \code{\link[base]{chartr}}, \code{\link[taxize]{taxize_capwords}},
#' \code{\link[SGP]{capwords}}
#' @references Fogarty, Mignon. Capitalizing Titles: "Which words should you
#' capitalize? Grammar Girl's Quick and Dirty Tips for Better Writing. 9 Jun.
#' 2011. Quick and Dirty Tips Website." Accessed 22 April 2016
#' \url{https://www.quickanddirtytips.com/education/grammar/capitalizing-titles}
#' @keywords utilites character
#' @examples
#'
#' capwords("a function to capitalize words in a title")
#' capwords("a function to capitalize words in a title", AP = FALSE)
#'
#' capwords("testing the iProduct for defects")
#' capwords("testing the iProduct for defects", strict = TRUE)
#' capwords("testing the iProduct for defects", onlyfirst = TRUE)
#' capwords("testing the iProduct for defects", preserveMixed = TRUE)
#'
#' capwords("title_using_underscores_as_separators", sep = "_")
#' @export
capwords <- function(s,
                     strict = FALSE,
                     AP = TRUE,
                     onlyfirst = FALSE,
                     preserveMixed = FALSE,
                     sep = " ") {
  # worker functions
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = ""
    )
  }

  # test if there is a lowercase letter followed by an uppercase letter
  isMixedCase <- function(s) {
    grepl("[a-z][A-Z]", s)
  }

  words <- unlist(strsplit(s, split = sep))
  mixedCaseFlag <- sapply(words, isMixedCase)

  # First, capitalize *every* word
  if (!onlyfirst) {
    newWords <- sapply(
      words,
      cap
    )

    if (preserveMixed == TRUE) {
      newWords[mixedCaseFlag] <- words[mixedCaseFlag]
    }

    words <- newWords
  }

  # Next (optionally) uncapitalize prepositions and conjunctions
  # recommended by the Associated Press.
  AP.nocap <- c(
    "a", "an", "and", "at", "but", "by", "for", "in",
    "nor", "of", "on", "or", "so", "the", "to", "up",
    "yet"
  )

  if (AP && !onlyfirst) {
    for (word in AP.nocap) {
      words <- gsub(paste0("^", word, "$"),
        word,
        words,
        ignore.case = TRUE
      )
    }
  }


  # Finally, ensure that the first word is capitalized
  if (length(words) > 0 && mixedCaseFlag[1] == FALSE) {
    words[1] <- cap(words[1])
  }

  retval <- paste(words, collapse = sep)

  retval
}
