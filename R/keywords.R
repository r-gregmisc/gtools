keywords <- function( topic )
{

  file <- file.path(R.home("doc"),"KEYWORDS")
  if(missing(topic))
      {
          file.show(file)
      }
  else
      {

          ## Local copy of trim.character to avoid cyclic dependency with gdata ##
          trim <-  function(s)
              {
                  s <- sub(pattern="^[[:blank:]]+", replacement="", x=s)
                  s <- sub(pattern="[[:blank:]]+$", replacement="", x=s)
                  s
              }

          kw <- scan(file=file, what=character(), sep="\n", quiet=TRUE)
          kw <- grep("&", kw, value=TRUE)
          kw <- gsub("&[^&]*$","", kw)
          kw <- gsub("&+"," ", kw)
          kw <- na.omit(trim(kw))

          ischar <- tryCatch(is.character(topic) && length(topic) ==
                             1L, error = identity)
          if (inherits(ischar, "error"))
              ischar <- FALSE
          if (!ischar)
              topic <- deparse(substitute(topic))

          item <- paste("^",topic,"$", sep="")

          topics <- function(k)
              {
                   matches <- help.search(keyword=k)$matches
                   matches[ , match("topic", tolower(colnames(matches)))]
              }
          matches <- lapply(kw, topics)
          names(matches) <- kw

          tmp <- unlist(lapply( matches, function(m) grep(item, m, value=TRUE) ))
          names(tmp)
      }
}
