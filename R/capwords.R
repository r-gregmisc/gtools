capwords <- function(s, 
                     strict=FALSE, 
                     AP=TRUE, 
                     onlyfirst=FALSE, 
                     preserveMixed=FALSE, 
                     sep=" ") 
{
  # worker functions
  cap <- function(s) 
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2); 
            if(strict) tolower(s) else s
          },
          sep = ""
    )
  
  # test if there is a lowercase letter followed by an uppercase letter
  isMixedCase <- function(s) 
    grepl("[a-z][A-Z]", s)

  words <- unlist(strsplit(s, split = sep))
  mixedCaseFlag <- sapply(words, isMixedCase)
  
  # First, capitalize *every* word
  if(!onlyfirst)
  {
    newWords <- sapply(words, 
               cap, 
               USE.NAMES = !is.null(names(retval))
               ) 
    
    if(preserveMixed==TRUE)
      newWords[mixedCaseFlag] <- words[mixedCaseFlag]
    
    words <- newWords
  }
       
  # Next (optionally) uncapitalize prepositions and conjunctions
  # recommended by the Associated Press.
  AP.nocap <- c("a", "an", "and", "at", "but", "by", "for", "in", 
                "nor", "of", "on", "or", "so", "the", "to", "up", 
                "yet")
  
  if(AP && !onlyfirst) 
    for(word in AP.nocap)
      words <- gsub(paste0("^",word,"$"), 
                            word, 
                            words, 
                            ignore.case=TRUE)
                
  
  # Finally, ensure that the first word is capitalized
  if(length(words)>0 && mixedCaseFlag[1]==FALSE)
    words[1] <- cap(words[1])
  
  retval <- paste(words, collapse=sep)
  
  retval
}