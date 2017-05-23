ask <- function(msg="Press <RETURN> to continue: ", con=stdin())
        {
          cat(msg);
          readLines(con=con,n=1)
        }
