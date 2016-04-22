na.replace <- function(x, replace, ...)
{
  if(is.function(replace))
    replace <- replace(x, ...)
  
  x[is.na(x)] <- replace
  x
}
