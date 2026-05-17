#' is.missing 
#' @description
#' Checks for validity of variable and path 
#' 
#' @param x  variable or path 
#' @param checkPath if to check for path
#'
#' @returns TRUE or FALSE
#'
#' @examples
is.missing  <- function(x, checkPath=T){
  if (is.null(x)) 
    return(TRUE)
  if (inherits(x, "try-error")) 
    return(TRUE)
  if (!is.atomic(x)) 
    return(FALSE)
  if (length(x) == 0) 
    return(TRUE)
  if (all(is.na(x))) 
    return(TRUE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) 
    return(TRUE) 
  if (is.logical(x) && !any(stats::na.omit(x))) 
    return(TRUE)
  if(checkPath){
    if (file.exists(x) || dir.exists(x)){
      return(FALSE) 
    } else { 
      return(TRUE)
    }
  }  
  return(FALSE)
}