#' Levels within specified DF$var (Depreciated)
#'
#' Show/Count number of unique levels within specified DF$var.
#'
#' For DF nested within a list, use `$` notation.
#'
#' @param dfName Input DF.
#' @param varName Input var.
#' @param showLevels (`TRUE`/`FALSE`) If `TRUE`, will print out all levels. If `FALSE`, will only print out length. Default is `TRUE`.
#'
#' @return Prints either all unique levels within specified DF$var or just the length in console.
#' @export
#'
#' @examples

singleLvls <- function(dfName,varName,showLevels=TRUE){
  if(showLevels==TRUE){
    unique(dfName[[varName]])
  }else{
    length(unique(dfName[[varName]]))
  }
}
