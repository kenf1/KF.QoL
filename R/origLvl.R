#' Show/Count distinct levels
#'
#' A custom function/wrapper for unique() and length(unique()). Use-case depends on user. Can use `$` or `[[]]` notation.
#'
#' @param dfName Input DF. Can use `$` notation for both single DF or nested DF.
#' @param varName Can be used to pass either var name or col num. Default is NULL.
#' @param length (TRUE/FALSE) If FALSE, will print out all levels. If TRUE, will only print out length. Default is FALSE.
#'
#' @return Prints either all unique levels within specified DF$var or just the length in console.
#' @export
#'
#' @examples

origLvl<- function(dfName,varName=NULL,length=FALSE){
  if(is.null(varName)==TRUE & length==FALSE){
    unique(dfName)
  }else if(is.null(varName)==TRUE & length==TRUE){
    length(unique(dfName))
  }else if(is.null(varName)==FALSE & length==FALSE){
    unique(dfName[[varName]])
  }else if(is.null(varName)==FALSE & length==TRUE){
    length(unique(dfName[[varName]]))
  }
}
