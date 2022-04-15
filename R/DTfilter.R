#' The Need for Speed: Fast Subset
#'
#' Fast subset of DF cols using DT tools.
#'
#' @param dfName Input DF.
#' @param purpose (`"keep"`/`"remove"`) Use `"keep"` to keep and `"remove"` to remove the specified cols.
#' @param varName Names of vars. Can be in form of : col num, string, or list of strings.
#' @param toDF (`TRUE`/`FALSE`) If `TRUE`, will convert back to class DF Default is `FALSE`.
#'
#' @return Either DF or DT & keep or remove specified cols.
#' @export
#'
#' @examples

DTfilter <- function(dfName,purpose,varName,toDF=FALSE){
  temp <- data.table::setDT(dfName)
  if(purpose=="keep"){ #conditional for purpose
    temp <- temp[,varName,with=FALSE]
  }else if(purpose=="remove"){
    temp <- temp[,!varName,with=FALSE]
  }else if(purpose!="keep"|purpose!="remove"){
    stop("Instructions not clear, please try again")
  }
  if(toDF==TRUE){ #conditional for output format
    data.table::setDF(temp)
  }else{
    data.table::setDT(temp)
    message("Currently set to default of class `data.table`.")
  }
  return(temp)
}
