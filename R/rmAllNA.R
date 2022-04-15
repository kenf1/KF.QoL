#' Out of sight, out of mind
#'
#' Why waste storage saving a bunch of unnecessary all NA columns when you can remove it to save on computation time and costs? This function removes all cols where everything is NA.
#'
#' User needs to manually save results.
#'
#' @param dfName Input DF.
#'
#' @return DF where columns with 100% NA are removed.
#' @export
#'
#' @examples

rmAllNA <- function(dfName){
  dfName %>% dplyr::select_if(~!all(is.na(.)))
}
