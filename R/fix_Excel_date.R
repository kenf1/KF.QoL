#' Fix Excel date format issue
#'
#' Converts Excel date to a format R/Lubridate can work with.
#'
#' @param value Excel date entry.
#'
#' @return Tidy date format in form of `%m/%d/%y`. Can further modify using Lubridate package.
#' @export
#'
#' @examples

fix_Excel_date <- function(value){
  format(as.Date(as.Date("1899-12-30")+value,"%m/%d/%y"),"%m/%d/%y")
}
