#' Fill in target DF using matches from another DF
#'
#' Very common occurrence. Very useful. 10/10, would recommend.
#'
#' @param ref_1 Matching var/entry in source DF.
#' @param ref_2 Matching var/entry in target DF.
#' @param fill_with Target DF.
#'
#' @return Target DF with additional content.
#' @export
#'
#' @examples

match_n_fill <- function(ref_1,ref_2,fill_with){
  fill_with[match(ref_2,ref_1)]
}
