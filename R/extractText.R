#' Extract Text from Image
#'
#' Uses [Google's OCR](https://github.com/tesseract-ocr/tesseract) (optical character recognition) engine to extract text from images. This function is just a wrapper of multiple steps found in [Tesseract documentation](https://docs.ropensci.org/tesseract/articles/intro.html).
#'
#' @param imagePath Path of image to extract text from.
#' @param ocrLang
#'
#' @return
#' @export
#'
#' @examples

extractText <- function(imagePath,ocrLang="eng"){
  library(tesseract)
  engineLang <- tesseract(ocrLang)
  text <- tesseract::ocr(imagePath,engine=engineLang)
  cat(text)
}
