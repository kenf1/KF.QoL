#' The Need for Speed: Sole Import
#'
#' Import specified file with all cols imported as class char to avoid most formatting issues.
#'
#' Requires the following packages in addition to base R packages: `tidyverse`, `data.table`, `read_xl`.
#'
#' Currently supported formats are: `.csv`, `.xls`, `.xlsx`
#'
#' @param File File Path + Name + Extension of file.
#' @param fread (`TRUE`/`FALSE`) If `TRUE`, will use fread to read as DT. Although it is faster, this option is only available for csv format only. Default is `FALSE`. If `TRUE`, will replace all "" with `NA`.
#' @param xlsheet Which Excel sheet to read and import. Default is `"1"`.
#'
#' @return Single DF/DT containing contents of imported file.
#' @export
#'
#' @examples

soleImport <- function(File,fread=FALSE,xlsheet=1){
  FileExtension <- stringr::str_extract(File,"\\.([A-z]+)$")
  if(FileExtension==".csv" & fread==TRUE){
    temp <- data.table::fread(File,colClasses=c("character"))
    temp[temp==""] <- NA
  }else if(FileExtension==".csv" & fread==FALSE){
    temp <- readr::read_csv(File,col_types=cols(.default="c"))
  }else if(FileExtension==".xls"|FileExtension==".xlsx"){
    temp <- readxl::read_excel(File,sheet=xlsheet,col_types="text")
  }
  return(temp)
}
