#' The Need for Speed: Bulk Import
#'
#' Mass import files from specified Path + File Extension. All cols imported as class char to avoid most formatting issues.
#'
#' Requires the following packages in addition to base R packages: `tidyverse`, `data.table`, `read_xl`.
#'
#' Currently supported formats are: `.csv`, `.xls`, `.xlsx`
#'
#' Function will stop and output error message if FileExtension if not from list of supported formats.
#'
#' @param FileExtension Supported format from list of currently supported formats. Also used to remove file extension from element name.
#' @param fread (`TRUE`/`FALSE`) If `TRUE`, will use fread to read as DT. Although it is faster, this option is only available for csv format only. Default is `FALSE`.
#' @param FolderPath Where target files are located. Default is `"./Input"`.
#' @param xlsheet Which Excel sheet to read and import. Default is `"1"`.
#'
#' @return List of DF or DT where each element of the list (same name as file name) contains the contents of a single imported file.
#' @export
#'
#' @examples

bulkImport <- function(FileExtension,fread=FALSE,FolderPath="./Input",xlsheet=1){
  files <- list.files(path=FolderPath,pattern=FileExtension,full.names=TRUE)
  temp_names <- stringr::str_split(files,"/",simplify=TRUE)
  names(files) <- temp_names[,ncol(temp_names)] %>%
    stringr::str_remove(FileExtension)
  if(purrr::is_empty(files)==TRUE){ #if no matching files of FileExtension are found
    stop("No files of this format type are found in the specified FolderPath")
  }else if(FileExtension==".csv" & fread==TRUE){
    temp <- lapply(files,function(x){
      data.table::fread(x,colClasses=c("character"),na.strings=c("","NA")) #read as DT (faster reads, but csv only)
    })
  }else if(FileExtension==".csv" & fread==FALSE){
    temp <- lapply(files,function(x){
      readr::read_csv(x,col_types=cols(.default="c")) #non-DT read
    })
  }else if(FileExtension==".xls"|FileExtension==".xlsx"){
    temp <- lapply(files,function(x){
      readxl::read_excel(x,sheet=xlsheet,col_types="text") #read Excel formats
    })
  }
  return(temp)
}
