#' @title Import a dataset
#'
#' @description
#' This \code{import} function can import data from
#' delimited text files, simple JSON files, EXCEL spreadsheets,
#' and statistical packages such as SAS, SPSS, and Stata.
#' This wonderful.
#'
#' @details
#' The import function is wrapper for the
#' \href{https://haven.tidyverse.org}{haven},
#' \href{https://readxl.tidyverse.org/}{readxl},
#' \href{https://cran.r-project.org/web/packages/rjson/index.html}{rjson}
#' and \href{https://github.com/r-lib/vroom}{vroom} packages.
#'
#' @note
#' Complex nested JSON files will not be imported properly.
#'
#' @seealso
#' \link[haven]{read_sas},
#' \link[haven]{read_dta},
#' \link[haven]{read_spss},
#' \link[readxl]{read_excel},
#' \link[rjson]{fromJSON},
#' \link[vroom]{vroom}
#'
#' @param file datafile to import.
#' @param ... parameters passed to the import function.
#'
#' @import haven
#' @import readxl
#' @import vroom
#' @import tools
#' @import rjson
#' @export
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' # import a comma delimited file
#' mydataframe <- import("mydata.csv")
#'
#' # import a SAS binary file
#' mydataframe <- import("mydata.sas7bdat")
#'
#' # import the second worksheet of an Excel workbook
#' mydataframe <- import("mydata.xlsx", sheet = 2)
#'
#' # prompt for a file to import
#' mydataframe <- import()
#' }


import <- function(file, ...){

  if (missing(file)){
    file <- file.choose()
  }

  if (!file.exists(file)){
    stop("File doesn't exist! Please choose another file")
  }

  extension <- tools::file_ext(file)
  extension <- tolower(extension)

  if(extension == "xls" | extension == "xlsx"){
    dataset <- readxl::read_excel(file, ...)
  }

  else if(extension == "sas7bdat") {
    dataset <- haven::read_sas(file, ...)
  }

  else if(extension =="sav") {
    dataset <- haven::read_sav(file, ...)
  }

  else if(extension == "dta") {
    dataset <- haven::read_dta(file, ...)
  }

  else if(extension == "json"){
    dataset <- rjson::fromJSON(file, ...)
    dataset <- as.data.frame(dataset)
  }

  else {
    dataset<- vroom::vroom(file, ...)
  }

  return(dataset)
}


