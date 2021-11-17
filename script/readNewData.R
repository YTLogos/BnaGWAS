#' Title
#'
#' @param fileinfo
#'
#' @return
#' @export
#'
#' @examples
readNewData <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "rapeseed_hair.txt", size = 1, type = "text/txt", datapath = "data/rapeseed_hair.txt")
  }
  ext <- tools::file_ext(fileinfo$datapath)
  switch(ext,
         csv=vroom::vroom(file = fileinfo$datapath, col_names = FALSE),
         txt=vroom::vroom(file = fileinfo$datapath, col_names = FALSE),
         xlsx=openxlsx::read.xlsx(fileinfo$datapath, colNames = FALSE,sheet = 1),
         validate("Invalid file; please upload a .txt or .csv or .xlsx file")
         )
}

readNewData_gene <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "darmor_test_gene.txt", size = 1, type = "text/txt", datapath = "data/Other_data/darmor_test_gene.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE)
  return(newdata)
}

readNewData_sample <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "Core.var.txt", size = 1, type = "text/txt", datapath = "data/Other_data/Core.var.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE)
  return(newdata)
}