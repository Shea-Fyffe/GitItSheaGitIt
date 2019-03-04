#' @title Convert PDF File to Text
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#'
#' @param .dir file directory of pdf(s)
#' @param ... a character vector to filter out PDF file names
#'
#' @examples \example{
#' \dontrun{
#'   \code{pdf_text_files <- get_pdf_text(.dir = "%systemdrive%/Documents and Settings/All Users/Desktop", "words in pdf file name")
#'         cat(pdf_text_files)}
#'         }
#' \dontrun{
#'   \code{pdf_text_files <- get_pdf_text(.dir = "%systemdrive%/Documents and Settings/All Users/Desktop", c("Employee", "Engagement"))
#'         cat(pdf_text_files)}
#'         }
#'            
#' @import pdftools readr
#' @export
get_pdf_text<- function(.dir = getwd(), ...){
  Rpdf <- function(.x){
    .x <- pdftools::pdf_text(.x)
    .x <- readr::read_lines(.x)
    return(.x)
  }
  Trim <- function(.x) {
    if(missing(.x)||!is.character(.x)) stop("Please define x as a character vector")
    .x <- gsub("\\s{2,}", " ", .x)
    .x <- gsub("^\\s+|\\s+$", "", .x)
    return(.x)
  }
  .pdfs <- list.files(.dir, pattern = ".pdf$")
  .fuzzy <- list(...)
  if(!!length(.fuzzy)){
    .fuzzy <- paste(.fuzzy, collapse = "|")
    if(any(grepl(.fuzzy, x = .pdfs, ignore.case = TRUE))) {
      .pdfs <- pdfs[grepl(.fuzzy, x = .pdfs, ignore.case = TRUE)]
    } else {
      .pdfs <- .pdfs
    }
  }
  .res <- lapply(.pdfs, Rpdf)
  .res <- lapply(.res, Trim)
  return(.res)
}
