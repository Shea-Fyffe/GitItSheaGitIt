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
  .pdfs <- list.files(.dir, pattern = ".pdf$")
  if(length(...)){
    .fuzzy <- list(...)
    .fuzzy <- paste(.fuzzy, collapse = "|")
    if(any(grepl(.fuzzy, x = .pdfs, ignore.case = TRUE))) {
      .pdfs <- pdfs[grepl(.fuzzy, x = .pdfs, ignore.case = TRUE)]
    } else {
      .pdfs <- .pdfs
    }
  }
  .res <- lapply(.pdfs, Rpdf)
  return(.res)
}
