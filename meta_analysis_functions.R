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
get_pdf_text<- function(.dir = getwd(), clean = TRUE, ...) {
  .paths <- tryCatch(list.files(.dir, pattern = "\\.pdf$"),
                     error = function(err) {NA})
  if(!length(.paths)||is.na(.paths)) {
    stop(sprintf("No valid PDF files found in %s", dir))
  }
  .fuzzy <- list(...)
  if(!!length(.fuzzy)){
    .fuzzy <- paste(.fuzzy, collapse = "|")
    if(any(grepl(.fuzzy, x = .paths, ignore.case = TRUE))) {
      .paths <- .paths[grepl(.fuzzy, x = .paths, ignore.case = TRUE)]
    }
  }
    .pdfs <- lapply(.paths, pdftools::pdf_text)
    if(clean) {
      .pdfs <- lapply(.pdfs, clean_pdf)
    }
    return(.pdfs) 
}
clean_pdf <- function(x) {
  x <- gsub("[[:punct:]]", "", x)
  # remove extra white spaces
  x <- gsub("[ \t]{2,}", " ", x)
  x <- gsub("[\r\n]", "", x)
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

#' @title Identify synonyms using wordnet
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of situational adejectives
#' @param POS part-of-speech to be identified
#' @param dictionary a file path wordnet dictionary
#' @param drop if false, will return all synonyms identified
#' @seealso [wordnet::setDict()]
#' @import reshape2 wordnet
#' @export
synonym_match <- function(x, POS = "ADJECTIVE", dictionary = "C:\\Program Files (x86)\\WordNet\\2.1\\dict", drop = TRUE) {
  .home <- gsub("*\\\\dict", "", dictionary)
  Sys.setenv(WNHOME = .home)
  wordnet::setDict(dictionary)
  .syn <- sapply(x, function(x) wordnet::synonyms(word = x, pos = POS))
  .syn <- reshape2::melt(.syn, factorsAsStrings = FALSE)
  names(.syn) <- c("Synonym", "Word")
  .syn[,1] <- gsub("*\\([^\\)]+\\)$","",as.character(.syn[,1]))
  .syn[,"match"] <- ifelse(.syn[,1]!=.syn[,2], T,F)
  .syn <- .syn[.syn[,"match"],]
  if(drop) {
    .syn <- .syn[vec_grep(.syn[,2],.syn[,1], FALSE),]
  }
  .syn
}




