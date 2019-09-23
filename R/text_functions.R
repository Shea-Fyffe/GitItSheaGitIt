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
get_pdf_text <- function(.dir = getwd(), clean = TRUE, ...) {
  .paths <- tryCatch(
    list.files(.dir, pattern = "\\.pdf$"),
    error = function(err) {
      NA
    }
  )
  if (!length(.paths) || is.na(.paths)) {
    stop(sprintf("No valid PDF files found in %s", dir))
  }
  .fuzzy <- list(...)
  if (!!length(.fuzzy)) {
    .fuzzy <- paste(.fuzzy, collapse = "|")
    if (any(grepl(.fuzzy, x = .paths, ignore.case = TRUE))) {
      .paths <- .paths[grepl(.fuzzy, x = .paths, ignore.case = TRUE)]
    }
  }
  .pdfs <- lapply(.paths, pdftools::pdf_text)
  if (clean) {
    .pdfs <- lapply(.pdfs, clean_pdf)
  }
  return(.pdfs)
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
synonym_match <-
  function(x,
           POS = "ADJECTIVE",
           dictionary = "C:\\Program Files (x86)\\WordNet\\2.1\\dict",
           drop = TRUE) {
    .home <- gsub("*\\\\dict", "", dictionary)
    Sys.setenv(WNHOME = .home)
    wordnet::setDict(dictionary)
    .syn <-
      sapply(x, function(x)
        wordnet::synonyms(word = x, pos = POS))
    .syn <- reshape2::melt(.syn, factorsAsStrings = FALSE)
    names(.syn) <- c("Synonym", "Word")
    .syn[, 1] <- gsub("*\\([^\\)]+\\)$", "", as.character(.syn[, 1]))
    .syn[, "match"] <- ifelse(.syn[, 1] != .syn[, 2], T, F)
    .syn <- .syn[.syn[, "match"], ]
    if (drop) {
      .syn <- .syn[vec_grep(.syn[, 2], .syn[, 1], FALSE), ]
    }
    .syn
  }
#' @title Count common words between two vectors
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @param y character vector of words or sentences.
#' @param stopwords Logical. Remove stop words? Uses [tm::stopwords]
#' @param stem Logical. Stem words? Uses [textstem::stem_word]
#' @import tm textstem
#' @export
count_words <- function(x, y, stopwords = TRUE, stem = FALSE) {
  stopifnot(is.character(x), is.character(y))
  if (stopwords) {
      sw <- paste(tm::stopwords("en"), collapse = "\\b|\\b")
      sw <- paste0("\\b", sw, "\\b")
      .x <- gsub(sw, " ", x)
      .y <- gsub(sw, " ", y)
    }
    if (stem) {
      x <- textstem::stem_words(x, "en")
    }
    l <- sapply(list(unique(.x), unique(.y)), clean_text)
    l <- sapply(l, function(x) strsplit(x, split = " "))
    res <- sapply(l[[1]], function(x) {
      res <- sapply(l[[2]], function(y) {
        n <- .count_words(x, y)
        n
      })
    })
    res <- as.data.frame(res)
    names(res) <- unique(x)
    res[, "doc_y"] <- unique(y)
    res <- tidyr::gather_(res, "doc_x", "common_word_count", 
                          names(res)[names(res) != "doc_y"], na.rm = T)
    return(res)
}
#' @title Wrap Text Function
#' @export
wrap_text <- function(txt, pattern) {
  if (any(nchar(txt) == 0L)) {
    txt <- txt[!nchar(txt) == 0L]
  }
  lines <- grep(pattern, txt)
  remove <- setdiff(seq(txt), lines)
  d <- diff(lines)
  spread <- unique(d)[order(unique(d), decreasing = TRUE)]
  for (i in seq(spread)) {
    wrap <- lines[d == spread[i]]
    if (spread[i] != 1L && (spread[i] - 1L) %in% spread) {
      txt[wrap] <- paste(txt[wrap], txt[wrap + 1L], sep = " ")
    } else if (spread[i] == 1L) {
      txt[wrap] <- txt[wrap]
    } else {
      txt[wrap + 1L] <- NA
    }
  }
  txt <- txt[-remove]
  return(txt)
}
#' @title Clean text from character vector
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @import qdap
#' @export
clean_text <- function(x) {
  if (typeof(x) != "character") {
    stop("Please define x as a character")
  }
  x <- gsub("[^[:alnum:]\\-\\'\\s\\.]", " ", x)
  x <- gsub("\\.$", "", x)
  if (any(grepl("\\.", x))) {
    message("text contains '.' not at end splitting data")
    x <- strsplit(x, ".")
    x <- unlist(x)
  }
  x <- gsub("\\s+", " ", x)
  if (any(grepl("[0-9]", x))) {
    x <- qdap::replace_number(x)
    x <- qdap::replace_ordinal(x)
  }
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- tolower(x)
  return(x)
}
#' @title Capture text between two characters
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @param between character vector of length 2 containing boundary characters 
extract_text_between <- function(x, between = c(".*", ".*")) {
  .pattern <- sprintf("%s(.*?)%s", between[1], between[2])
  .x <- regmatches(x, regexec(.pattern, x))
  return(.x)
}
