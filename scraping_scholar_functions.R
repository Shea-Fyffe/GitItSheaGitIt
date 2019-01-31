#' @Title Scraping Google Scholar
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Wrapper function that collect data from a Google Scholar Search
#' @param ... String to be passed to Google Scholar search. Use \code{max = } and \code{year = } to restirct number of articles returned as well as year.
#' @examples \dontrun{
#'                    ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 250 
#'                    res <- scrape_scholar("Engagement", "organizational behavior", max = 250)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 
#'                    res <- scrape_scholar("Job satistfaction", year = 2010, max = 200)}
#'@export
scrape_scholar <- function(...) {
  .url <- build_search(...)
  .out <- list()
  for(i in seq(length(.url))) {
    .out[[i]] <- scrape(.url[i])
    Sys.sleep(sample(3, 1))
  }
  .out <- do.call("rbind", .out)
  return(.out)
}


#' @Title Build Search
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Helper function for scraping Google Scholar
#' @param ... String to be passed to Google Scholar search. Use \code{min =}, \code{max = } and \code{year = } to restirict number of articles returned as well as year.
#' @examples \dontrun{
#'                    ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 250 
#'                    res <- build_search("Engagement", "organizational behavior", max = 250)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satistfaction", year = 2010, min = 100, max = 200)}
#'@seealso \code{scrape_scholar}
#'@export
build_search <- function(...) {
  .search <- list(...)
  if("year" %in% names(.search)){
    .year <- .search[["year"]]
    .search[["year"]] <- NULL
  }
  if("max" %in% names(.search)){
    .max <- .search[["max"]]
    if(as.numeric(.max) %% 10 != 0) {
      stop("Please verify max is divisible by 10")
    }
    .search[["max"]] <- NULL
  }
  if("min" %in% names(.search)){
    .min <- .search[["min"]]
    if(as.numeric(.min) %% 10 != 0) {
      stop("Please verify min is divisible by 10")
    }
    .search[["min"]] <- NULL
  }
  .args <- c(exists(".year"),exists(".max"),exists(".min"))
  .search <- sapply(.search, utils::URLencode)
  .search <- paste(.search, collapse = "+")
  if(all(.args)) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%dhl=en&as_vis=1&as_sdt=%%2C47&as_ylo=%s&q=%s&btnG=", .max, .year, .search)
  } else if (.args[1] & !.args[2] & !.args[3]) {
    .max <- seq(0, 100, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%dhl=en&as_vis=1&as_sdt=%%2C47&as_ylo=%s&q=%s&btnG=", .max, .year,.search)
  } else if (!.args[1] & (.args[2] & .args[3])) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0%%2C47&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0%%2C47&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 100, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0%%2C47&q=%s&btnG=", .max, .search)
  } else if (.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%dhl=en&as_vis=1&as_sdt=%%2C47&as_ylo=%s&q=%s&btnG=", .max, .year,.search)
  } else if (.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 100, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%dhl=en&as_vis=1&as_sdt=%%2C47&as_ylo=%s&q=%s&btnG=", .max, .year,.search)
  } else {
    .max <- seq(0, 100, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0%%2C47&q=%s&btnG=", .max, .search)
  }
  return(.base)
}


#'@Title Scrape Child Function
#'@Description This function actually does the scraping of Google Scholar
#'@export
#'@import curl rvest xml2
NULL
scrape <- function(url, verbose = FALSE){
  my_page <- xml2::read_html(curl::curl(url, handle = curl::new_handle("useragent" = "Chrome/60.0.3112.113")))
    if(verbose){
      return(xml2::html_structure(my_page))
    } else {
        .title <- rvest::html_text(rvest::html_nodes(my_page, ".gs_rt"), trim = TRUE)
        .info <- rvest::html_text(rvest::html_nodes(my_page, ".gs_a"), trim = TRUE)
        .abs <- rvest::html_text(rvest::html_nodes(my_page, ".gs_rt"), trim = TRUE)
        .url <- rvest::html_nodes(my_page, ".gs_ri h3 a")
        .url <- rvest::html_attr(.url, "href")
        #clean strings
        .auth <- gsub("-.*", "", .info)
        .auth <- trimws(.auth)
        .auth <- gsub(".", " et al.", .auth)
        .info <- sapply(strsplit(.info, "-"), `[`, 2)
        .journ <- gsub(",.*", "", .info)
        .journ <- trimws(.journ)
        .date <- gsub(".*,", "",  .info)
        .date <- trimws(.date)
        .title <- gsub("\\[[^\\]]*\\]", "", .title, perl=TRUE)
        .abs <- gsub("\\[[^\\]]*\\]", "", .abs, perl=TRUE)
        .abs <- gsub(".", "", .abs, perl=TRUE)
        .abs <- paste(.abs, collapse  = " ")
        #build result
        .out <- list(Title = .title, Author = .auth, Journal = .journ, Year = .date, Link = .url, Abstract = .abs)
        if(all(sapply(.out, length)==length(.out[[1]]))){
          .out <- as.data.frame(.out, stringsAsFactors = FALSE)
        }
        return(.out)
    }
}


#'@Title just a helper to get the number of pages from a search
n_pages <- function(html_page) { 
  .pages <- rvest::html_nodes(html_page, '.gs_ab_mdw')
  .pages <- rvest::html_text(.pages)[2]
  .pages <-  gsub(",", "", sub("About (.*?) results.*","\\1", .pages))
  .pages <- as.numeric(.pages) / 10
  return(.pages)
  }
