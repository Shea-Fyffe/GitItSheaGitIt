#' @Title Scraping Google Scholar
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Wrapper function that collect data from a Google Scholar Search
#' @seealso \link{https://scholar.google.com/robots.txt} For preventing scraper from being caught.
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
    .out[[i]] <- try(scrape(.url[i]))
    if((inherits(.out[[i]], "try-error") || is.na(.out[[i]])) & i > 1) {
      .i <- i - 1L
      return(.out[seq(.i)])
    } else if ((inherits(.out[[i]], "try-error") || is.na(.out[[i]])) & i == 1) {
      stop(sprintf("open:%s and check for CAPTCHA and/or mispellings", .url[i]))
    } else {
      .n <- sub('.*?start=(\\d+)&hl.*', '\\1', .url[i])
      .nn <- nrow(.out[[i]])
          if(is.na(.n)) {
            .n <- .nn *(i - 1L) 
          } else {
          .n <- as.numeric(.n)
          .n <- seq(.n + 1L, .n + .nn)
          .out[[i]]$Article_Number <- .n
          }
      .wait <- sample(5:10, 1) * (1 / (10 / length(.out)))
      Sys.sleep(.wait)
    }
  }
  return(.out)
}
#' @Title Build Search
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Helper function for scraping Google Scholar
#' @param ... String to be passed to Google Scholar search. Use \code{min =}, \code{max = } and \code{year = } to restirict number of articles returned as well as year.
#' @details \code{max = } will default to 100 if note explicitly defined.
#' @examples \dontrun{
#'                    ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 250 
#'                    res <- build_search("Engagement", "organizational behavior", max = 250)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satistfaction", year = 2010, min = 100, max = 200)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 but earlier than 2015 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satistfaction", year = c(2010,2015), min = 100, max = 200)}                    
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
  if(is.recursive(.search)) {
  .search <- sapply(unlist(.search), utils::URLencode)
  } else {
  .search <- sapply(.search, utils::URLencode)
  }
  .search <- paste(.search, collapse = "+")
  if(all(.args)) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    if(length(.year)==2L) {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (.args[1] & !.args[2] & !.args[3]) {
    .max <- seq(0, 250, by = 10)
    if(length(.year)==2L) {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (!.args[1] & (.args[2] & .args[3])) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0,47&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=00,47&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 250, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0,47&q=%s&btnG=", .max, .search)
  } else if (.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    if(length(.year)==2L) {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 250, by = 10)
    if(length(.year)==2L) {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else {
    .max <- seq(0, 250, by = 10)
    .base <- sprintf("https://scholar.google.com/scholar?start=%d&hl=en&as_vis=1&as_sdt=0,47&q=%s&btnG=", .max, .search)
  }
  return(.base)
}
#'@Title Scrape Child Function
#'@Description This function actually does the scraping of Google Scholar
#'@export
#'@import curl rvest xml2 httr
NULL
scrape <- function(url, user_agent = NULL, verbose = FALSE){
  if(is.null(user_agent)){
    user_agent <- sample(use_age(), 1)
  }
  #set a random user_agent and a timeout
  httr::set_config(httr::user_agent(user_agent), override = TRUE)
  httr::set_config(httr::timeout(5L))
  
  my_page <- tryCatch(xml2::read_html(x = url), error = function(err) {stop(print(err))})
    
  if(verbose){
      return(xml2::html_structure(my_page))
    } else if(is.na(my_page)){
      on.exit(closeAllConnections())
      return(invisible(my_page))
    } else {
        .out <- parse_page(my_page)
    }
        on.exit(closeAllConnections())
        return(.out)
}
#'@Title just a helper to get the number of pages from a search
n_pages <- function(html_page) { 
  .pages <- rvest::html_nodes(html_page, '.gs_ab_mdw')
  .pages <- rvest::html_text(.pages)[2]
  .pages <-  gsub(",", "", sub("About (.*?) results.*","\\1", .pages))
  .pages <- as.numeric(.pages) / 10
  return(.pages)
}
#'@Title just a helper that finds if articles are missing nodes (i.e., Abstracts, links, etc.)
#'@export
#'@import rvest xml2
parse_page <- function(.html) {
  .html <- rvest::html_nodes(.html, "div.gs_ri")
  .html <- do.call(rbind, lapply(.html, function(x) {
    Title <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_rt")), error=function(err) {NA})
    Meta <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_a")), error=function(err) {NA})
    Abstract <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_rs")), error=function(err) {NA})
    Link <- tryCatch(rvest::html_attr(rvest::xml_node(x, "h3 a"), "href"), error=function(err) {NA})
    x <- data.frame(Title, Meta, Abstract, Link, stringsAsFactors = FALSE)
    return(x)
  }))
  if(is.null(.html)) {
    return(NA)
  } else {
  .html <- .parsing_helper(.html)
  .html <- data.frame(apply(.html, 2, function(x) parsing_helper(x)), stringsAsFactors = FALSE)
  .html <- .html[c("Title", "Author", "Journal", "Year", "Link", "Abstract")]
  return(.html)
  }
}
####children
parsing_helper <- function(.vec) {
  .vec <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", .vec, perl=TRUE)
  return(.vec)
}
.parsing_helper <- function(.df) {
  .meta <- strsplit(.df$Meta, "\\s-\\s")
  .meta <- lapply(.meta, .f)
  .meta <- do.call("rbind", .meta)
  .df$Title <- gsub("\\[[^\\]]*\\]", "", .df$Title, perl=TRUE)
  .df$Abstract <- gsub("[^[:alnum:][:blank:]+?&/\\-]", " ", .df$Abstract, perl=TRUE)
  .df$Abstract <- gsub("^Page\\s\\d+.", "", .df$Abstract)
  .df$Meta <- NULL
  .df <- cbind(.df, .meta)
  return(.df)
}
#' @details helper that extracts Author, Journal, and Year
.f <- function(.vec) {
  if(length(.vec) == 3||length(.vec) == 2) {
    Author <- gsub("\\s-.*", "", .vec[1])
    if(grepl(",", .vec[2])) {
      Date <- gsub(".*(\\d{4})", "\\1", .vec[2])
      Journal <- gsub("(.+?)(\\,.*)", "\\1", .vec[2])
    } else if (grepl("^\\d{4}$|^\\s\\d{4}$", .vec[2])) {
      Date <- gsub(".*(\\d{4})", "\\1", .vec[2])
      Journal <- NA
    } else if (!grepl("^\\d{4}$|^\\s\\d{4}$", .vec[2])) {
      Date <- NA
      Journal <- .vec[2]
    } else {
      Date <- NA
      Journal <- NA
    }
  } else if(length(.vec == 1)) {
    if(grepl("([A-Z]+ )", .vec[1])) {
      Author <- .vec[1]
      Date <- NA
      Journal <- NA
    } else if(grepl("^\\d{4}$|^\\s\\d{4}$", .vec[1])) {
      Author <- NA
      Date <- gsub(".*(\\d{4})", "\\1", .vec[1])
      Journal <- NA
    } else if(!grepl("\\.", .vec[1])) {
      Author <- NA
      Date <- NA
      Journal <- .vec[1]
    } else {
      Author <- NA
      Date <- NA
      Journal <- NA
    }
  }
  .res <- data.frame(Author = Author, Journal = Journal, Year = Date, stringsAsFactors = FALSE)
  return(.res)
}

#' @title User-Agent Helper
use_age <- function(){
  .out <-c("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36",
           "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:53.0) Gecko/20100101 Firefox/53.0",
           "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.79 Safari/537.36 Edge/14.14393")
  return(.out)
}