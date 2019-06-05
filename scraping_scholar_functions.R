#' @title Scraping Google Scholar
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Wrapper function that collect data from a Google Scholar Search
#' @seealso \link{https://scholar.google.com/robots.txt} For preventing scraper from being caught.
#' @param ... String to be passed to Google Scholar search.
#'  Use \code{min =} and \code{max = } to restirict number of articles returned.
#'  Use \code{year = } as a single-element vector to pull articles newer than the given year.
#'  Use \code{year = } as a double-element vector to pull articles between two dates.
#'  \code{year[1]} sets signifies 'after date' and \code{year[2]} 'before date'.
#'  Use \code{article =} to search within a particular article. *note* this should be the article
#'  given by Google (e.g., usually a string of digits), define this as a character.
#' @details \code{max = } will default to 250 if note explicitly defined.
#'  \code{article = } will use search terms within the article defined.
#' @examples \dontrun{
#'                    ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 250 
#'                    res <- build_search("Engagement", "organizational behavior", max = 250)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", year = 2010, min = 100, max = 200)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 but earlier than 2015 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", year = c(2010,2015), min = 100, max = 200)} 
#'           \dontrun{
#'                    ## Searches for articles WITHIN article 10572144307387769361 newer that 2010 but earlier than 2015 with the words 'Job satistfaction'
#'                    ## and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", article = "10572144307387769361", year = c(2010,2015), min = 100, max = 200)}  
#'@export
scrape_scholar <- function(...) {
  .tmp <- paste(c(...))
  if(any(sapply(.tmp, function(x) grepl("^https://", x)))) {
    .url <- .tmp
  } else {
    .url <- build_search(...)
  }
  .out <- list()
  for(i in seq(length(.url))) {
    .out[[i]] <- try(scrape(.url[i]))
    if((inherits(.out[[i]], "try-error") || is.na(.out[[i]])) & i > 1) {
      .i <- i - 1L
      return(.out[seq(.i)])
    } else if ((inherits(.out[[i]], "try-error") || is.na(.out[[i]])) & i == 1) {
      stop(sprintf("open:%s and check for CAPTCHA and/or mispellings", .url[i]))
    } else {
      .n <- sub('.*?start=(\\d+)&.*', '\\1', .url[i])
      .nn <- nrow(.out[[i]])
      if(is.na(.n)) {
        .n <- .nn *(i - 1L) 
      } else {
        .n <- as.numeric(.n)
        .n <- seq(.n + 1L, .n + .nn)
        .out[[i]]$Article_Number <- .n
      }
      
      .pause(.out)
    }
  }
  return(.out)
}
#' @title Scrape Child Function
#' @description This function actually does the scraping of Google Scholar
#' @export
#' @import curl rvest xml2 httr
NULL
scrape <- function(url, user_agent = NULL, verbose = FALSE){
  if(is.null(user_agent)){
    user_agent <- sample(.use_age(), 1)
  }
  #set a random user_agent and a timeout
  httr::set_config(httr::user_agent(user_agent), override = TRUE)
  httr::set_config(httr::timeout(3L))
  
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
#' @title Build Search
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Helper function for scraping Google Scholar
#' @param ... String to be passed to Google Scholar search.
#'  Use \code{min =} and \code{max = } to restirict number of articles returned.
#'  Use \code{year = } as a single-element vector to pull articles newer than the given year.
#'  Use \code{year = } as a double-element vector to pull articles between two dates.
#'  \code{year[1]} sets signifies 'after date' and \code{year[2]} 'before date'.
#'  Use \code{article =} to search within a particular article. *note* this should be the article
#'  given by Google (e.g., usually a string of digits), define this as a character.
#' @details \code{max = } will default to 250 if note explicitly defined.
#'  \code{article = } will use search terms within the article defined.
#' @examples \dontrun{
#'                    ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 250 
#'                    res <- build_search("Engagement", "organizational behavior", max = 250)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", year = 2010, min = 100, max = 200)}
#'           \dontrun{
#'                    ## Searches for articles newer that 2010 but earlier than 2015 with the words 'Job satistfaction' and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", year = c(2010,2015), min = 100, max = 200)} 
#'           \dontrun{
#'                    ## Searches for articles WITHIN article 10572144307387769361 newer that 2010 but earlier than 2015 with the words 'Job satistfaction'
#'                    ## and limits return to 200 starts at 100
#'                    res <- build_search("Job satisfaction", article = "10572144307387769361", year = c(2010,2015), min = 100, max = 200)}                    
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
  if("article" %in% names(.search)){
    .cites <- .search[["article"]]
    .search[["article"]] <- NULL
  }
  
  if(exists(".cites")) {
    .base <- .get_base(.cites)
  }
  else {
    .base <- .get_base(NULL)
  }
  
  
  .args <- c(exists(".year"),exists(".max"),exists(".min"))
  
  if(is.recursive(.search)) {
    .search <- sapply(unlist(.search), utils::URLencode)
  } 
  else {
    .search <- sapply(.search, utils::URLencode)
  }
  
  .search <- paste(.search, collapse = "+")
  
  if(all(.args)) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    if(length(.year)==2L) {
      .add <- sprintf("start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .add <- sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (.args[1] & !.args[2] & !.args[3]) {
    .max <- seq(0, 250, by = 10)
    if(length(.year)==2L) {
      .add <- sprintf("start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .add <- sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (!.args[1] & (.args[2] & .args[3])) {
    .max <- seq(as.numeric(.min), as.numeric(.max), by = 10)
    .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
  } else if (!.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 250, by = 10)
    .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
  } else if (.args[1] & .args[2] & !.args[3]) {
    .max <- seq(0, as.numeric(.max), by = 10)
    if(length(.year)==2L) {
      .add <- sprintf("start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .add <- sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else if (.args[1] & !.args[2] & .args[3]) {
    .max <- seq(as.numeric(.min), as.numeric(.min) + 250, by = 10)
    if(length(.year)==2L) {
      .add <- sprintf("start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=", .max, .year[1], .year[2], .search)
    } else {
      .add <- sprintf("start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=", .max, .year, .search) 
    }
  } else {
    .max <- seq(0, 250, by = 10)
    .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
  }
  
  .base <- paste0(.base,.add)
  
  return(.base)
}
#'@details just a helper to get the number of pages from a search
n_pages <- function(html_page) { 
  .pages <- rvest::html_nodes(html_page, '.gs_ab_mdw')
  .pages <- rvest::html_text(.pages)[2]
  .pages <-  gsub(",", "", sub("About (.*?) results.*","\\1", .pages))
  .pages <- as.numeric(.pages) / 10
  return(.pages)
}
#' @title Parse web page
#' @details Helper function that finds if articles are missing nodes (i.e., Abstracts, links, etc.).
#'  Also cleans data.
#' @export
#' @import rvest xml2
parse_page <- function(.html) {
  .html <- rvest::html_nodes(.html, "div.gs_ri")
  .html <- do.call(rbind, lapply(.html, function(x) {
    Title <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_rt")), error = function(err) {NA})
    Meta <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_a")), error = function(err) {NA})
    Abstract <- tryCatch(xml2::xml_text(rvest::xml_node(x, ".gs_rs")), error = function(err) {NA})
    Link <- tryCatch(rvest::html_attr(rvest::xml_node(x, "h3 a"), "href"), error = function(err) {NA})
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
#' @title Extract Author, Journal, and Year
#' @details Helper function that extracts Author, Journal, and Year from a single node
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
#' @title Randomize User-Agent
#' @details Function that randomly selects user-agents to make the bot seem 'human'
.use_age <- function(){
  .out <-c("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36",
           "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:53.0) Gecko/20100101 Firefox/53.0",
           "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.79 Safari/537.36 Edge/14.14393")
  return(.out)
}
#' @title Pause Helper
#' @details Function that randomly pauses after scraping a web page to make the bot seem 'human'.
#'  to increase scraping speed, lower sampling time.
.pause <- function(.out){
  .wait <- sample(1:6, 1) * (1 / (10 / length(.out)))
  Sys.sleep(.wait) #pause to let connection work
  closeAllConnections()
  gc()
}
#' @title Base Url Helper
#' @details Builds a base-url and determines if search should be global (i.e., across all articles)
#'  or local (i.e., within article cited by)
.get_base <- function(.cites = NULL) {
  if(!is.null(.cites)) {
    .base <- sprintf("https://scholar.google.com/scholar?&hl=en&as_vis=1?&as_sdt=1,47&cites=%s&scipsc=1&", .cites)
  }
  else {
    .base <- "https://scholar.google.com/scholar?&hl=en&as_vis=1?&as_sdt=1,47&"
  }
  return(.base)
}