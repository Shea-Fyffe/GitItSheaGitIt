#' @title Get Group Rank Labels
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to index
#' @param pattern Required. Pattern of variable names to select columns. May use Regex.
#' @param rank
#' @param verbose
#' @family qualtrics
#' @export
get_group_rank_labels <-
  function(x,
           pattern,
           rank = FALSE,
           verbose = FALSE) {
    .pat <- sprintf("%s\\d+", pattern)
    if (verbose) {
      .pat <- sprintf("(^Q\\d+)(%s)(.*)", .pat)
      .out <- names(x)[grepl(.pat, names(x))]
      .out <- gsub(.pat, "\\2", .out)
      return(unique(.out))
    }
    if (rank) {
      .pat <- sprintf("%s_rank$", .pat)
    } else {
      .pat <- sprintf("^Q\\d+%s", .pat)
    }
    .out <- grep(.pat, names(x),
                 value = T, ignore.case = T)
    return(.out)
  }
#' @title combine multiple columns into one
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to index
#' @param labels Required. A character vector of column names.
colesce_columns <- function(x, labels) {
  .out <- apply(x[,labels], 1, function(cols){
    cols <- which(cols == 1L)
  })
  return(.out)
}