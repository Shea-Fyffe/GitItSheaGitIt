#' @title
#' @author 
#' @param x 
#' @param expand_contractions 
#' @param split 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
preprocess_text <- function(x, expand_contractions = TRUE, split = NULL, ...) {
  if(!is.null(split)) {
    x <- unlist(strsplit(x, split))
  }
  x <- qdap::replace_contraction(x)
  x <- gsub("'s", "", x)
  x <- clean_text(x, ...)
  return(x)
} 
#' @title
#' @author
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
create_lexicon <- function(x, ...) {
  if(!is.character(x)) {
    stop("x is not a character vector")
  }
  .it <- text2vec::itoken(x, pro)
  .vocab <- text2vec::create_vocabulary(.it)
  .vocab <- text2vec::prune_vocabulary(.vocab, ...)
  .vec <- text2vec::vocab_vectorizer(.vocab)
  return(.vec)
}

plot_rstne <- function(mat, group, labs, ...) {
  .guid <- apply(mat, 1, paste0, collapse = "")
  .indx <- which(!duplicated(.guid))
  tsne_out <- Rtsne::Rtsne(as.matrix(mat[.indx,]), ...) # Run TSNE
  plot(tsne_out$Y, col = group[.indx], xlab = "word2vecx", ylab = "word2vecy", main = "t-SNE of Personality Items",
       asp = 1)
  text(tsne_out$Y, pos = 3, labels = labs[.indx], cex = .75)# Plot the result
}
