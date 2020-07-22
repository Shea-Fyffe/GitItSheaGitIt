#' todo(shea.fyffe) add proper documenation
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
create_lexicon <- function(x, doc_prop_max = 1, word_min = 5, word_max = Inf, ...) {
  if(!is.character(x)) {
    stop("x is not a character vector")
  }
  .it <- text2vec::itoken(x,  progressbar = FALSE)
  .vocab <- text2vec::create_vocabulary(.it)
  .vocab <- text2vec::prune_vocabulary(.vocab, doc_proportion_max = doc_prop_max,
                                       term_count_min = word_min,
                                       term_count_max = word_max, ...)
  .vec <- text2vec::vocab_vectorizer(.vocab)
  return(list(tkn = .it, vec = .vec, vocab = .vocab))
}
#' @title
#'
#' @param text 
#' @param skip_win 
#' @param co_occur_max 
#' @param word_vec_len 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
create_gloVe_embeddings <- function(text, skip_win = 5L, co_occur_max = 100L, word_vec_len = 50L, ...){
  .x <- create_lexicon(text, ...)
  .dtm <-  text2vec::create_tcm(.x[[1]], .x[[2]], skip_win)
  .gloVe <- text2vec::GlobalVectors$new(word_vectors_size = word_vec_len, vocabulary = .x[[3]], x_max = co_occur_max)
  .wd_vec <- text2vec::fit_transform(x = .dtm, model = .gloVe, n_iter = 10L, convergence_tol = 0.01)
  .wd_vec_context <- .gloVe$components
  .word_vectors <- .wd_vec + t(.wd_vec_context)
  return(as.data.frame(.word_vectors))
}
#' @title
#'
#' @param text 
#' @param word_vec 
#' @param has_words 
#' @param mean_vec 
#'
#' @return
#' @export
#'
#' @examples
create_doc_vectors <- function(text, word_vec, has_words = FALSE, mean_vec = TRUE) {
  if(!inherits(word_vec, "data.frame")) {
    word_vec <- as.data.frame(word_vec)
  }
  if(!has_words) {
    word_vec <- data.frame(row.names(word_vec), word_vec)
  }
  if(!all(sapply(word_vec[-1], is.numeric))) {
    word_vec[-1] <- sapply(word_vec[-1], as.numeric)
  }
  .res <- sapply(text, function(.x) {
    .x <- softmaxreg::wordEmbed(.x, word_vec, meanVec = mean_vec)
  })
  return(t(.res))
}

plot_rstne <- function(mat, group, labs, ...) {
  .guid <- apply(mat, 1, paste0, collapse = "")
  .indx <- which(!duplicated(.guid))
  tsne_out <- Rtsne::Rtsne(as.matrix(mat[.indx,]), ...) # Run TSNE
  plot(tsne_out$Y, col = group[.indx], xlab = "word2vecx", ylab = "word2vecy", main = "t-SNE of Personality Items",
       asp = 1)
  text(tsne_out$Y, pos = 3, labels = labs[.indx], cex = .75)# Plot the result
}
