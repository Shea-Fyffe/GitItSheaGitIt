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
create_lexicon <- function(x, doc_prop_max = 1, word_min = 5, word_max = Inf, out_dtm = FALSE, ...) {
  if(!is.character(x)) {
    stop("x is not a character vector")
  }
  .it <- text2vec::itoken(x,  progressbar = FALSE)
  .vocab <- text2vec::create_vocabulary(.it)
  .vocab <- text2vec::prune_vocabulary(.vocab, doc_proportion_max = doc_prop_max,
                                       term_count_min = word_min,
                                       term_count_max = word_max, ...)
  .vec <- text2vec::vocab_vectorizer(.vocab)
  if(out_dtm) {
    return(text2vec::create_dtm(.it, .vec))
  }
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
create_doc_vectors <-
  function(text,
           word_vec,
           has_words_as_row_names = TRUE,
           mean_vec = TRUE,
           wts = NULL,
           FUN = NULL) {
    .wordEmbed_helper <- function(.res, wts, FUN) {
      if (is.null(wts) && is.null(FUN)) {
        return(.res)
      }
      .n <- sapply(.res, nrow)
      if (!is.null(wts)) {
        if (length(wts) == 1)
          wts <- rep(wts, sum(.n))
        if (length(wts) != sum(.n)) {
          warning(
            "weights argument must be equal to number of valid words in text returning object for inspection"
          )
          return(.res)
        } else {
          .indx <- rep(seq_along(.n), .n)
          .res <- do.call(rbind, .res)
          .f <- ncol(.res)
          .wvals <- unique(wts)
          if (all(.wvals %in% c(1, 0))) {
            wts <- as.logical(wts)
            .res <- split(as.data.frame(.res[wts, ]), .indx[wts])
          } else {
            .wres <- sweep(x = .res,
                           MARGIN = 1,
                           STATS = wts,
                           `*`)
            .res <- split(as.data.frame(.wres), .indx)
          }
        }
      }
      if (!is.null(FUN)) {
        .res <- sapply(unlist(.res, recursive = F), function(x) {
          x <- FUN(x)
        })
        .res <- matrix(.res, ncol = .f, byrow = T)
      }
      return(.res)
    }
    if (!inherits(word_vec, "data.frame")) {
      word_vec <- as.data.frame(word_vec)
    }
    if (has_words_as_row_names || !is.character(word_vec[, 1])) {
      word_vec <- data.frame(row.names(word_vec), word_vec)
    }
    if (!all(sapply(word_vec[, -1], is.numeric))) {
      word_vec[, -1] <- sapply(word_vec[, -1], as.numeric)
    }
    if (!mean_vec || !is.null(wts)) {
      .res <- softmaxreg::wordEmbed(text, word_vec, meanVec = FALSE)
      if (is.null(FUN)) {
        .res <- .wordEmbed_helper(.res, wts, FUN = mean)
      } else {
        .res <- .wordEmbed_helper(.res, wts, FUN)
      }
    } else {
      .res <- softmaxreg::wordEmbed(text, word_vec, meanVec = mean_vec)
    }
    
    return(.res)
  }


plot_rstne <- function(mat, group, labs = NULL, plot = FALSE, ...) {
  if(is.character(group)) {
    group <- as.factor(group)
  }
  .pal <- viridisLite::viridis(n = length(levels(group)))
  tsne_out <- Rtsne::Rtsne(as.matrix(mat), check_duplicates = F, ...)# Run TSNE
  if(plot) {
  plot(tsne_out$Y, col = .pal[group], xlab = "word2vecx", ylab = "word2vecy", main = "t-SNE of Personality Items",
       asp = 1)
    if(!is.null(labs)) {
    text(tsne_out$Y, pos = 3, labels = labs, cex = .75)# Plot the result
    }
  } else {
    return(tsne_out)
  }
}
