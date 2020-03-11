#' Title
#'
#' @param n1 
#' @param n2 
#' @param es 
#' @param out_vg 
#'
#' @return
#' @export
#'
#' @examples
compute_g <- function(n1, n2, es, out_vg = TRUE) {
  .df <- 4 * (n1 + n2 - 2)
  .j <- 1 - 3/(.df - 1)
  .res <-  .j * es
  if (out_vg) {
    .vg <- .j^2 * compute_v(n1, n2, es)
    .res <- list(g = .res, vg = .vg)
  }
  return(.res)
}
#' Title
#'
#' @param n1 
#' @param n2 
#' @param es 
#' @param inverse 
#'
#' @return
#' @export
#'
#' @examples
compute_v <- function(n1, n2, es, inverse = FALSE) {
  .res <- (n1 + n2) / (n1 + n2)
  .res <- es^2 / (2 * (n1 + n2))
  if (inverse) {
    .res <- 1 / .res
  }
  return(.res)
}

#' Title
#'
#' @param x1 
#' @param x2 
#' @param n1 
#' @param n2 
#' @param sd1 
#' @param sd2 
#'
#' @return
#' @export
#'
#' @examples
compute_d <- function(x1, x2, n1, n2, sd1, sd2) {
  .md <- x1 - x2
  
  .sswn <- (n1 - 1)*sd1^2 + (n2 - 1)*sd2^2
  .sswd <- (n1 + n2 - 2)
  
  .es <- .md / sqrt(.sswn / .sswd)
  return(.es)
}

convert_effect_sizes <- function(x, from, to) {
  
}

