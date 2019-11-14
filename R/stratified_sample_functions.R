#' @title Create representative sub-sample based on stratification
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Splits \code{data.frame(x)} based in strata passed using \code{...}.
#' Will attempt to return a sample with \code{nrow()==size} that meets population parameters.
#'
#'
#' @param x a data.frame that represents the population frame.
#' @param size an integer value to specify rows in the object returned.
#' @param ... a character vector or list of names to be used to identify strata in \code{x}
#'
#' @usage stat.stratifiedsample(x, size = 2500, ...)
#'
#' @return a data.frame that represents a subsample of \code{x}
#'
#' @examples \examples{
#' #create SQL query
#' sql <- sprintf('SELECT * FROM Table_A')
#' #Run SQL query to get data from database
# res <- DBI::dbGetQuery(con = con, sql)
#' #define strata
#' strata <- c('US Census Geographic Region','Gender','Physician Or Sonographer')
#' res <- stat.stratifiedsample(res, size = 2500, strata)
#'
#' #you could also use:
#' res <- stat.stratifiedsample(res, size = 2500, 'US Census Geographic Region','Gender','Physician Or Sonographer')
#' }
#'
#'
#' @family Statistics
#' @export
stat.stratifiedsample <- function(x, size = 2500, ...) {
    if (!inherits(x, "data.frame")) 
        stop("verify x is a data.frame")
    if (is.missing(size) || !is.integer(size) || size < 0) 
        stop("verify size is a valid, positive integer")
    strata <- list(...)
    if (length(strata) == 1) {
        strata <- unlist(strata)
    } else if (length(strata) == 0) {
        stop("Please define strata.")
    } else {
        strata <- sapply(strata, `[`)
    }
    if (!all(strata %in% names(x))) 
        stop(sprintf("%s is invalid try a different column name", strata[!strata %in% 
            names(x)]))
    N <- nrow(x)
    if (size > N) {
        warning("Desired sample is greater population frame, returning population frame. Try lowering size argument")
        return(x)
    } else {
        lx <- with(x, split(x, x[, strata]))
        rel_per <- sapply(lx, function(x) nrow(x)/N)
        rel_per <- round(rel_per * size, 0)
        strat_loop <- function(x, y) {
            if (class(x) == "list") 
                x <- as.data.frame(x)
            if (nrow(x) > y) {
                x <- x[sample(1:nrow(x), y), ]
            } else {
                x
            }
            x
        }
        out <- Map(strat_loop, x = lx, y = rel_per)
        out <- do.call("rbind", out)
        row.names(out) <- NULL
        out
    }
}



#' @title Stratified Sample helper optimized allocation
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @description Creates desired distribution of sample sizes based on vec(e.g., Item Difficulty)
neyman_allocation <- function(N, df, vec, strata) {
    .num <- tapply(df[, vec], strata, function(x) {
        x <- length(x) * sd(x)
        x
    })
    .num[is.na(.num)] <- 0
    nh <- N * (.num/sum(.num))
    nh <- nh[order(nh, decreasing = TRUE)]
    return(nh)
}
