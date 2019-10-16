#' @title Get and Convert SPSS files to R
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @import haven sjlabelled
#' @family SPSS
get_spss <- function(dir = getwd(),
                     var.pattern = NULL) {
  if (!dir.exists(dir)) {
    stop("Please specify correct directory")
  }
  .file_paths <- list.files(dir, pattern = ".sav")
  if (!length(.file_paths)) {
    stop(sprintf("No .sav files found in %s", dir))
  }
  .files <- lapply(.file_paths, haven::read_sav)
  .files  <- lapply(.files, haven::zap_formats)
  .files <- lapply(.files, sjlabelled::remove_all_labels)
  .files <-
    lapply(.files, function(X)
      as.data.frame(X, stringsAsFactors = FALSE))
  if (!is.null(var.pattern)) {
    .files <- lapply(.files, function(X)
      get_vars(X, var.pattern))
  }
  names(.files) <- basename(.file_paths)
  if (length(.files) == 1L) {
    return(.files[[1]])
  } else {
    return(.files)
  }
}


#' @title Recode variable values
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to recode
#' @param vars Required. Vector of class character of variable names. May use [GitItSheaGitIt::get_vars()]
#' @param old_values Required. Vector of old values to replace.
#' @param old_values Required. Vector of new values to replace old values.
#' @import plyr
#' @family SPSS
recode_variables <-
  function(x,
           vars,
           old_values = NULL,
           new_values = NULL) {
    if (!all(is.character(vars))) {
      stop("vars should be of class character")
    }
    if (!all(vars %in% names(x))) {
      stop(sprintf("%s are not all valid columns in x", vars))
    }
    .temp <- unique(unlist(x[, vars]))
    if (!all(.temp %in% old_values)) {
      stop(
        sprintf("Old values are missing: %s\n", setdiff(.temp, old_values))
      )
    }
    if (length(vars) > 1) {
      x[, vars] <-
        apply(x[, vars], 2, function(x)
          plyr::mapvalues(x, from = old_values, to = new_values))
    } else {
      x[, vars] <-
        plyr::mapvalues(x[, vars], from = old_values, to = new_values)
    }
    return(x)
  }


#' @title Sum set of variables
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to sum
#' @param pat Required. Pattern of variable names to select columns. May use Regex.
#' @param new_col Required. Name of new column to be created.
#' @family SPSS
sum_subscale <- function(x, pat, new_col) {
  x[, new_col] <-
    apply(x[grep(pat, names(x))], 1, function(x)
      sum(x, na.rm = TRUE))
  return(x)
}
#' @title Calculate average of set of variables
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to average
#' @param pat Required. Pattern of variable names to select columns. May use Regex.
#' @param new_col Required. Name of new column to be created.
#' @family SPSS
mean_subscale <- function(x, pat, new_col) {
  x[, new_col] <-
    apply(x[grep(pat, names(x))], 1, function(x)
      mean(x, na.rm = TRUE))
  return(x)
}

#' @title Identify all variables in a dataframe given a pattern
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to index
#' @param pat Required. Pattern of variable names to select columns. May use Regex.
#' @param ... Optional. Additional arguments to pass to grepl.
#' @seealso [base::grep()]
#' @family SPSS
get_vars <- function(x, pat, ...) {
  .out <- names(x)[grepl(y, names(x), ...)]
  return(.out)
}
