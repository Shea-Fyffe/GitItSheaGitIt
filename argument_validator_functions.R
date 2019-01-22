#' @title Validate function calls
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Validator function that captures function calls from parent function
#'
#' @returns List of arguments called in function
#' 
#' @example \donotrun{ \code{parent_function <- function(x, y = NULL, z) {
#'                     .args <- arg_val()
#'                     return(.args)}}
#'              x <- parent_function("A", z = "C")
#'              print(x)
#' } 
#'
#' @seealso \code{\link[base]{sys.call}} for capturing function calls.
#' \code{\link[base]{eval}} for capturing function calls
#'
#'
#' @export
arg_val <- function() {
  #Helper functions
  #get exam statuses
  #assign parent function call to .Call
  .Call <- sys.call(-1)
  #evaluate call given parent function environment and arguments passed to parent function
  eval.Call <- eval(.Call [[1]], parent.frame())
  #return all possible arguments from parent function
  .Formals <- formals(eval.Call)
  #return explicitly called arguments from parent function
  .Args <- match.call(definition = eval.Call, call = .Call)
  #get name of the parent function call and turn it into a character vector
  .Call <- as.character(.Call[[1]])
  #create named list of explicitly called arguments
  .Args <- as.list(.Args)[-1]
  #check if nothing is defined
  if(!length(.Args)) {
    stop("All arguments undefined, please specify arguments")
  }
  #if a required argument was not defined.
  if(any(!names(.Formals) %in% names(.Args))) {
    missing.args <- sapply(.Formals[!names(.Formals) %in% c(names(.Args), "...")], function(.x) identical(.x, quote(expr = )))
    missing.args <- missing.args[missing.args]
    if(any(missing.args)) stop(sprintf("Required argument missing %s", names(missing.args)))
  }
  #validate character arguments based on function
  #.Args <- lapply(.Args, valid.char, parent.call = .Call)
  #validate date based on if date is a possible formal argument
  # if(all(c("start_date", "end_date") %in% names(.Formals))) {
  #   if(all(c("start_date","end_date") %in% names(.Args)) || ("start_date" %in% names(.Args) & !"end_date" %in% names(.Args))) {
  #     if(!"end_date" %in% names(.Args)) {
  #       `$`(.Args,"end_date") <- as.character(Sys.Date())
  #     }
  #     try(sapply(c(`$`(.Args,"start_date"),`$`(.Args,"end_date")), valid.date))
  #     span <- difftime(`$`(.Args,"end_date"),`$`(.Args,"start_date"))
  #     if(span < 0) {
  #       stop("Please verify start_date is before end_date")
  #     }
  #     .date <- TRUE
  #   } else if (!all(c("start_date","end_date") %in% names(.Args))){
  #     .date <- FALSE
  #   } else {
  #     stop(sprintf("Please define %s",names(.Formals)[!names(.Formals) %in% names(.Args)]))
  #   }
  # }
  return(.Args)
  
}