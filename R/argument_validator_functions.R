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
#'              x <- parent_function('A', z = 'C')
#'              print(x)
#' } 
#'
#' @seealso \code{\link[base]{sys.call}} for capturing function calls.
#' \code{\link[base]{eval}} for capturing function calls
#'
#'
#' @export
arg_val <- function() {
    # assign parent function call to .Call
    .Call <- sys.call(-1)
    # evaluate call given parent function environment and arguments passed to parent
    # function
    eval.Call <- eval(.Call[[1]], parent.frame())
    # return all possible arguments from parent function
    .Formals <- formals(eval.Call)
    # return explicitly called arguments from parent function
    .Args <- match.call(definition = eval.Call, call = .Call)
    # get name of the parent function call and turn it into a character vector
    .Call <- as.character(.Call[[1]])
    # create named list of explicitly called arguments
    .Args <- as.list(.Args)[-1]
    # check if nothing is defined
    if (!length(.Args)) {
        stop("All arguments undefined, please specify arguments")
    }
    # if a required argument was not defined.
    if (any(!names(.Formals) %in% names(.Args))) {
        missing.args <- sapply(.Formals[!names(.Formals) %in% c(names(.Args), "...")], 
            function(.x) identical(.x, quote(expr = )))
        missing.args <- missing.args[missing.args]
        if (any(missing.args)) 
            stop(sprintf("Required argument missing %s", names(missing.args)))
    }
    return(.Args)
}
