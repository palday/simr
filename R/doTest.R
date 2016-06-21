#' Apply a hypothesis test to a fitted model.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param object an object to apply a statistcal test to, usually a fitted model.
#' @param test a test function, see \link{tests}.
#' @param ... additional options.
#'
#' @return a p-value with attributes describing the test.
#'
#' @export
doTest <- function(object, test, ...) UseMethod('doTest', object)

#' @param simultaneous Use simultaneous tests (\code{FALSE}).
#' @rdname doTest
#' @export
doTest.default <- function(object, test=fixed(getDefaultXname(object)), simultaneous=FALSE,...) {

    opts <- simrOptions(...)
    on.exit(simrOptions(opts))

    test <- wrapTest(test)

    pval <- test(object)

    if(!is.numeric(pval) || is.na(pval)) stop("Test did not return a p-value")

    if(!simultaneous && length(pval)!= 1) stop("Test did not return a p-value")

    if(is.numeric(simultaneous) && length(pval)!= simultaneous) stop("Test did not return enough p-values")

    pval_str <- ifelse(simultaneous,"p-values","p-value")

    rval <- structure(pval,

        text = str_c(pval_str,substring(attr(test, "text")(object, object), 6)),
        description = attr(test, "description")(object, object)
    )

    class(rval) <- "test"

    return(rval)
}

#' @export
print.test <- function(x, ...) {

    cat(attr(x, "text"), ": ", paste(x,collapse=", "), "\n", sep="")

    cat("          --------------------\n")

    pad <- "Test: "
    for(text in attr(x, "description")) {
        cat(pad); pad <- "      "
        cat(text)
        cat("\n")
    }
}
