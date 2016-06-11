getDefaultXname <- function(obj) {

    rhs <- formula(obj)[[3]]

    a <- all.vars(rhs)[[1]]
    b <- str_trim(str_split(deparse(rhs), stringr::fixed("+"))[[1]][1])

    if(a != b) stop("Couldn't automatically determine a default fixed effect for this model.")

    return(a)
}

#' Extract all fixed-effect model terms
#'
#' @param fit a fitted model from \code{\link{lm}}, \code{\link{glm}},
#'   \code{\link{lmer}},  \code{\link{glmer}}
#' @param exclude a vector of model terms to exclude, by default the intercept
#'   term
#' @param pred.type type of predictor to extract.
#'
#' @details
#'   \code{pred.type} determines whether \code{anova}-type model "terms" are
#'   extracted or model coefficients (the actual $beta$'s). For numerical
#'   predictors, there is no difference. For categorical predictors,
#'   \code{pred.type="anova"} returns a single name per predictor, while
#'   \code{pred.type="coef"} returns a name for each contrast in the model. i.e.
#'   with categorical predictors unexpanded. \code{}
#'
#' @export
getAllXnames <- function(fit, exclude=c("(Intercept)"), pred.type=c("anova","coef")) {
    # default to ANOVA style since the default test is LR, which does ANOVA-style names
    # also, coef-style has a lot more problems with multiple comparisons for categorical
    # regresssors. For continuous regressors, it doesn't make any difference since the n
    # names are the same either way
    pred.type <- if(missing(pred.type)) "anova" else match.arg(pred.type)

    if(pred.type == "coef"){
        if(inherits(fit,"lm")){
            n <-  names(coef(fit))
        }else if(inherits(fit,"merMod")){
            n <- names(fixef(fit))
        }else{
            stop("Couldn't automatically determine fixed effect terms for this model.")
        }
    }else{
        # ANOVA-style predictor names
        n <- attr(terms(fit),"term.labels")
    }

    n[!(n %in% exclude)]
}

plotpal <- function(n=length(x), x=getPalette(n)) {

    plot(seq_along(x), rep(1, n), col=x, bg=lighten(x), pch=21, cex=10, xlim=c(0, n+1), lwd=3)

    invisible(x)
}

getPalette <- function(n) {

    start <- c(lcrblue, lcrgreen, lcrbrown)

    if(n <= 3) return(start[seq_len(n)])

    return(c(start, seq_len(n-3)))
}
