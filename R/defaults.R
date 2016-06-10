getDefaultXname <- function(obj) {

    rhs <- formula(obj)[[3]]

    a <- all.vars(rhs)[[1]]
    b <- str_trim(str_split(deparse(rhs), stringr::fixed("+"))[[1]][1])

    if(a != b) stop("Couldn't automatically determine a default fixed effect for this model.")

    return(a)
}

#' @export
getAllXnames <- function(obj, exclude=c("(Intercept)"), pred.type=c("anova","coef")) {
    # default to ANOVA style since the default test is LR, whcih does ANOVA-style names
    # also, coef-style has a lot more problems with multiple comparisons for categorical
    # regresssors. For continuous regressors, it doesn't make any difference since the n
    # names are the same either way
    pred.type <- if(missing(pred.type)) "anova" else match.arg(pred.type)

    if(pred.type == "coef"){
        if(inherits(obj,"lm")){
            n <-  names(coef(obj))
        }else if(inherits(obj,"merMod")){
            n <- names(fixef(obj))
        }else{
            stop("Couldn't automatically determine fixed effect terms for this model.")
        }
    }else{
        # ANOVA-style predictor names
        n <- attr(terms(obj),"term.labels")
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
