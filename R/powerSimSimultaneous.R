#' Estimate omnibus power for multiple effects by simulation
#'
#' Perform a power analysis for a mixed model, testing multiple predictors
#' simultaneously. This follows the usual procedure for statistical testing,
#' where all variables in a model are tested simultaneously in a single model
#' fitted to a given dataset.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the package-default
#'   fixed-effect test will be performed (see \code{\link{tests}})
#' @param xvars The model parameters to test. By defaultt \code{fit} will be
#' @param sim an object to simulate from. By default this is the same as
#'   \code{fit} (see \code{\link{doSim}}).
#' @param seed specify a random number generator seed, for reproducible results.
#' @param fitOpts extra arguments for \code{\link{doFit}}.
#' @param testOpts extra arguments for \code{\link{doTest}}.
#' @param simOpts extra arguments for \code{\link{doSim}}.
#' @param ... any additional arguments are passed on to \code{\link{simrOptions}}.
#'  Common options include:
#'  \describe{
#'      \item{\code{nsim}:}{the number of simulations to run (default is \code{1000}).}
#'      \item{\code{alpha}:}{the significance level for the statistical test (default is \code{0.05}).}
#'      \item{\code{progress}:}{use progress bars during calculations (default is \code{TRUE}).}
#'  }
#'
#' @details This function differs from \code{\link{powerSim}} primarily in the
#'   specification of the test. Instead of requiring a test for a single term,
#'   the test must be specified as a function that returns the test for a vector
#'   of terms specified as a character. For \code{\link[=tests]{fixed}}, you can
#'   simply pass the 'bare' function name, as \code{fixed} itself returns the
#'   appropriate test when passed a model term as a character and many test
#'   methods can return multiple values.
#'
#'   This function differs from \code{\link{powerSimMultiple}} primarily in the
#'   application of the test. While \code{\link{powerSimMultiple}} performs each
#'   simulation only once, it still performs a new test for each term to be
#'   tested. As many tests calculate omnibus results internally (e.g.
#'   \code{car::Anova},\code{lmerTest::anova()} and the t- and z-tests on
#'   coefficients), this is rather wasteful. \code{powerSimSimultaneous} takes
#'   advantage of this and performs each test only once. Unfortunately, not all
#'   tests perform or even support omnibus testing internally (e.g. KR-testing
#'   via \code{pbkrtest::KRmodcomp}).
#'
#'   In other words, \link{powerSimMultiple} performs \strong{multiple} tests for
#'   each simulation iteration, while \link{powerSimSimultaneous} performs a
#'   single set of \strong{simultaneous} sets for each simulation iteration.
#'
#' @section Adapting Test Functions:
#'
#'   To specify additional options to \code{fixed}, you need to wrap the call
#'   (see examples). It should be possible to construct an appropriate wrapper
#'   to \code{\link[=tests]{compare}}, but this will be somewhat more
#'   complicated and the specification of arbitrary nested models isn't
#'   necessary for the omnibus tests this function is designed to implement.
#'
#'   Note that the test function for \code{powerSimSimultaneous} must also work
#'   for \code{powerSimMultiple}, because everything is a vector in R, so any
#'   function that can handle an arbitrary vector of names can handle a vector
#'   of length 1, i.e. a single name. The reverse may not be true, e.g.
#'   KR-testing via \code{pbkrtest::KRmodcomp}.
#'
#' @return an object of class `powerSimList`, which is similar to the `powerSim`
#'   object but has lists for some of its fields
#' @examples
#' fm1 <- lmer(angle ~ recipe + temp + (1|recipe:replicate), cake, REML= FALSE)
#' fm2 <- lm(mpg ~ disp + wt,data=mtcars)
#'
#' # default tests on all model terms
#' powerSimMultiple(fm1, nsim=10)
#'\dontrun{
#' # KR-test on all model terms
#' powerSimMultiple(fm1, function(x) fixed(x,method="kr"), nsim=10)
#'}
#' # t-tests on all model coefficients: with separate tests for each categorical contrast
#' powerSimMultiple(fm2, function(x) fixed(x,method="t"), getAllXnames(fm2, pred.type="coef"), nsim=10)
#'
#' @seealso  \code{\link{powerSim}},\code{\link{powerSimMultiple}}, \code{\link{print.powerSimList}},
#'   \code{\link{confint.powerSimList}}, \code{\link{summary.powerSimList}}
#' @export
powerSimSimultaneous <- function(

    fit,
    test = fixed,
    xvars = getAllXnames(fit,exclude=c("(Intercept)"), pred.type="anova"),
    sim = fit,

    fitOpts = list(),
    testOpts = list(),
    simOpts = list(),

    seed,

    ...

) {

    opts <- simrOptions(...)
    on.exit(simrOptions(opts))

    nsim <- getSimrOption("nsim")
    alpha <- getSimrOption("alpha")
    nrow <- NA

    # START TIMING
    start <- proc.time()

    # setup
    if(!missing(seed)) set.seed(seed)

    # summarise the fitted models

    f <- function() {

        # y <- doSim(sim, [opts])
        tag(y <- do.call(doSim, c(list(sim), simOpts)), tag="Simulating")

        # how many rows?
        ss <- fitOpts$subset
        nrow <<- length(if(is.null(ss)) y else y[ss])

        # fit <- doFit(y, fit, [opts])
        tag(z <- do.call(doFit, c(list(y, fit), fitOpts)), tag="Fitting")

        # Note that while this prevent the model for each simulation from being
        # computed multiple times, the test themselves may still be 'redudant'.
        # For LR-tests, this isn't so much an issue as each test truly only
        # tests one predictor at a time, but for tests that generate ANOVA
        # tables, the ANOVA table is being re-generated for each predictor, even
        # though the ANOVA table already contains tests for all predictors!
        # There doesn't seem to be convenient way of addressing this redudancy
        # without redoing a lot of the infrastructure and/or adding a
        # potentially quite memory-intensive memoisation feature to doTest()
        # (and which would have to include all the package, test, and simulation
        # options!)
        #aply.fnc <- function(xv) do.call(doTest, c(list(z, test[[xv]], testOpts)))
        #tag(pval <- sapply(xvars,aply.fnc,simplify = FALSE), tag="Testing")
        test.fnc <- test(xvars)
        tag(pval <- do.call(doTest, c(list(z, test.fnc), testOpts,simultaneous=length(xvars))), tag="Testing")

        return(pval)
    }

    p <- maybe_rlply(nsim, f(), .text="Simulating")
    p$value <- as.data.frame(do.call(rbind,p$value))
    names(p$value) <- xvars

    # Unlike powerSim and powerSimMultiple, we wrap the test after performing it
    # Wrapping it with sapply gives us the text attribute that we need, but also breaks
    # the simultaneous omnibus portion into multiple distinct tests.
    test <- sapply(xvars, function(xv) wrapTest(test(xv)) , simplify = FALSE  )


    # END TIMING
    timing <- proc.time() - start

    # structure the return value
    rval <- list()

    rval $ x <- sapply(p$value,function(x) sum(x < alpha,na.rm=TRUE) , simplify = FALSE )
    names(rval $ x) <- xvars
    rval $ n <- nsim

    rval $ xnames <- xvars
    #rval $ effect <- fixef(sim)[xname] # can't guarantee this is available?
    rval $ text <- sapply(xvars, function(xv) attr(test[[xv]], "text")(fit, sim) , simplify = FALSE )
    rval $ description <- sapply(xvars, function(xv) attr(test[[xv]], "description")(fit, sim) , simplify = FALSE )
    rval $ pval <- p$value

    rval $ alpha <- alpha
    rval $ nrow <- nrow

    rval $ warnings <- p$warnings
    rval $ errors <- p$errors

    rval $ timing <- timing
    rval $ simrTag <- observedPowerWarning(sim)

    class(rval) <- "powerSimList"

    .simrLastResult $ lastResult <- rval

    return(rval)
}

#' @export
plot.powerSimList <- function(x, ...) stop("Not yet implemented.")