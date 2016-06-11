#' Estimate power by simulation for multiple effects simultaneously.
#'
#' Perform a power analysis for a mixed model.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the first fixed effect in \code{fit} will be tested.
#'     (see: \link{tests}).
#' @param sim an object to simulate from. By default this is the same as \code{fit} (see \code{\link{doSim}}).
#' @param seed specify a random number generator seed, for reproducible results.
#' @param fitOpts extra arguments for \code{\link{doFit}}.
#' @param testOpts extra arguments for \code{\link{doTest}}.
#' @param simOpts extra arguments for \code{\link{doSim}}.
#' @param ... any additional arguments are passed on to \code{\link{simrOptions}}. Common options include:
#' \describe{
#'   \item{\code{nsim}:}{the number of simulations to run (default is \code{1000}).}
#'   \item{\code{alpha}:}{the significance level for the statistical test (default is \code{0.05}).}
#'   \item{\code{progress}:}{use progress bars during calculations (default is \code{TRUE}).}
#'   }#'
#' @examples
#' fm1 <- lmer(y ~ x + (1|g), data=simdata)
#' powerSim(fm1, nsim=10)
#'
#' @export
powerSimMultiple <- function(

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
    # I'm not sure this call to wrapTest is necessary since doTest also calls wrapTest?
    # Ahh, because you need the text attribute later ...
    test <- sapply(xvars, function(xv) wrapTest(test(xv)) )
    #p <- maybe_laply(z, test, .text="Testing")

    f <- function() {

        # y <- doSim(sim, [opts])
        tag(y <- do.call(doSim, c(list(sim), simOpts)), tag="Simulating")

        # how many rows?
        ss <- fitOpts$subset
        nrow <<- length(if(is.null(ss)) y else y[ss])

        # fit <- doFit(y, fit, [opts])
        tag(z <- do.call(doFit, c(list(y, fit), fitOpts)), tag="Fitting")

        # doTest(fit, test, [opts])
        # Note that while this prevent the model for each simulation from being computed multiple times,
        # the test themselves may still be 'redudant'. For LR-tests, this isn't so much an issue
        # as each test truly only tests one predictor at a time, but for tests that generate ANOVA
        # tables, the ANOVA table is being re-generated for each predictor, even though the ANOVA
        # table already contains tests for all predictors! There doesn't seem to be convenient way of
        # addressing this redudancy without redoing a lot of the infrastructure and/or adding a
        # potentially quite memory-intensive memoisation feature to doTest()
        # (and which would have to include all the package, test, and simulation options!)
        aply.fnc <- function(xv) do.call(doTest, c(list(z, test[[xv]], testOpts)))
        tag(pval <- sapply(xvars,aply.fnc), tag="Testing")

        return(pval)
    }

    p <- maybe_rlply(nsim, f(), .text="Simulating")
    p$value <- as.data.frame(do.call(rbind,p$value))

    # END TIMING
    timing <- proc.time() - start

    # structure the return value
    rval <- list()

    rval $ x <- lapply(p$value,function(x) sum(x < alpha,na.rm=TRUE))
    rval $ n <- nsim

    rval $ xnames <- xvars
    #rval $ effect <- fixef(sim)[xname] # can't guarantee this is available?
    rval $ text <- sapply(xvars, function(xv) attr(test[[xv]], "text")(fit, sim) )
    rval $ description <- sapply(xvars, function(xv) attr(test[[xv]], "description")(fit, sim) )
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