context("powerSim")

test_that("powerSimMultiple yields the same result as powerSim for the single predictor case", {
    test <- function(x) fixed(x,method="z")
    xvars <- "x"

    ps <- powerSim(fm1,test=test(xvars),nsim=10,seed=42)
    ps.sum <- summary(ps)
    row.names(ps.sum) <- xvars

    psm <- powerSimMultiple(fm1,test=test, xvars=xvars,nsim=10,seed=42)
    psm.sum <- summary(psm)

    expect_equivalent(ps.sum,psm.sum)
})

test_that("powerSimSimultaneous yields the same result as powerSim for the single predictor case", {
    test <- function(x) fixed(x,method="z")
    xvars <- "x"

    ps <- powerSim(fm1,test=test(xvars),nsim=10,seed=42)
    ps.sum <- summary(ps)
    row.names(ps.sum) <- xvars

    pss <- powerSimSimultaneous(fm1,test=test, xvars=xvars,nsim=10,seed=42)
    pss.sum <- summary(pss)

    expect_equivalent(ps.sum,pss.sum)
})

test_that("powerSimMultiple and powerSimSimultaneous yield the same results for multiple predictors", {
    test <- function(x) fixed(x,method="z")
    xvars <- getAllXnames(fm1,exclude=NULL,pred.type = "coef")

    psm <- powerSimMultiple(fm1,test=test, xvars=xvars,nsim=10,seed=42)
    pss <- powerSimSimultaneous(fm1,test=test, xvars=xvars,nsim=10,seed=42)

    pss.sum <- summary(pss)
    psm.sum <- summary(psm)

    expect_equal(pss.sum,psm.sum)
})