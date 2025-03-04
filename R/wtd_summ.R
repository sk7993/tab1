wtd_tbl <- function(x, weights = NULL){
  if (is.null(weights)) {
    return(table(x, ...))
  }
  if (!is.factor(x)) {
    x <- factor(x)
  }
  if (!is.null(weights)) {
    tapply(weights, x, sum, na.rm = TRUE, default = 0)
  }
}
# Slightly modified helper functions from `Hmisc` to calculate weighted quantiles
## Copyright (C) 2001 Frank E Harrell Jr
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

"%nin%" <- function (x, table) {
  match(x, table, nomatch = 0) == 0
}
all.is.numeric <- function (x, what = c("test", "vector", "nonnum"), extras = c(".",
                                                              "NA"))
{
  what <- match.arg(what)
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  xs <- x[x %nin% c("", extras)]
  if (!length(xs) || all(is.na(x)))
    return(switch(what, test = FALSE, vector = x, nonnum = x[0]))
  isnon <- suppressWarnings(!is.na(xs) & is.na(as.numeric(xs)))
  isnum <- !any(isnon)
  switch(what, test = isnum, vector = if (isnum) suppressWarnings(as.numeric(x)) else x,
         nonnum = xs[isnon])
}

wtd.mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE)
{
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sum(weights * x)/sum(weights)
}

wtd.var <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE, method = c("unbiased",
                                                                      "ML"))
{
  method <- match.arg(method)
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  if (normwt || method == "ML")
    return(as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov))
  sw <- sum(weights)
  if (sw <= 1)
    warning("only one effective observation; variance estimate undefined")
  xbar <- sum(weights * x)/sw
  sum(weights * ((x - xbar)^2))/(sw - 1)
}

wtd.ecdf <- function (x, weights = NULL, type = c("i/n", "(i-1)/(n-1)", "i/(n+1)"),
                      normwt = FALSE, na.rm = TRUE)
{
  type <- match.arg(type)
  switch(type, `(i-1)/(n-1)` = {
    a <- b <- -1
  }, `i/(n+1)` = {
    a <- 0
    b <- 1
  }, `i/n` = {
    a <- b <- 0
  })
  if (!length(weights)) {
    oldopt <- options("digits")
    options(digits = 7)
    on.exit(options(oldopt))
    cumu <- table(x)
    isdate <- testDateTime(x)
    ax <- attributes(x)
    ax$names <- NULL
    x <- as.numeric(names(cumu))
    if (isdate)
      attributes(x) <- c(attributes(x), ax)
    cumu <- cumsum(cumu)
    cdf <- (cumu + a)/(cumu[length(cumu)] + b)
    if (cdf[1] > 0) {
      x <- c(x[1], x)
      cdf <- c(0, cdf)
    }
    return(list(x = x, ecdf = cdf))
  }
  w <- wtd.table(x, weights, normwt = normwt, na.rm = na.rm)
  cumu <- cumsum(w$sum.of.weights)
  cdf <- (cumu + a)/(cumu[length(cumu)] + b)
  list(x = c(if (cdf[1] > 0) w$x[1], w$x), ecdf = c(if (cdf[1] >
                                                        0) 0, cdf))
}

wtd.quantile <- function (x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
          type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"), normwt = FALSE,
          na.rm = TRUE)
{
  if (!length(weights))
    return(quantile(x, probs = probs, na.rm = na.rm))
  type <- match.arg(type)
  if (any(probs < 0 | probs > 1))
    stop("Probabilities must be between 0 and 1 inclusive")
  nams <- paste(format(round(probs * 100, if (length(probs) >
                                              1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
  i <- is.na(weights) | weights == 0
  if (any(i)) {
    x <- x[!i]
    weights <- weights[!i]
  }
  if (type == "quantile") {
    w <- wtd.table(x, weights, na.rm = na.rm, normwt = normwt,
                   type = "list")
    x <- w$x
    wts <- w$sum.of.weights
    n <- sum(wts)
    order <- 1 + (n - 1) * probs
    low <- pmax(floor(order), 1)
    high <- pmin(low + 1, n)
    order <- order%%1
    allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant",
                   f = 1, rule = 2)$y
    k <- length(probs)
    quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
    names(quantiles) <- nams
    return(quantiles)
  }
  w <- wtd.Ecdf(x, weights, na.rm = na.rm, type = type, normwt = normwt)
  structure(approx(w$ecdf, w$x, xout = probs, rule = 2)$y,
            names = nams)
}

wtd.table <- function (x, weights = NULL, type = c("list", "table"), normwt = FALSE,
                       na.rm = TRUE)
{
  type <- match.arg(type)
  if (!length(weights))
    weights <- rep(1, length(x))
  isdate <- testDateTime(x)
  ax <- attributes(x)
  ax$names <- NULL
  if (is.character(x))
    x <- as.factor(x)
  lev <- levels(x)
  x <- unclass(x)
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s, drop = FALSE]
    weights <- weights[s]
  }
  n <- length(x)
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  i <- order(x)
  x <- x[i]
  weights <- weights[i]
  if (anyDuplicated(x)) {
    weights <- tapply(weights, x, sum)
    if (length(lev)) {
      levused <- lev[sort(unique(x))]
      if ((length(weights) > length(levused)) && any(is.na(weights)))
        weights <- weights[!is.na(weights)]
      if (length(weights) != length(levused))
        stop("program logic error")
      names(weights) <- levused
    }
    if (!length(names(weights)))
      stop("program logic error")
    if (type == "table")
      return(weights)
    x <- all.is.numeric(names(weights), "vector")
    if (isdate)
      attributes(x) <- c(attributes(x), ax)
    names(weights) <- NULL
    return(list(x = x, sum.of.weights = weights))
  }
  xx <- x
  if (isdate)
    attributes(xx) <- c(attributes(xx), ax)
  if (type == "list")
    list(x = if (length(lev)) lev[x] else xx, sum.of.weights = weights)
  else {
    names(weights) <- if (length(lev))
      lev[x]
    else xx
    weights
  }
}
