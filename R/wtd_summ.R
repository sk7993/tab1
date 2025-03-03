

# Helper functions from `Hmisc` to calculate weighted quantiles
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
