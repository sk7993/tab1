#' SMD for numeric variables
#'
#' @param x1 Numeric vector of data values
#' @param x2 Optional numeric vector if `grp` is not specified.
#' @param grp Variable specifying grouping for for data values
#' @param abs Specify whether to return absolute values or not (default is TRUE).
#'
#' @returns
#' @export
#'
#' @examples
smd_num <- function(x1, x2 = NULL, grp = NULL, abs = TRUE){

  if (is.null(x2) & is.null(grp)) {
    stop("Must specify either x2 or grp.")
  }

  if (!is.null(x2) & !is.null(grp)) {
    stop("Must specify either x2 or grp, not both.")
  }

  if (!is.null(grp)) {
    cat <- unique(grp)
    x2 <- x1[grp == cat[2]]
    x1 <- x1[grp == cat[1]]
  }
  mu1 <- mean(x1, na.rm = TRUE)
  mu2 <- mean(x2, na.rm = TRUE)
  d = mu2 - mu1
  if (abs == TRUE) {
    d = abs(d)
  }

  sd_pooled <- sqrt((var(x1) + var(x2))/2)
  smd = d/sd_pooled

  return(smd)
}

#' SMD for skewed/non-normal numeric variables
#'
#' @param x1 Numeric vector of data values
#' @param x2 Optional numeric vector if `grp` is not specified.
#' @param grp Variable specifying grouping for for data values
#' @param ... Additional arguments passed to `smd_num`.
#'
#' @returns
#' @export
#'
#' @examples
smd_num_nn <- function(x1, x2 = NULL, grp = NULL, ...){
  if (is.null(x2) & is.null(grp)) {
    stop("Must specify either x2 or grp.")
  }

  if (!is.null(x2) & !is.null(grp)) {
    stop("Must specify either x2 or grp, not both.")
  }

  if (!is.null(x2)) {
    r <- rank(c(x1, x2))
    x1 <- r[seq_along(x1)]
    x2 <- r[seq_along(x2) + length(x1)]
  } else if (!is.null(grp)) {
    x1 <- rank(x1)
  }

  return(smd_num(x1, x2, grp, ...))

}


#' SMD for categorical variables
#'
#' @param x
#' @param grp
#'
#' @returns
#' @export
#'
#' @examples
smd_fac <- function(x1, grp){

  tbl <- table(x, grp) |>
    prop.table(2)

  # Differences in proportions
  x1_p <- tbl[,1][-1]
  x2_p <- tbl[,2][-1]

  d <- x2_p - x1_p

  # Covariance matrix
  dgl <- (x1_p*(1 - x1_p) + x2_p*(1 - x2_p))/2
  covar <- (-1*outer(x1_p, x1_p) + -1*outer(x2_p, x2_p))/2
  diag(covar) <- dgl

  # Calculate ASD
  smd_fac <- sqrt(drop(t(d) %*% solve(covar) %*% d))
  return(smd_fac)
}


tableone::CreateCatTable("carb", "am", mtcars, test = FALSE) |>
  tableone::ExtractSmd()

smd_fac(mtcars$carb, mtcars$am)
