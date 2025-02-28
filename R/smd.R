#' SMD for numeric variables
#'
#' Computes standardized mean difference for numeric variables.
#'
#' Standardized mean difference is calculated as the difference in means
#' divided by the pooled standard deviation.
#'
#' @param x A numeric vector
#' @param grp A factor of same length as `x` specifying groups.
#' If not a factor, then the vector will be coerced.
#' @param abs Logical specifying whether to return absolute SMD (default is TRUE).
#'
#' @returns A named numeric vector of length `choose(unique(grp), 2)`.
#' @export
#'
#' @examples
smd_num <- function(x, grp, abs = TRUE){

  if (length(x) != length(grp)) {
    stop("`x` and `grp` vectors must be of equal length.")
  }

  if (length(unique(grp)) < 2) {
    stop("At least two groups are needed to compute SMD.")
  }

  # Coerce grouping variable to factor-----
 if (!is.factor(grp)) {
   grp <- factor(grp)
 }

  # Define function for computing ASD-----
  compute_smd <- function(x1, x2, abs) {
    mu1 <- mean(x1, na.rm = TRUE)
    mu2 <- mean(x2, na.rm = TRUE)
    diff <- mu2 - mu1
    if (abs == TRUE) {
      diff <- abs(diff)
    }
    pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
    return(diff / pooled_sd)
  }

  # Compute pairwise ASD for each group------
  x_split <- split(x, grp)
  smd <- combn(levels(grp), 2, FUN = \(p) {
    g1 <- p[1]
    g2 <- p[2]
    res <- compute_smd(x_split[[g1]], x_split[[g2]], abs)
    names(res) <- sprintf("%s vs %s", g1, g2)
    return(res)
  },
  simplify = FALSE) |>
    unlist()

  return(smd)
}

#' SMD for skewed/non-normal numeric variables
#'
#' @param x Numeric vector of data values
#' @param grp Variable specifying grouping for for data values
#' @param ... Additional arguments passed to `smd_num`.
#'
#' @returns
#' @export
#'
#' @examples
smd_num_nn <- function(x, grp, ...){

  return(smd_num(rank(x1), grp, ...))

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
smd_fac <- function(x, grp){

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
