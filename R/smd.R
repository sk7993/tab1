smd <- function(data, grp, nonnormal = NULL){

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }

  data_sub <- data[names(data) != grp]

  f1 <- function(x){
    x |>
      as.list() |>
      list2DF()
  }

  res_num <- rapply(data[setdiff(names(data_sub), nonnormal)],
                    smd_num,
                    "numeric",
                    how = "list",
                    grp = data[[grp]]
                    )

  res_num_nn <- rapply(data_sub[nonnormal],
                       smd_num_nn,
                       "numeric",
                       how = "list",
                       grp = data[[grp]])

  res_fac <- rapply(data_sub,
                    smd_fac,
                    "factor",
                    how = "list",
                    grp = data[[grp]]
                    )

  res1 <- c(res_num, res_num_nn)

  if (all(sapply(res1, is.null))) {
    res1 <- NULL
  } else {
    res1 <- Filter(Negate(is.null), res1) |>
      lapply(f1) |>
      rbind2("var")
  }

  if (all(sapply(res_fac, is.null))) {
    res2 <- NULL
  } else {
    res2 <- Filter(Negate(is.null), res_fac) |>
      lapply(f1) |>
      rbind2("parent_var")
  }

  return(list(res1, res2))
}

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
  # Compute pairwise SMD for each group------
  x_split <- split(x, grp)
  smd <- combn(levels(grp), 2, FUN = \(p) {
    g1 <- p[1]
    g2 <- p[2]
    res <- compute_smd_num(x_split[[g1]], x_split[[g2]], abs)
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  simplify = FALSE) |>
    unlist()

  return(smd)
}

#' SMD computation for numeric vectors (supports only two groups)
#'
#' @param x1
#' @param x2
#' @param abs
#'
#' @returns
#' @export
#'
#' @examples
compute_smd_num <- function(x1, x2, abs) {
  mu1 <- mean(x1, na.rm = TRUE)
  mu2 <- mean(x2, na.rm = TRUE)
  diff <- mu2 - mu1
  if (abs == TRUE) {
    diff <- abs(diff)
  }
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
  return(diff / pooled_sd)
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

  return(smd_num(rank(x), grp, ...))

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

  if (!is.factor(x)) {
    x <- factor(x)
    warning("Coercing `x` to factor variable")
  }

  if (!is.factor(grp)) {
    grp <- factor(grp)
  }

  if (identical(x, grp)) {
    warning("`x` and `grp` cannot be identical variables. Returning NULL.")
    return(NULL)
  }

  if (length(x) != length(grp)) {
    stop("`x` and `grp` vectors must be of equal length.")
  }

  if (length(unique(grp)) < 2) {
    stop("At least two groups are needed to compute SMD.")
  }

 # Compute pairwise ASD for each group------
  x_split <- split(x, grp)
  smd <- combn(levels(grp), 2, FUN = \(p) {
    g1 <- p[1]
    g2 <- p[2]
    res <- compute_smd_fac(x_split[[g1]], x_split[[g2]])
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  simplify = FALSE) |>
    unlist()

  return(smd)
}

#' SMD computation for factor variables (supports only two groups)
#'
#' @param x1
#' @param x2
#'
#' @returns
#' @export
#'
#' @examples
compute_smd_fac <- function(x1, x2){

  # Diff in proportions
  x1_p <- prop.table(table(x1))[-1]
  x2_p <- prop.table(table(x2))[-1]
  d <- x2_p - x1_p

  # Covariance matrix
  dgl <- (x1_p*(1 - x1_p) + x2_p*(1 - x2_p))/2
  covar <- (-1*outer(x1_p, x1_p) + -1*outer(x2_p, x2_p))/2
  diag(covar) <- dgl

  # Calculate ASD
  smd <- sqrt(drop(t(d) %*% solve(covar) %*% d))
  return(smd)
}


