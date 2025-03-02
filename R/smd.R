#' Get SMD for Variables in a Dataframe
#'
#' @param data
#' @param grp
#' @param nonnormal
#'
#' @returns
#' @export
#'
#' @examples
smd <- function(data, grp, nonnormal = NULL, ...){

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }
   # Subset data-----
  data_sub <- data[names(data) != grp]

  f1 <- function(x){
    x |>
      as.list() |>
      list2DF()
  }
  # Get SMDs by variable type
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

  # Combine results for numeric type variables
  res1 <- c(res_num, res_num_nn)

  if (all(sapply(res1, is.null))) {
    res1 <- NULL
  } else {
    res1 <- Filter(Negate(is.null), res1) |>
      lapply(f1) |>
      rbind2("var")
  }

  # Combine results for factor variables
  if (all(sapply(res_fac, is.null))) {
    res2 <- NULL
  } else {
    res2 <- Filter(Negate(is.null), res_fac) |>
      lapply(f1) |>
      rbind2("parent_var")
  }

  # Return list
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
smd_num <- function(x, grp){

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
    res <- compute_smd_num(x_split[[g1]], x_split[[g2]])
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  simplify = FALSE) |>
    unlist()

  return(smd)
}

#' SMD computation for numeric vectors
#'
#' @param x1
#' @param x2
#' @param abs
#'
#' @returns
#' @export
#'
#' @examples
compute_smd_num <- function(x1, x2, abs = TRUE, digits = 3) {
  mu1 <- mean(x1, na.rm = TRUE)
  mu2 <- mean(x2, na.rm = TRUE)
  diff <- mu2 - mu1
  if (abs == TRUE) {
    diff <- abs(diff)
  }
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
  res <- round(diff / pooled_sd, digits)
  return(res)
}

#' SMD for skewed/non-normal numeric variables
#'
#' @param x Numeric vector of data values
#' @param grp Vector of same length as `x` specifying grouping for for
#' data values
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
smd_fac <- function(x, grp, digits = 5){

  if (!is.factor(x)) {
    x <- factor(x)
    warning("Coercing `x` to factor variable")
  }

  if (!is.factor(grp)) {
    grp <- factor(grp)
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
    res <- compute_smd_fac(x_split[[g1]], x_split[[g2]], digits)
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  simplify = FALSE) |>
    unlist()

  return(smd)
}

#' SMD computation for factor variables
#'
#' @param x1
#' @param x2
#'
#' @returns
#' @export
#'
#' @examples
compute_smd_fac <- function(x1, x2, digits = 3){

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
  return(round(smd, digits))
}
