#' Get SMD for Variables in a Data Frame
#'
#' @param data
#' @param grp
#' @param nonnormal
#'
#' @returns
#' @export
#'
#' @examples
smd <- function(data, grp, wts = NULL,
                nonnormal = NULL, ...){

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }
  # Coerce character to factor
  data <- rapply(data, as.factor, "character", how = "replace")
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
                    c("numeric", "integer"),
                    how = "list",
                    grp = data[[grp]],
                    wts = wts
                    )

  res_num_nn <- rapply(data_sub[nonnormal],
                       smd_num_nn,
                       c("numeric", "integer"),
                       how = "list",
                       grp = data[[grp]],
                       wts = wts)

  res_fac <- rapply(data_sub,
                    smd_fac,
                    "factor",
                    how = "list",
                    grp = data[[grp]],
                    wts = wts
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
    res2[["var"]] <- res2$parent_var
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
smd_num <- function(x, grp, wts = NULL){

  if (length(x) != length(grp)) {
    stop("`x` and `grp` vectors must be of equal length.")
  }

  if (length(unique(grp)) < 2) {
    stop("At least two groups are needed to compute SMD.")
  }

  if (is.null(wts)) {
    wts <- rep(1, length(x))
  }

  # Coerce grouping variable to factor-----
 if (!is.factor(grp)) {
   grp <- factor(grp)
 }
  # Compute pairwise SMD for each group------
  x_split <- split(data.frame("x" = x,
                              "wts" = wts),
                   grp)
  smd <- combn(levels(grp), 2, FUN = \(p) {
    g1 <- p[1]
    g2 <- p[2]
    x1 <- x_split[[g1]][["x"]]
    x2 <- x_split[[g2]][["x"]]
    wts1 <- x_split[[g1]][["wts"]]
    wts2 <- x_split[[g2]][["wts"]]
    res <- compute_smd_num(x1, x2, wts1, wts2)
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
#'
#' @examples
compute_smd_num <- function(x1, x2,
                            wts1 = NULL,
                            wts2 = NULL,
                            denom = "unweighted",
                            abs = TRUE, digits = 3) {
  if ((is.null(wts1) & !is.null(wts2)) |
      (!is.null(wts1) & is.null(wts2))) {
    stop("Must specify both `wts1` and `wts2`, not just one.")
  }
  if (is.null(wts1) & is.null(wts2)) {
    wts1 <- rep(1, length(x1))
    wts2 <- rep(1, length(x2))
  }

  mu1 <- wtd.mean(x1, wts1, na.rm = TRUE)
  mu2 <- wtd.mean(x2, wts2, na.rm = TRUE)
  diff <- mu2 - mu1
  if (abs == TRUE) {
    diff <- abs(diff)
  }
  if (denom == "unweighted") {
    pooled_sd <- sqrt(0.5*(var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)))
  } else if (denom == "weighted") {
    pooled_sd <- sqrt(0.5*(wtd.var(x1, wts1, na.rm = TRUE) +
                             wtd.var(x2, wts2, na.rm = TRUE)))
  }

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
smd_fac <- function(x, grp, wts = NULL,
                    digits = 3){

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

  if (is.null(wts)) {
    wts <- rep(1, length(x))
  }

 # Compute pairwise ASD for each group------
  x_split <- split(data.frame("x" = x,
                              "wts" = wts),
                   grp)

  smd <- combn(levels(grp), 2, FUN = \(p) {
    g1 <- p[1]
    g2 <- p[2]
    x1 <- x_split[[g1]][["x"]]
    x2 <- x_split[[g2]][["x"]]
    w1 <- x_split[[g1]][["wts"]]
    w2 <- x_split[[g2]][["wts"]]
    res <- compute_smd_fac(x1, x2,
                           w1,
                           w2,
                           digits)
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
compute_smd_fac <- function(x1, x2,
                            wts1 = NULL,
                            wts2 = NULL,
                            digits = 3){

  if ((is.null(wts1) & !is.null(wts2)) |
      (!is.null(wts1) & is.null(wts2))) {
    stop("Must specify both `wts1` and `wts2`, not just one.")
  }
  if (is.null(wts1) & is.null(wts2)) {
    wts1 <- rep(1, length(x1))
    wts2 <- rep(1, length(x2))
  }

  # Diff in proportions
  x1_p <- prop.table(wtd_tbl(x1, wts1))[-1]
  x2_p <- prop.table(wtd_tbl(x2, wts2))[-1]
  d <- x2_p - x1_p

  # Covariance matrix
  x1_p <- prop.table(table(x1))[-1]
  x2_p <- prop.table(table(x2))[-1]
  dgl <- (x1_p*(1 - x1_p) + x2_p*(1 - x2_p))/2
  covar <- (-1*outer(x1_p, x1_p) + -1*outer(x2_p, x2_p))/2
  diag(covar) <- dgl

  # Calculate ASD
  smd <- sqrt(drop(t(d) %*% solve(covar) %*% d))
  return(round(smd, digits))
}
