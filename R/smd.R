#' Get SMD for Variables in a Data Frame
#' Returns pairwise standardized mean differences (SMDs) for all variables
#' in the data frame.
#'
#' @param data A data frame
#' @param grp A character vector of length 1 specifying grouping variable
#' @param nonnormal A character vector specifying variables to be treated as
#' non-normal
#' @param wts A non-negative numeric vector of weights.
#' @param abs Logical specifying whether to return absolute SMD
#' @param denom Character vector specifying how to calculate denominator
#' @param digits Integer specifying number of decimal places to be used.
#'
#' @returns A list with two elements. First element holds SMDs for numeric variables,
#' and second element holds SMDs for factor variables.
#' @export
#'
#' @examples
#' smd(iris, "Species")
#' smd(warpbreaks, "tension")
smd <- function(data, grp,
                wts = NULL,
                nonnormal = NULL,
                abs = TRUE,
                denom = "unweighted",
                digits = 3){

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }
  # Coerce character to factor-------
  data <- rapply(data, as.factor, "character", how = "replace")
  # Subset data-----
  data_sub <- data[names(data) != grp]

  f1 <- function(x){
    x |>
      as.list() |>
      list2DF()
  }
  # Get SMDs by variable type---------------
  opts_num <- list(
    grp = data[[grp]],
    denom = denom,
    wts = wts,
    abs = abs,
    digits = digits
  )
  res_num <- rapply(data[setdiff(names(data_sub), nonnormal)],
                    \(x, opts) do.call(smd_num,
                                       c(list(x),
                                         opts)
                    ),
                    c("numeric", "integer"),
                    how = "list",
                    opts = opts_num)

  res_num_nn <- rapply(data[nonnormal],
                       \(x, opts) do.call(smd_num_nn,
                                          c(list(x),
                                            opts)
                       ),
                       c("numeric", "integer"),
                       how = "list",
                       opts = opts_num)

  opts_fac <- list(
    grp = data[[grp]],
    denom = denom,
    wts = wts,
    digits = digits
  )

  res_fac <- rapply(data_sub,
                    \(x, opts) do.call(smd_fac, c(list(x), opts)),
                    "factor",
                    how = "list",
                    opts = opts_fac)

  # Combine results for numeric variables--------
  res1 <- c(res_num, res_num_nn)

  if (all(sapply(res1, is.null))) {
    res1 <- NULL
  } else {
    res1 <- Filter(Negate(is.null), res1) |>
      lapply(f1) |>
      rbind2("var")
  }

  # Combine results for factor variables----------
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
#' @param wts A non-negative numeric vector of weights.
#' @param denom Specifies how the denominator is calculated.
#' @param digits Integer indicating number of decimal places.
#'
#' @returns A named numeric vector of length `choose(unique(grp), 2)`.
smd_num <- function(x, grp,
                    wts = NULL, denom = "unweighted",
                    abs = TRUE, digits = 3){

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
  opts = as.list(match.call())[-c(1:4)]
  # Compute pairwise SMD for each group------
  x_split <- split(data.frame("x" = x,
                              "wts" = wts),
                   grp)
  smd <- utils::combn(levels(grp), 2, FUN = \(p, opts) {
    g1 <- p[1]
    g2 <- p[2]
    x1 <- x_split[[g1]][["x"]]
    x2 <- x_split[[g2]][["x"]]
    wts1 <- x_split[[g1]][["wts"]]
    wts2 <- x_split[[g2]][["wts"]]
    res <- do.call(compute_smd_num,
                   c(list(x1, x2, wts1, wts2),
                     opts))
    #res <- compute_smd_num(x1, x2, wts1, wts2, denom, digits)
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  opts = opts,
  simplify = FALSE) |>
    unlist()

  return(smd)
}

# Function that computes SMD
compute_smd_num <- function(x1, x2,
                            wts1 = NULL,
                            wts2 = NULL,
                            denom = "unweighted",
                            abs = TRUE,
                            digits = 3) {
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
#' @param grp A factor of same length as `x` specifying groups.
#' If not a factor, then the vector will be coerced.
#' @param ... Additional arguments passed to `smd_num`.
#'
#' @returns A named numeric vector
smd_num_nn <- function(x, grp, ...){

  return(smd_num(rank(x), grp, ...))

}


#' SMD for categorical variables
#'
#' @param x A factor vector
#' @param grp A factor of same length as `x` specifying groups.
#' If not a factor, then the vector will be coerced.
#' @param wts A non-negative numeric vector specifying weights
#' @param denom Specifies how the denominator is calculated.
#' @param digits Integer indicating number of decimal places.
#'
#' @returns A named numeric vector

smd_fac <- function(x, grp,
                    wts = NULL,
                    denom = "unweighted",
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
  # Select options to pass to `compute_smd_fac`
  opts = as.list(match.call())[-c(1:4)]
 # Compute pairwise ASD for each group------
  x_split <- split(data.frame("x" = x,
                              "wts" = wts),
                   grp)

  smd <- utils::combn(levels(grp), 2,
               FUN = \(p, opts) {
    g1 <- p[1]
    g2 <- p[2]
    x1 <- x_split[[g1]][["x"]]
    x2 <- x_split[[g2]][["x"]]
    w1 <- x_split[[g1]][["wts"]]
    w2 <- x_split[[g2]][["wts"]]
    res <- do.call("compute_smd_fac",
                   c(list(x1 = x1, x2 = x2,
                          wts1 = w1, wts2 = w2),
                   opts))
    names(res) <- sprintf("smd_%s vs %s", g1, g2)
    return(res)
  },
  opts = opts,
  simplify = FALSE) |>
    unlist()

  return(smd)
}

# Compute SMD for factors
compute_smd_fac <- function(x1, x2,
                            wts1 = NULL,
                            wts2 = NULL,
                            denom = "unweighted",
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
  if (denom == "unweighted") {
    x1_p <- prop.table(table(x1))[-1]
    x2_p <- prop.table(table(x2))[-1]
  }

  dgl <- (x1_p*(1 - x1_p) + x2_p*(1 - x2_p))/2
  covar <- (-1*outer(x1_p, x1_p) + -1*outer(x2_p, x2_p))/2
  diag(covar) <- dgl

  # Calculate ASD
  smd <- sqrt(drop(t(d) %*% solve(covar) %*% d))
  return(round(smd, digits))
}
