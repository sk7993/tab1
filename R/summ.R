#' Summarize numeric variables
#'
#' @param x A numeric vector.
#' @param wts A numeric vector of weights (optional).
#' @param digits Number of digits to round to (default is 2).
#'
#' @returns A formatted string "mean (SD)".
summ_num <- function(x, wts = NULL, digits = 2) {

  if (!(is.numeric(digits) & length(digits) == 1)) {
    stop("`digits` must be a numeric vector of length 1.")
  }

  len <- length(x)
  if (is.null(wts)) {
    wts <- rep(1, times = len)
  }

  loc <- wtd.mean(x, wts, na.rm = TRUE) |>
    round(digits)
  scale <- wtd.var(x, wts, na.rm = TRUE) |>
    sqrt() |>
    round(digits)

  sprintf("%s (%s)",
          loc,
          scale)
}

#' Summarize non-normal numeric variables
#'
#' @param x A numeric vector.
#' @param wts A numeric vector of weights (optional).
#' @param digits Number of digits to round to (default is 2).
#' @param delim Delimiter to use for separating values.
#'
#' @returns A string with median [Q1, Q3]
summ_num_nn <- function(x,
                        wts = NULL,
                        digits = 0,
                        delim = ","){

  len <- length(x)
  if (is.null(wts)) {
    wts <- rep(1, times = len)
  }

  loc <- wtd.quantile(x,
                      wts,
                      0.5,
                     na.rm = TRUE) |>
    round(digits)
  scale <- wtd.quantile(x,
                        wts,
                       c(0.25, 0.75),
                       na.rm = TRUE) |>
    round(digits)

  # Return
    result <- sprintf("%s [%s%s %s]",
            loc,
            scale[[1]],
            delim,
            scale[[2]])

    return(result)

}

#' Summarize categorical variables
#'
#' @param x A factor vector.
#' @param wts A non-negative vector of weights.
#' @param digits Number of decimal places to use.
#'
#' @returns A named character vector with "n (%)" for each level, or just "%" if weighted.
summ_fac <- function(x,
                     wts = NULL,
                     digits = 0){

  len <- length(x)
  if (is.null(wts)) {
    wts <- rep(1, times = len)
  }

  tab <- wtd_tbl(x, wts) |>
    round(digits)
  prop <- tab |>
    prop.table()
  prop <- 100*prop
  prop <- round(prop, digits)

  if (all(wts == 1)){
    s <- sprintf("%s (%s%%)",
                 tab,
                 prop) |>
      stats::setNames(names(tab))
  } else {
    s <- sprintf("%s%%", prop) |>
      stats::setNames(names(tab))
  }

  return(s)
}

create_summary_df <- function(var = NULL,
                              parent_var = NULL,
                              type = NULL,
                              summ = NULL){
  data.frame(
    "var" = var,
    "parent_var" = parent_var,
    "type" = type,
    "summ" = summ
  )
}
