#' Summarize numeric variables
#'
#' @param x A numeric vector.
#' @param wts A numeric vector of weights (optional).
#' @param digits Number of digits to round to (default is 2).
#'
#' @returns Returns (weighted) arithmetic mean of values in x.
#' @export
#'
#' @examples
summ_num <- function(x, wts = NULL, digits = 2) {

  if (!(is.numeric(digits) & length(digits) == 1)) {
    error("`digits` must be a numeric vector of length 1.")
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
#' @param x
#' @param digits
#' @param delim
#'
#' @returns
#' @export
#'
#' @examples
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
#' @param x
#' @param digits
#'
#' @returns
#' @export
#'
#' @examples
summ_fac <- function(x,
                     digits = 0){
  tab <- table(x) |>
    round(digits)
  prop <- tab |>
    prop.table()
  prop <- 100*prop
  prop <- round(prop, digits)

  s <- sprintf("%s (%s%%)",
          tab,
          prop) |>
    stats::setNames(names(tab))

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
