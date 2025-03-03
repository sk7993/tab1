#' Summarize numeric variables
#'
#' @param x
#' @param digits
#'
#' @returns
#' @export
#'
#' @examples
summ_num <- function(x, digits = 2, wts = NULL) {
  len <- length(x)
  if (is.null(wts)) {
    wts <- rep(1, times = len)
  }
  loc <- matrixStats::weightedMean(x, wts, na.rm = TRUE) |>
    round(digits)
  scale <- stats::sd(x, na.rm = TRUE) |>
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
                        digits = 0,
                        delim = ","){

  loc <- stats::median(x,
                     na.rm = TRUE) |>
    round(digits)
  scale <- stats::quantile(x,
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
