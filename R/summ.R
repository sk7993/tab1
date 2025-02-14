#' Title
#'
#' @param x
#' @param digits
#' @param loc
#' @param disp
#' @param delim
#'
#' @returns
#' @export
#'
#' @examples
summ_num <- function(x, digits = 2) {
  l <- base::mean(x, na.rm = TRUE) |>
    round(digits)
  s <- stats::sd(x, na.rm = TRUE) |>
    round(digits)

  sprintf("%s (%s)",
          l,
          s)
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
summ_num_nn <- function(x,
                        digits = 0,
                        delim = 1){

  l <- stats::median(x,
                     na.rm = TRUE) |>
    round(digits)
  s <- stats::quantile(x,
                       c(0.25, 0.75),
                       na.rm = TRUE) |>
    round(0)

  if (delim == 1) {
    sprintf("%s [%s, %s]",
            l,
            s[[1]],
            s[[2]])
  } else if (delim == 2) {
    sprintf("%s [%s - %s]",
            l,
            s[[1]],
            s[[2]])
  }

}

#' Title
#'
#' @param x
#' @param digits
#'
#' @returns
#' @export
#'
#' @examples
summ_cat <- function(x,
                     digits = 0){
  tab <- table(x) |>
    round(digits)
  prop <- tab |>
    prop.table()
  prop <- 100*prop
  prop <- round(prop, digits)

  s <- sprintf("%s (%s%%)",
          tab,
          prop)

  d <- data.frame(var = names(tab),
                  type = "subcat",
                  summ = s)
  d
}

