#' Title
#'
#' @param tab1
#' @param lbl A named list of the form `list(old_name = new_name)` to rename the
#' variables in tab1. Must be of the same length as `var` col in tab1.
#'
#' @returns
#'
#' @import flextable
#' @export
#'
#' @examples
pub_tab1 <- function(tab1, lbl = NULL){
  names(tab1) <- gsub("summ_", "", names(tab1))
  names(tab1) <- gsub("smd_", "ASD: ", names(tab1))

  if (!is.null(lbl)) {
    if (all(names(lbl) %in% tab1[["var"]])) {
      tab1[["var"]] <- unlist(lbl[tab1[["var"]]])
    } else {
      stop("`lbl` must be a named list with names matching `tab1$var`.")
    }
  }
  idx_sg <- which(!is.na(tab1$parent_var))
  tab1 <- tab1[,-c(2:3)]

  flextable::flextable(tab1) |>
    flextable::italic(i = idx_sg, j = 1) |>
    flextable::padding(i = idx_sg, j = 1,
                       padding.left = 10)
}


