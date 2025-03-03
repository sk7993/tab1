#' Title
#'
#' @param tab1
#'
#' @returns
#'
#' @import flextable
#' @export
#'
#' @examples
pub_tab1 <- function(tab1,
                     lbl_nm = "mean (SD)",
                     lbl_nn = "median [Q1, Q3]",
                     lbl_fac = "n (%)"){
  names(tab1) <- gsub("summ_", "", names(tab1))
  names(tab1) <- gsub("smd_", "ASD: ", names(tab1))

  tab1$var[tab1$type == "numeric"] <- sprintf("%s, %s",
                                              tab1$var[tab1$type == "numeric"],
                                              lbl_nm)

  tab1$var[tab1$type == "numeric_nn"] <- sprintf("%s, %s",
                                                 tab1$var[tab1$type == "numeric_nn"],
                                                 lbl_nn)

  tab1$var[tab1$type == "factor"] <- sprintf("%s, %s",
                                             tab1$var[tab1$type == "factor"],
                                                 lbl_fac)

  idx_sg <- which(tab1$type == "factor_lvl")
  tab1 <- tab1[,-c(2:3)]

  res <- flextable::flextable(tab1) |>
    flextable::italic(i = idx_sg, j = 1) |>
    flextable::padding(i = idx_sg, j = 1,
                       padding.left = 10) |>
    flextable::autofit()

  return(res)
}


