#' Create publication quality table 1 from tab1 object
#'
#' @param tab1 Data frame output from `tab1` function.
#' @param lbl_nm Summary statistic label for numeric variables.
#' @param lbl_nn Summary statistic label for non-numeric variables.
#' @param lbl_fac Summary statistic label for factor variables
#'
#' @returns A flextable object.
#'
#' @import flextable
#' @export
#'
#' @examples
pub_tab1 <- function(tab1,
                     lbl_nm = "mean (SD)",
                     lbl_nn = "median [Q1, Q3]",
                     lbl_fac = "n (%)"){

  # Clean up variable names
  names(tab1) <- gsub("summ_", "", names(tab1))
  names(tab1) <- gsub("smd_", "ASD: ", names(tab1))

  # Append summary stats label to variables

  tab1$var[tab1$type == "numeric"] <- sprintf("%s, %s",
                                              tab1$var[tab1$type == "numeric"],
                                              lbl_nm)

  tab1$var[tab1$type == "numeric_nn"] <- sprintf("%s, %s",
                                                 tab1$var[tab1$type == "numeric_nn"],
                                                 lbl_nn)

  tab1$var[tab1$type == "factor"] <- sprintf("%s, %s",
                                             tab1$var[tab1$type == "factor"],
                                                 lbl_fac)

  # Identify vars that correspond to subcategories
  idx_sg <- which(tab1$type == "factor_lvl")
  tab1 <- tab1[,-c(2:3)]
  #
  res <- flextable::flextable(tab1) |>
    # Italicize and indent subcategories
    flextable::italic(i = idx_sg, j = 1) |>
    flextable::padding(i = idx_sg, j = 1,
                       padding.left = 15) |>
    flextable::autofit()

  return(res)
}


