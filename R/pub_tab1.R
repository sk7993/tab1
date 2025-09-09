#' Create publication quality table 1 from tab1 object
#'
#' @description
#' Creates a publication ready table from tab1 output using `flextable`
#'
#' @details
#' The function will output a flextable object which can be modified further
#' depending on user needs.
#'
#' If set to true, the `bin_coll` option will collapse binary categorical variables
#' to a single row. The row will display summary statistics for the second level.
#'
#' @param tab1 Data frame output from `tab1` function.
#' @param lbl_nm Summary statistic label for numeric variables.
#' @param lbl_nn Summary statistic label for non-numeric variables.
#' @param lbl_fac Summary statistic label for factor variables
#' @param bin_coll Logical variable indicating whether to collapse factor variables
#' with two categories.
#'
#' @returns A flextable object.
#'
#' @import flextable
#' @export
#'
#' @examples
#' tab1(iris, "Species") |>
#' pub_tab1()
pub_tab1 <- function(tab1,
                     lbl_nm = "mean (SD)",
                     lbl_nn = "median [Q1, Q3]",
                     lbl_fac = "n (%)",
                     bin_coll = FALSE){

  # Append summary stats label to variable names

  if (bin_coll) {
    tab1_split <- split(tab1, factor(tab1$parent_var,
                                     levels = unique(tab1$parent_var)))
    print(tab1_split)
    tab1_split <- lapply(tab1_split, function(df){
      n <- nrow(df)
      if (n==3 & df$type[1] == "factor") {
        df <- df[3,,drop=FALSE]
        df$var <- df$parent_var
        df$type <- "factor"
      }
      return(df)
    }
      )
    tab1 <- do.call(rbind, tab1_split)
  }

  tab1$var[tab1$type == "numeric"] <- sprintf("%s, %s",
                                              tab1$var[tab1$type == "numeric"],
                                              lbl_nm)

  tab1$var[tab1$type == "numeric_nn"] <- sprintf("%s, %s",
                                                 tab1$var[tab1$type == "numeric_nn"],
                                                 lbl_nn)

  tab1$var[tab1$type == "factor"] <- sprintf("%s, %s",
                                             tab1$var[tab1$type == "factor"],
                                             lbl_fac)

  # Clean up variable names
  names(tab1) <- gsub("summ_", "", names(tab1))
  names(tab1) <- gsub("smd_", "ASD: ", names(tab1))
  names(tab1)[names(tab1) == "var"] <- "Characteristic"
  names(tab1)[names(tab1) == "missing"] <- "Missing, n (%)"

  # Identify vars that correspond to subcategories
  idx_sg <- which(tab1$type == "factor_lvl")
  tab1 <- tab1[,-c(2:3)]
  #
  res <- flextable::flextable(tab1) |>
    flextable::bold(part = "header") |>
    # Italicize and indent subcategories
    flextable::italic(i = idx_sg, j = 1) |>
    flextable::padding(i = idx_sg, j = 1,
                       padding.left = 15) |>
    flextable::autofit()

  return(res)
}


