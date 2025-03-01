tab1 <- function(data, grp, ...) {
  tbl1 <- by(data, grp, \(x) do.call("tab1_ug", c(list(x), ...))) |>
    rbind2(id = "group") |>
    # Change from long to wide format
    reshape(
      direction = "wide",
      idvar = c("var"),
      timevar = "group",
      v.names = "summ",
      sep = "_"
    )

  # Add SMD

  return(tbl1)
}

#' Construct table 1 (without groups)
#'
#' This function is the workhorse of the `tab1` function but not intended
#' to be called directly.
#'
#' @param data
#' @param nonnormal
#'
#' @returns
#'
#' @examples
tab1_ug <- function(data,
                 nonnormal = NULL,
                 opts_num = NULL,
                 opts_num_nn = NULL,
                 opts_cat = NULL){

  if (!is.null(nonnormal) & !is.character(nonnormal)) {
    stop("Nonnormal variables must be input as a character vector")
  }

  # Change character variables to factor----------
  var_names <- names(data)

  data <- rapply(data,
                 factor,
                 "character",
                 how = "replace")

  # Get summary tables--------------
  t_num <- tab1_num(data[setdiff(var_names, nonnormal)],
                      opts_num)

  t_num_nn <- tab1_num_nn(data[nonnormal],
                            opts_num_nn)

  t_fac <- tab1_fac(data,
                      opts_cat)

  # Combine summary tables-----------------
  return(rbind(t_num,
        t_num_nn,
        t_fac))
}

#' Table 1 for numeric variables
#'
#' @param data
#' @param opts
#'
#' @returns
#'
#' @examples
tab1_num <- function(data, opts = NULL) {

  # Checks

  if (nrow(data) == 0 |
      ncol(data) == 0) {
    return(create_summary_df())
  }

  if(sum(sapply(data, is.numeric)) == 0) {
    return(create_summary_df())
  }

  # Output summary

  res <- rapply(data,
         \(x) do.call(summ_num,
                      c(list(x), opts)),
         "numeric",
         how = "unlist")

  res <- create_summary_df(var = names(res),
                           parent_var = NA,
                           type = "numeric",
                           summ = unname(res))

  return(res)
}

#' Table 1 for skewed/non-normal numeric variables
#'
#' @param data
#' @param opts
#'
#' @returns
#'
#' @examples
tab1_num_nn <- function(data, opts = NULL) {

  # Checks-------------------
  if (nrow(data) == 0 |
      ncol(data) == 0) {
    return(create_summary_df())
  }

  if(sum(sapply(data, is.numeric)) == 0) {
    return(create_summary_df())
  }

  # Output summary--------------------------
  res <- rapply(data,
         \(x) do.call(summ_num_nn,
                      c(list(x), opts)),
         "numeric",
         how = "unlist")

  res <- create_summary_df(var = names(res),
                           parent_var = NA,
                           type = "numeric_nn",
                           summ = unname(res))

  return(res)
}


#' Categorical Table 1
#'
#' @param data
#' @param opts
#'
#' @returns
#'
#' @examples
tab1_fac <- function(data, opts = NULL) {

  # Checks---------------------------
  if (nrow(data) == 0 |
      ncol(data) == 0) {
    return(create_summary_df())
  }

  if(sum(sapply(data, is.factor)) == 0) {
    return(create_summary_df())
  }

  # Summarize every factor variable in  the dataset----
  # Outputs list whose elements are dataframes
  res <- rapply(data,
                  \(x) do.call("summ_fac_df", c(list(x), opts)),
                  "factor",
                  how = "list"
                  )
  # Remove NULL elements from the list----
  # (i.e., non-factor variables)
  res <- Filter(Negate(is.null), res)
  res <- rbind2(res, "parent_var")

  return(res)
}
