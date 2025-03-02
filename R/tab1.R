#' Create Table 1
#'
#' @param data
#' @param grp
#' @param vars
#' @param nonnormal
#' @param opts
#'
#' @returns
#' @export
#'
#' @examples
tab1 <- function(data, grp, vars = NULL, nonnormal = NULL,
                 opts = NULL) {

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }

  data_sub <- data[names(data) != grp]

  if (!is.null(vars)) {
    data_sub <- data_sub[vars]
  }

  tbl1 <- by(data_sub,
             data[[grp]],
             \(x) do.call("tab1_ug", c(list(x), nonnormal,
                                                  opts))) |>
    rbind2(id = "group") |>
    # Change from long to wide format
    reshape(
      direction = "wide",
      idvar = c("var"),
      timevar = "group",
      v.names = "summ",
      sep = "_"
    )

  # Construct SMD table

  data_sub[[grp]] <- data[[grp]]

  smd <- smd(data_sub,
             grp,
             nonnormal)
  # Combine sumamry and SMD table
  if (!is.null(smd[[1]])) {

    res <- merge(tbl1[tbl1$type %in% c("numeric", "numeric_nn"),],
                 smd[[1]],
                 by = "var",
                 all.x = TRUE)
  } else {
    res <- NULL
  }

  if (!is.null(smd[[2]])){
    res2 <- merge(tbl1[tbl1$type == "factor",],
                  smd[[2]],
                  by = "parent_var",
                  all.x = TRUE)
  } else {
    res2 <- NULL
  }

  return(do.call("rbind",
                 list(res, res2))
  )
}

#' Construct table 1 (no groups)
#'
#' This function is the workhorse of the `tab1` function. It is not intended
#' to be called directly.
#'
#' @param data data
#' @param nonnormal Character vector specifiying nonnormal variables.
#' @param opts_num  (List) Options for numeric variables passed to
#' `sum_num`.
#' @param opts_num_nn (List) Options for nonnormal numeric variables
#' passed to `summ_num_nn`.
#' @param opts_fac (List) Options for factorv variables passed to `summ_fac`.
#'
#' @returns
#'
#' @examples
tab1_ug <- function(data,
                 nonnormal = NULL,
                 opts_num = NULL,
                 opts_num_nn = NULL,
                 opts_fac = NULL){

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
                      opts_fac)

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
