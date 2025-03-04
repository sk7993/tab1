#' Create Table 1
#'
#' @param data A data frame.
#' @param grp A character variable specifying the grouping variable.
#' @param vars A character vector specifying the variables to summarize.
#' @param nonnormal A character vector specifying which variables to treat as
#' skewed/nonnormal.
#' @param lbl A named list of the form `list(old_name = new_name)` to rename the
#' variables in tab1. Must be of the same length as `var` col in tab1 (optional)
#' @param opts_summ A list specifying additional options for summarizing.
#'
#' @returns
#' @export
#'
#' @examples
tab1 <- function(data, grp,
                 vars = NULL,
                 nonnormal = NULL,
                 lbl = NULL,
                 opts_summ = list(num_digits = 2,
                             numnn_digits = 0,
                             fac_digits = 1)) {

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }

  if (!is.null(lbl)) {
    if (all(names(lbl) %in% names(data))) {
      names(data) <- unlist(lbl[names(data)])
      grp <- lbl[[grp]]
    } else {
      stop("`lbl` must be a named list with names matching variables in the dataframe.")
    }
  }

  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }

  # Subset data based on specified vars--------------
  data_sub <- data[names(data) != grp]

  if (!is.null(vars)) {
    data_sub <- data_sub[vars]
  }

  # Get summary stats by group
  tbl1 <- by(data_sub,
             data[[grp]],
             \(x) do.call("tab1_ug", c(list(x),
                                       nonnormal,
                                       opts_summ))) |>
    rbind2(id = "group") |>
    # Change from long to wide format
    reshape(
      direction = "wide",
      idvar = c("var", "parent_var"),
      timevar = "group",
      v.names = "summ",
      sep = "_"
    )

  # Get SMD by group
  data_sub[[grp]] <- data[[grp]]

  smd <- smd(data_sub,
             grp,
             nonnormal)

  # Combine summary and SMD by group
  if (!is.null(smd[[1]])) {

    res1 <- merge(tbl1[tbl1$type %in% c("numeric", "numeric_nn"),],
                 smd[[1]],
                 by = "var",
                 all.x = TRUE)
  } else {
    res1 <- NULL
  }

  if (!is.null(smd[[2]])){
    res2 <- merge(tbl1[tbl1$type %in% c("factor", "factor_lvl"),],
                  smd[[2]],
                  by = "parent_var",
                  all.x = TRUE)
  } else {
    res2 <- NULL
  }

  return(do.call("rbind",
                 list(res1, res2))
  )
}

#' Construct table 1 (no groups)
#'
#' This function is the workhorse of the `tab1` function. It is not intended
#' to be called directly.
#'
#' @param data A data frame.
#' @param num_digits Number of digits for numeric variables.
#' @param num_nn_digits Number of digits for sumamry of non-normal numeric variables.
#' @param fac_digits Number of digits for summaries of factor variables.
#' @param nonnormal Character vector specifying non-normal variables.
#'
#' @returns
#'
#' @examples
tab1_ug <- function(data,
                    wts = NULL,
                    nonnormal = NULL,
                    num_digits = 2,
                    numnn_digits = 0,
                    fac_digits = 1
                 ){

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
  t_num <- do.call("tab1_num",
                   c(list(data[setdiff(var_names, nonnormal)]),
                     list(wts = wts),
                     digits = num_digits)
                   )

  t_num_nn <- do.call("tab1_num_nn",
                      c(list(data[nonnormal]),
                        list("wts" = wts),
                        list(digits = numnn_digits)))

  t_fac <- do.call("tab1_fac",
                   c(list(data),
                     list(wts = wts),
                     digits = fac_digits))

  miss <- sapply(data, \(x) sum(is.na(x)))
  miss <- data.frame(var = names(miss),
                     missing = unname(miss))

  res <- rbind(t_num,
               t_num_nn,
               t_fac)

  res <- merge(res, miss, by = "var",
               all.x = TRUE,
               sort = FALSE)

  # Combine summary tables-----------------
  return(res)
}

#' Table 1 for numeric variables
#'
#' @param data
#' @param opts
#'
#' @returns
#'
#' @examples
tab1_num <- function(data, wts = NULL, digits = 2, ...) {

  args <- as.list(match.call())[-(1:2)]

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
                      c(list(x), args)),
         "numeric",
         how = "unlist")

  res <- create_summary_df(var = names(res),
                           parent_var = names(res),
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
tab1_num_nn <- function(data, wts = NULL, digits = 2, ...) {

  args <- as.list(match.call())[-(1:2)]

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
                      c(list(x), args)),
         "numeric",
         how = "unlist")

  res <- create_summary_df(var = names(res),
                           parent_var = NA,
                           type = "numeric_nn",
                           summ = unname(res))

  return(res)
}


#' Table 1 for factor variables
#'
#' @param data
#' @param opts
#'
#' @returns
#'
#' @examples
tab1_fac <- function(data, wts = NULL, digits = 0, ...) {

  args <- as.list(match.call())[-(1:2)]

  # Checks---------------------------
  if (nrow(data) == 0 |
      ncol(data) == 0) {
    return(create_summary_df())
  }

  if(sum(sapply(data, is.factor)) == 0) {
    return(create_summary_df())
  }
  factor_vars <- names(data)[sapply(data, is.factor)]
  # Summarize every factor variable in  the dataset----
  # Outputs list whose elements are dataframes
  res <- lapply(factor_vars, \(x){
    d <- data[[x]]
    s <- do.call(summ_fac, c(list(d), args))
    s <- create_summary_df(
      var = c(x, names(s)),
      parent_var = x,
      type = c("factor",
               rep("factor_lvl",
                   length(s))),
      summ = c(NA, unname(s))
    )
    return(s)
  })
  # Remove NULL elements from the list----
  # (i.e., non-factor variables)

  res <- do.call("rbind", res)

  return(res)
}
