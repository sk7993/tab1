#' Create Table 1
#' @description
#' Creates a table with (weighted) summary statistics and pairwise SMDs from a given
#' data frame.
#'
#' @details
#' The summary statistics returned are mean (SD), median [Q1, Q3]
#' and n (%) for numeric, non-normal numeric and factor variables respectively.
#' If using weights, the function will only report proportions for the variables
#' since counts are not meaningful due to weights not being normalized. Furthermore,
#' the sample size reported will be the effective sample size for each group calculated as
#' sum(w)^2/sum(w^2), where w is the vector of weights.
#' For weighted data, the standardized mean difference (SMD) can be calculated using
#' the weighted or unweighted pooled variance in the denominator. However,
#' using the weighted pooled variance may result in a paradoxical situation where
#' the mean difference decreases but the SMD increases. It is thus recommended
#' that the unweighted pooled variance be used to ensure that the same
#' standardization factor is used both before and after weighting.
#'
#' @param data A data frame.
#' @param grp A character variable specifying the grouping variable.
#' @param wts A (non-negative) numeric vector of weights (optional). See details.
#' @param vars A character vector specifying the variables to summarize.
#' @param nonnormal A character vector specifying variables to be treated as
#' skewed/nonnormal.
#' @param lbl A named list of the form `list(old_name = new_name)` to rename the
#' variables in tab1. Must be of the same length as the `vars` parameter (optional).
#' @param opts_summ A list specifying additional options for summarizing variables.
#' Currently supports the following options:
#'  -
#' @param opts_smd A list specifying options for SMD calculations. Supports the
#' following options:
#'  - denom: A character vector specifying how to handle denominators when
#'  calculating SMD. See details.
#'  - abs: Logical specifying whether to return absolute SMD. Default is TRUE.
#'
#'
#' @returns A data frame with summary statistics and pairwise SMDs.
#' @export
#'
#' @examples
#' # Run with defaults
#' tab1(iris, grp = "Species")
#' # Available options
#' tab1(iris, grp = "Species", nonnormal = "Sepal.Width",
#' opts_summ = list(num_digits = 3, numnn_digits = 2),
#' opts_smd = list(abs = FALSE))
#' # Weighted data
#' set.seed(123)
#' w <- rnorm(length(iris))
#' # IMPORTANT: Note the difference in how denominator is calculated
#' tab1(iris, "Species")
#' tab1(iris, "Species", opts_smd = list(denom = "weighted"))
tab1 <- function(data, grp,
                 wts = NULL,
                 vars = NULL,
                 nonnormal = NULL,
                 lbl = NULL,
                 opts_summ = list(num_digits = 2,
                             numnn_digits = 0,
                             fac_digits = 1),
                 opts_smd = list(denom = "unweighted",
                                 abs = TRUE)) {

  if (!is.character(grp) | length(grp) != 1) {
    stop("`grp` must be a character vector of length 1.")
  }


  if (is.null(data[[grp]])) {
    stop("`grp` variable not found in `data`.")
  }

  if (is.null(wts)) {
    wts <- rep(1, nrow(data))
  }

  if (nrow(data) != length(wts)) {
    stop("Data frame and weights must have the same length.")
  }

  if (is.null(vars)) {
    vars <- names(data)[names(data) != grp]
  }

  get_lbl_nm <- function(lbl, old_names){
    # Helper function that returns
    # new label names given original variable names
    lbl[old_names] |> unlist()
  }

  # Subset data based on specified vars--------------
  data_sub <- data[vars]

  # Replace original var names with labels where applicable ---------
  if (!is.null(lbl)) {
    chk <- identical(sort(names(data_sub)),
                     sort(names(lbl)))
    if (chk) {
      names(data_sub) <- get_lbl_nm(lbl, names(data_sub))
      nonnormal = get_lbl_nm(lbl, nonnormal)
      vars <- unname(lbl) |> unlist()
      rm(chk)
    } else {
      stop("`lbl` must be a named list of the form `list(old_name = new_name)` with the same length as `vars` argument or number of variables in dataframe.")
    }
  }

  # Get summary stats by group
  opts_summ <- c(list(nonnormal = nonnormal),
                 opts_summ)

  tab <- by(cbind(data_sub, "__w" = wts),
             data[[grp]],
             \(x, opts) do.call("tab1_ug",
                                c(list(x[,-ncol(x), drop = FALSE]),
                                  list("wts" = x[[ncol(x)]]),
                                  opts)),
             opts = opts_summ)|>
    rbind2(id_col = "group") |>
    # Change from long to wide format
    reshape(
      direction = "wide",
      idvar = c("var", "parent_var", "type"),
      timevar = "group",
      v.names = "summ",
      sep = "_"
    )

  # Missing-------
  miss <- sapply(data_sub, \(x) sum(is.na(x)))
  miss_perc <- sapply(data_sub, \(x) round(100*sum(is.na(x))/length(x), 2))
  miss <- data.frame(var = names(miss),
                     missing = sprintf("%s (%s%%)",
                                       miss,
                                       miss_perc))

  tab <- merge2(tab, miss,
                by = "var",
                all.x = TRUE)

  # Get SMD by group---------
  data_sub[[grp]] <- data[[grp]]
  opts_smd <- c(list(grp = grp,
                   wts = wts,
                   nonnormal = nonnormal),
                opts_smd)
  smd <- do.call("smd",
                 c(list(data_sub),
                   opts_smd))

  # Combine summary and SMD by group
  if (!is.null(smd[[1]])) {

    res1 <- merge2(tab[tab$type %in% c("numeric", "numeric_nn"),],
                 smd[[1]],
                 by = "var",
                 all.x = TRUE)
  } else {
    res1 <- NULL
  }

  if (!is.null(smd[[2]])){
    res2 <- merge2(tab[tab$type %in% c("factor", "factor_lvl"),],
                  smd[[2]],
                  by = c("var", "parent_var"),
                  all.x = TRUE)
  } else {
    res2 <- NULL
  }

  res <- do.call("rbind",
                 list(res1, res2))
  # Relocate missing col to last
  miss <- res[["missing"]]
  res <- res[,-match("missing", names(res))]
  res[["missing"]] <- miss

  # Change variables to original order

  res["order"] = match(res$parent_var,
                         vars)
  res <- res[order(res$order),]
  res["order"] = NULL

  # Add sample sizes
  if (all(wts == 1)) {
    n <- tapply(data_sub, data[[grp]], nrow)
  } else {
    n <- tapply(wts, data[[grp]], ess)
  }

  res[nrow(res) + 1,] <- c("n", "n", "other",
                           n,
                           rep(NA,
                               ncol(res) - length(n) - 3))

  return(res)
}

#' Construct table 1 (no groups)
#'
#' This function is the workhorse of the `tab1` function. It is not intended
#' to be called directly.
#'
#' @param data A data frame.
#' @param wts A non-negative numeric vector of weights.
#' @param num_digits Number of digits for numeric variables.
#' @param numnn_digits Number of digits for sumamry of non-normal numeric variables.
#' @param fac_digits Number of digits for summaries of factor variables.
#' @param nonnormal Character vector specifying non-normal variables.
#'
#' @returns A data frame.

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

  # Combine summary tables-----------------
  res <- rbind(t_num,
               t_num_nn,
               t_fac)

  return(res)
}

#' Table 1 for numeric variables
#'
#' @param data A data frame
#' @param wts A non-negative numeric vector of weights.
#' @param digits Number of digits to use for summary stats.
#' @param ... Other arguments passed to summ_num.
#'
#' @returns A data frame.

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
#' @param data A data frame.
#' @param wts A non-negative numeric vector of weights.
#' @param digits Number of digits to use for summary stats.
#' @param ... Other arguments based to summ_num_nn.
#'
#' @returns A data frame with summary statistics.
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
                           parent_var = names(res),
                           type = "numeric_nn",
                           summ = unname(res))

  return(res)
}


# Table 1 for factor variables-----
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
  res <- lapply(factor_vars, \(x, args){
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
    return(s)},
    args = args)

  res <- do.call("rbind", res)

  return(res)
}

# Function calculates the effective sample size give a vector of weights
ess <- function(w){
  num <- sum(w, na.rm = TRUE)^2
  denom <- sum(w^2, na.rm = TRUE)
  return(num/denom)
}
