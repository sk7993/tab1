#' Title
#'
#' @param data
#' @param nonnormal
#'
#' @returns
#' @export
#'
#' @examples
tab1 <- function(data,
                 nonnormal = NULL){

  if (!is.null(nonnormal) & !is.character(nonnormal)) {
    stop("Nonnormal variables must be input as a character vector")
  }

  var_names <- names(data)

  # Numeric variables

  t_num <- rapply(data[setdiff(var_names, nonnormal)],
                  summ_num,
                  "numeric",
                  how = "unlist")

  if (is.null(t_num)) {
    t_num <- create_summary_df()
  }

  t_num <- create_summary_df(var = names(t_num),
                      parent_var = NA,
                      type = "numeric",
                      summ = unname(t_num))

  # Non-numeric variables

  if (!is.null(nonnormal)) {
    t_num_nn <- rapply(data[nonnormal],
                       summ_num_nn,
                       how = "unlist")

    t_num_nn <- create_summary_df(var = names(t_num_nn),
                           parent_var = NA,
                           type = "numeric_nn",
                           summ = unname(t_num_nn))
  } else {
    t_num_nn <- create_summary_df()
  }

  # Categorical variables

  df_cat <- data[sapply(data,
                        \(x) is.character(x) |
                          is.factor(x))]

  # t_cat <- lapply(df_cat,
  #                  summ_cat)
  #
  # parent_var <- rep(names(t_cat),
  #                   sapply(t_cat, nrow))
  #
  # t_cat <- do.call("rbind",
  #                  c(t_cat,
  #                  make.row.names = FALSE))
  #
  # t_cat["parent_var"] <- parent_var

  t_cat <- create_summary_df()


  rbind(t_num,
        t_num_nn,
        t_cat)
}

tab1(iris, nonnormal = "Petal.Length")


