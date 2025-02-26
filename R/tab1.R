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
                 nonnormal = NULL,
                 digits_num = 1,
                 digits_num_nn = 0,
                 digits_factor = 0){

  if (!is.null(nonnormal) & !is.character(nonnormal)) {
    stop("Nonnormal variables must be input as a character vector")
  }

  # Change character variables to factor

  data <- lapply(data, \(x) if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }) |>
    list2DF()

  var_names <- names(data)

  # Numeric variables

  t_num <- rapply(data[setdiff(var_names, nonnormal)],
                  summ_num,
                  "numeric",
                  how = "unlist",
                  digits = digits_num)

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
                       how = "unlist",
                       digits = digits_num_nn)

    t_num_nn <- create_summary_df(var = names(t_num_nn),
                           parent_var = NA,
                           type = "numeric_nn",
                           summ = unname(t_num_nn))
  } else {
    t_num_nn <- create_summary_df()
  }

  # Categorical variables

  t_cat <- lapply(data[sapply(data, is.factor)],
                   \(x){
                     s <- summ_cat(x, digits = digits_factor)

                     create_summary_df(
                       var = names(s),
                       parent_var = NA,
                       type = "factor",
                       summ = unname(s)
                     )
                   })


  parent_var <- rep(names(t_cat),
                    sapply(t_cat, nrow))

  t_cat <- do.call("rbind",
                   c(t_cat,
                   make.row.names = FALSE))

  t_cat["parent_var"] <- parent_var

#  t_cat <- create_summary_df()


  rbind(t_num,
        t_num_nn,
        t_cat)
}

tab1(iris, nonnormal = "Petal.Length",
     digits_factor  = 0)


