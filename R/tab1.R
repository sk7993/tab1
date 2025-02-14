tab1 <- function(data,
                 nonnormal = NULL){
  if (!is.null(nonnormal) & !is.character(nonnormal)) {
    stop("Nonnormal variables must be input as a character vector")
  }
  df_num <- data[!names(data) %in% nonnormal]
  df_num <- df_num[sapply(df_num,
                          is.numeric)]

  df_num_nn <- data[nonnormal]

  df_cat <- data[sapply(data,
                        \(x) is.character(x) |
                          is.factor(x))]

  t_num <- sapply(df_num,
                  summ_num)

  t_num <- data.frame(var = names(t_num),
                      type = "numeric",
                      summ = unname(t_num))

  if (!is.null(nonnormal)) {
    t_num_nn <- sapply(df_num_nn,
                       summ_num_nn)

    t_num_nn <- data.frame(var = names(t_num_nn),
                           type = "numeric (nn)",
                           summ = unname(t_num_nn))
  }


  t_cat <- lapply(df_cat,
                   summ_cat)

  print(ls())

  list(t_num,
       if (exists("t_num_nn")) t_num_nn,
       t_cat)
}

tab1(mtcars,
     nonnormal = "cyl")
