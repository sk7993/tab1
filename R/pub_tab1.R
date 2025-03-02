pub_tab1 <- function(tab1){
  names(tab1) <- gsub("summ_|smd_", "", names(tab1))
  tab1 <- tab1[,-c(2:3)]
  flextable::flextable(tab1)
}

# tab1(iris, "Species") |>
#   pub_tab1()
