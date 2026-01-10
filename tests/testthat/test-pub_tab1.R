# Test pub_tab1 function

test_that("pub_tab1 returns a flextable object", {
  t1 <- tab1(iris, "Species")
  result <- pub_tab1(t1)

  expect_s3_class(result, "flextable")
})

test_that("pub_tab1 appends statistic labels to variable names", {
  t1 <- tab1(iris, "Species")
  result <- pub_tab1(t1)

  # Extract the data from flextable
  ft_data <- result$body$dataset

  # Numeric variables should have "mean (SD)" appended
  expect_true(any(grepl("mean \\(SD\\)", ft_data$Characteristic)))
})

test_that("pub_tab1 uses custom labels", {
  t1 <- tab1(iris, "Species")
  result <- pub_tab1(t1,
                     lbl_nm = "M (SD)",
                     lbl_nn = "Mdn [IQR]")

  ft_data <- result$body$dataset

  expect_true(any(grepl("M \\(SD\\)", ft_data$Characteristic)))
})

test_that("pub_tab1 collapses binary variables when bin_coll = TRUE", {
  df <- data.frame(
    group = rep(c("A", "B"), each = 50),
    binary_var = factor(rep(c("No", "Yes"), 50))
  )
  t1 <- tab1(df, "group")

  result_expanded <- pub_tab1(t1, bin_coll = FALSE)
  result_collapsed <- pub_tab1(t1, bin_coll = TRUE)

  # Collapsed should have fewer rows
  n_expanded <- nrow(result_expanded$body$dataset)
  n_collapsed <- nrow(result_collapsed$body$dataset)

  expect_lt(n_collapsed, n_expanded)
})

test_that("pub_tab1 renames SMD columns to ASD", {
  t1 <- tab1(iris, "Species")
  result <- pub_tab1(t1)

  col_names <- names(result$body$dataset)

  expect_true(any(grepl("^ASD:", col_names)))
  expect_false(any(grepl("^smd_", col_names)))
})
