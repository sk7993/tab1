# Test main tab1 function

test_that("tab1 returns a data frame with expected structure", {
  result <- tab1(iris, "Species")

  expect_s3_class(result, "data.frame")
  expect_true("var" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true(any(grepl("^summ_", names(result))))
  expect_true(any(grepl("^smd_", names(result))))
})

test_that("tab1 includes all numeric variables", {
  result <- tab1(iris, "Species")
  numeric_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

  for (v in numeric_vars) {
    expect_true(v %in% result$var, info = paste(v, "should be in result"))
  }
})

test_that("tab1 handles nonnormal variables", {
  result <- tab1(iris, "Species", nonnormal = "Sepal.Width")

  # Sepal.Width should be marked as numeric_nn
  expect_equal(result$type[result$var == "Sepal.Width"], "numeric_nn")

  # Other numeric vars should be normal
  expect_equal(result$type[result$var == "Sepal.Length"], "numeric")
})

test_that("tab1 handles factor variables", {
  df <- data.frame(
    group = rep(c("A", "B"), each = 50),
    category = factor(rep(c("X", "Y"), 50))
  )
  result <- tab1(df, "group")

  expect_true("category" %in% result$var)
  expect_true(any(result$type == "factor"))
  expect_true(any(result$type == "factor_lvl"))
})

test_that("tab1 includes sample sizes", {
  result <- tab1(iris, "Species")

  # Last row should be sample size
  n_row <- result[result$var == "n", ]
  expect_equal(nrow(n_row), 1)
})

test_that("tab1 includes missing value counts", {
  df <- iris
  df$Sepal.Length[1:5] <- NA

  result <- tab1(df, "Species")

  expect_true("missing" %in% names(result))
  # Sepal.Length should show missing count
  missing_val <- result$missing[result$var == "Sepal.Length"]
  expect_match(missing_val, "5")
})

test_that("tab1 respects vars parameter", {
  result <- tab1(iris, "Species", vars = c("Sepal.Length", "Petal.Length"))

  # Should only include specified vars (plus n row)
  expect_equal(sum(result$type == "numeric"), 2)
  expect_true("Sepal.Length" %in% result$var)
  expect_true("Petal.Length" %in% result$var)
  expect_false("Sepal.Width" %in% result$var)
})

test_that("tab1 validates grp parameter", {
  expect_error(tab1(iris, c("Species", "Other")), "length 1")
  expect_error(tab1(iris, "NonExistent"), "not found")
})

test_that("tab1 handles weighted data", {
  set.seed(123)
  wts <- runif(nrow(iris))
  result <- tab1(iris, "Species", wts = wts)

  expect_s3_class(result, "data.frame")
  # Sample size row should reflect effective sample size
  n_row <- result[result$var == "n", ]
  expect_equal(nrow(n_row), 1)
})

test_that("tab1 applies variable labels", {
  result <- tab1(iris, "Species",
                 vars = c("Sepal.Length", "Sepal.Width"),
                 lbl = list(Sepal.Length = "Sepal Length (cm)",
                           Sepal.Width = "Sepal Width (cm)"))

  expect_true("Sepal Length (cm)" %in% result$var)
  expect_true("Sepal Width (cm)" %in% result$var)
  expect_false("Sepal.Length" %in% result$var)
})
