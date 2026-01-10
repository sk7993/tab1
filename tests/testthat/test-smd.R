# Test SMD calculations

test_that("smd_fac calculates correct SMD for categorical variables", {
  expect_equal(
    smd_fac(factor(mtcars$cyl), mtcars$am) |> round(3) |> unname(),
    1.251
  )
})

test_that("smd_num calculates correct SMD for numeric variables", {
  # Known case: two groups with known means and SDs
  set.seed(42)
  x <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1))
  grp <- factor(rep(c("A", "B"), each = 100))

  result <- smd_num(x, grp, abs = TRUE)

  # SMD should be approximately 1 (difference of 1 / pooled SD of ~1)
  expect_true(abs(result - 1) < 0.3)
})

test_that("smd_num returns absolute value when abs = TRUE", {
  x <- c(rep(10, 50), rep(5, 50))
  grp <- factor(rep(c("A", "B"), each = 50))

  result_abs <- smd_num(x, grp, abs = TRUE)
  result_signed <- smd_num(x, grp, abs = FALSE)


  expect_true(result_abs > 0)
  expect_equal(result_abs, abs(result_signed))
})

test_that("smd_num_nn uses ranks for calculation", {
  # Highly skewed data
  x <- c(rexp(50, rate = 1), rexp(50, rate = 0.5))
  grp <- factor(rep(c("A", "B"), each = 50))

  result_normal <- smd_num(x, grp)
  result_nn <- smd_num_nn(x, grp)

  # Results should differ since rank-based SMD is used

  expect_false(isTRUE(all.equal(result_normal, result_nn)))
})

test_that("smd requires at least two groups", {
  x <- rnorm(50)
  grp <- factor(rep("A", 50))

  expect_error(smd_num(x, grp), "At least two groups")
})

test_that("smd handles weighted data", {
  x <- c(rep(0, 50), rep(1, 50))
  grp <- factor(rep(c("A", "B"), each = 50))
  wts <- rep(1, 100)

  result_unweighted <- smd_num(x, grp)
  result_weighted <- smd_num(x, grp, wts = wts)

  # With uniform weights, results should be identical
  expect_equal(result_unweighted, result_weighted)
})

test_that("smd returns named vector for pairwise comparisons", {
  result <- smd(iris, "Species")

  # Should have SMD columns for each pair
  expect_true(any(grepl("setosa vs versicolor", names(result[[1]]))))
  expect_true(any(grepl("versicolor vs virginica", names(result[[1]]))))
})
