# Test summary functions

test_that("summ_num returns mean (SD) format", {
  x <- c(1, 2, 3, 4, 5)
  result <- summ_num(x, digits = 1)

  expect_match(result, "^[0-9.]+ \\([0-9.]+\\)$")
  expect_equal(result, "3 (1.6)")
})

test_that("summ_num handles weighted data", {
  x <- c(1, 2, 3)
  wts <- c(1, 1, 8)  # Heavy weight on 3

  result <- summ_num(x, wts = wts, digits = 1)

  # Weighted mean should be closer to 3 than unweighted mean of 2
  expect_match(result, "^2\\.7")
})

test_that("summ_num_nn returns median [Q1, Q3] format", {
  x <- 0:100
  result <- summ_num_nn(x, digits = 0)

  expect_match(result, "^[0-9]+ \\[[0-9]+, [0-9]+\\]$")
  expect_equal(result, "50 [25, 75]")
})

test_that("summ_fac returns n (%) format for unweighted data", {
  x <- factor(c("A", "A", "A", "B", "B"))
  result <- summ_fac(x, digits = 0)

  expect_equal(result[["A"]], "3 (60%)")
  expect_equal(result[["B"]], "2 (40%)")
})

test_that("summ_fac returns % format for weighted data", {
  x <- factor(c("A", "A", "B", "B"))
  wts <- c(1, 1, 3, 3)  # B has more weight
  result <- summ_fac(x, wts = wts, digits = 0)

  # A: 2/8 = 25%, B: 6/8 = 75%
  expect_equal(result[["A"]], "25%")
  expect_equal(result[["B"]], "75%")
})

test_that("summ_fac handles single-level factor", {
  x <- factor(rep("A", 10))
  result <- summ_fac(x, digits = 0)

  expect_equal(result[["A"]], "10 (100%)")
})
