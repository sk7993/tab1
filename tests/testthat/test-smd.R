test_that("SMD is calculated correctly", {

  expect_equal(smd_fac(factor(datasets::mtcars$cyl),
                       datasets::mtcars$am) |>
                 round(3) |>
                 unname(),
               1.251)
})

