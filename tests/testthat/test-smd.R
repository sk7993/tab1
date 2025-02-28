test_that("SMD is calculated correctly", {

  expect_equal(smd_fac(datasets::mtcars$cyl,
                       datasets::mtcars$am) |>
                 round(3),
               1.251)
})

