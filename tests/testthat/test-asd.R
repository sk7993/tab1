test_that("ASD is calculated correctly", {

  expect_equal(smd_fac(datasets::mtcars$cyl,
                       datasets::mtcars$am) |>
                 round(3),
               1.251)
})

