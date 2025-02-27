test_that("ASD is calculated correctly", {

  expect_equal(asd_cat(mtcars$cyl[mtcars$am == 0],
                                 mtcars$cyl[mtcars$am == 1]) |>
                 round(3),
               1.251)
})

