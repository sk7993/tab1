g <- rep(c(1,2),
         each = 75)
wts <- rnorm(150, 5)
b <- rbinom(150, 1, 0.4)

cobalt::col_w_smd(iris,
                  g,
                  weights = wts,
                  abs = TRUE)

rapply(iris, smd_num, "numeric", grp = g, wts = wts)


cobalt::col_w_smd(iris,)


cobalt::col_w_smd(b,
                  g,
                  #weights = wts,
                  abs = TRUE)

smd_fac(b,
        grp = g)



###

iris2 <- cbind(iris, g = rep(c(1,2),
                                  each = 75))

cobalt::bal.tab(iris2, iris2$g, s.d.denom = "pooled", abs = TRUE,
                continuous = "std")[[1]]
smd(iris2, "g")
tableone::CreateTableOne(names(iris2),
                          "g",
                          iris2) |>
  tableone::ExtractSmd()

### Check if SMD calculation matches up with cobalt
set.seed(123)
iris2 <- cbind(iris, g = rep(c(1,2),
                             each = 75))
wts <- rnorm(150, 5)

smd(iris2 |> splitfactor() |>
      rapply(as.factor,
             "integer",
             how = "replace"
             ),  "g", wts)
col_w_smd(iris2 |> splitfactor(), iris2$g, weights = wts, abs = TRUE) |>
  round(3)
