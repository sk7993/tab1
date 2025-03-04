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

###
set.seed(123)
iris2 <- cbind(iris, g = rep(c(1,2),
                             each = 75))
wts <- rnorm(150, 5)

tab1(iris2, "g", wts = w)
bal.tab(iris2, iris2$g, weights = w)

###
library(tableone)
library(survey)

## Create a weighted survey design object
data(nhanes)
nhanesSvy <- svydesign(ids = ~ 1, weights = ~ WTMEC2YR, data = nhanes)

## Create a table object
## factorVars are converted to factors; no need for variables already factors
## strata will stratify summaries; leave it unspecified for overall summaries
svyCreateTableOne(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
                          strata = "RIAGENDR", data = nhanesSvy,
                          factorVars = c("race","RIAGENDR"), test = FALSE) |>
  print(smd = TRUE)

tab1(nhanes |>
       dplyr::mutate(race = factor(race)),
     "RIAGENDR",
     nhanes$WTMEC2YR,
     vars = c("HI_CHOL","race","agecat"))
