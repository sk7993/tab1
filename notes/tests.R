tab1(warpbreaks, "tension") |>
  pub_tab1()

tab1(warpbreaks, "tension",
     lbl = list("breaks" = "A",
                "wool" = "Y",
                "tension" = "III")) |>
  pub_tab1()

tab1(iris, "Species") |>
  pub_tab1()

tab1(iris, "Species", nonnormal = "Petal.Length") |>
  pub_tab1()

wtd.
