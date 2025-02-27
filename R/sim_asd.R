sim_asd <- function(n, b){
  asd <- vector("numeric", b)

  for (i in seq_len(b)) {
    asd[i] <- asd_cat(c(rbinom(n, 5, 0.5),
                      rbinom(n, 5, 0.5)),
                      rep(c(0,1), each = n)
    )
  }

  return(asd)
}

sim_asd(500, 1000) |>
  quantile(c(0.025, 0.975))
