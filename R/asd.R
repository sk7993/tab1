asd_num <- function(x1, x2 = NULL, grp = NULL){

  if (is.null(x2) & is.null(grp)) {
    stop("Must specify either x2 or grp.")
  }

  if (!is.null(x2) & !is.null(grp)) {
    stop("Must specify either x2 or grp, not both.")
  }

  if (!is.null(grp)) {
    cat <- unique(grp)
    x2 <- x1[grp == cat[2]]
    x1 <- x1[grp == cat[1]]
  }
  mu1 <- mean(x1, na.rm = TRUE)
  mu2 <- mean(x2, na.rm = TRUE)
  d = mu2 - mu1
  sd_pooled <- sqrt((var(x1) + var(x2))/2)

  return(d/sd_pooled)
}

asd_num_nn <- function(x1, x2 = NULL, grp = NULL){
  if (is.null(x2) & is.null(grp)) {
    stop("Must specify either x2 or grp.")
  }

  if (!is.null(x2) & !is.null(grp)) {
    stop("Must specify either x2 or grp, not both.")
  }

  if (!is.null(x2)) {
    r <- rank(c(x1, x2))
    x1 <- r[seq_along(x1)]
    x2 <- r[seq_along(x2) + length(x1)]
  } else if (!is.null(grp)) {
    x1 <- rank(x1)
  }

  asd_num(x1, x2, grp)

}

asd_cat <- function(x1, x2, grp){

}
