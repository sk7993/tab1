# Compute ASD threshold--------
asd_threshold <- function(n1, n2, digits = 3){
  round(1.96*sqrt(1/n1 + 1/n2), 3)
}

# `rbind` wrapper that adds an ID column representing the data frame's
# source list element name-----------

rbind2 <- function(df_list, id_col = "id"){

  if (is.null(names(df_list))) {
    stop("The input list must be named.")
  }

  # Add a column to each data frame indicating its ID (list element name)
  df_list <- lapply(names(df_list), function(nm) {
    df <- df_list[[nm]]
    df[[id_col]] <- nm
    df
  })
  res <- do.call(rbind, c(df_list,
                              make.row.names = FALSE))

  sel_var <- c(id_col, names(res)[-ncol(res)])
  res <- res[,sel_var]
  return(res)
}

# Simple wrapper for `merge` that preserves order
# based on the left data frame -----------------
merge2 <- function(x, y, ...){
  nm <- "._sort&"
  x[[nm]] <- seq_len(nrow(x))
  res <- merge(x, y, ...)
  res <- res[order(res[[nm]]),]
  res[[nm]] <- NULL

  return(res)
 }
