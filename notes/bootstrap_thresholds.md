# Bootstrap/Permutation ASD Thresholds

## Current Approach

The `asd_threshold` function uses an asymptotic formula:

```r
asd_threshold <- function(n1, n2, digits = 3){
  round(1.96 * sqrt(1/n1 + 1/n2), 3)
}
```

This assumes large samples and relies on the asymptotic distribution of SMD under the null.

## Bootstrap Alternative

### Why Consider Bootstrap?

| Aspect | Analytical | Bootstrap |
|--------|-----------|-----------|
| Small samples | May be inaccurate | More reliable |
| Weighted data | Formulas get complex | Handles naturally |
| Non-normal data | Assumes normality | Distribution-free |
| Categorical (ASD) | Covariance matrix assumptions | Empirical distribution |
| Computational cost | O(1) | O(B × n) |

### Implementation Sketch: Numeric SMD

```r
bootstrap_smd_threshold <- function(x, grp, wts = NULL,
                                     B = 1000, alpha = 0.05) {
  # Pool data under null hypothesis
  n <- length(x)
  n1 <- sum(grp == levels(grp)[1])
  n2 <- sum(grp == levels(grp)[2])

  if (is.null(wts)) wts <- rep(1, n)

  # Bootstrap under null
  boot_smds <- replicate(B, {
    # Resample indices with replacement
    idx <- sample(n, replace = TRUE)
    x_boot <- x[idx]
    wts_boot <- wts[idx]

    # Assign to "groups" randomly (null hypothesis)
    grp_boot <- factor(c(rep(1, n1), rep(2, n2)))

    # Compute SMD
    compute_smd_num(
      x1 = x_boot[1:n1],
      x2 = x_boot[(n1+1):n],
      wts1 = wts_boot[1:n1],
      wts2 = wts_boot[(n1+1):n],
      abs = TRUE
    )
  })

  # Threshold is the (1-alpha) quantile
  quantile(boot_smds, 1 - alpha, na.rm = TRUE)
}
```

### Implementation Sketch: Categorical ASD

```r
bootstrap_asd_threshold <- function(x, grp, wts = NULL,
                                     B = 1000, alpha = 0.05) {
  n <- length(x)
  n1 <- sum(grp == levels(grp)[1])
  n2 <- n - n1

  if (is.null(wts)) wts <- rep(1, n)

  boot_asds <- replicate(B, {
    idx <- sample(n, replace = TRUE)
    x_boot <- x[idx]
    wts_boot <- wts[idx]

    # Random group assignment under null
    grp_boot <- factor(c(rep(1, n1), rep(2, n2)))

    compute_smd_fac(
      x1 = x_boot[1:n1],
      x2 = x_boot[(n1+1):n],
      wts1 = wts_boot[1:n1],
      wts2 = wts_boot[(n1+1):n]
    )
  })

  quantile(boot_asds, 1 - alpha, na.rm = TRUE)
}
```

## Permutation Alternative (Often Preferred)

Permutation tests are conceptually cleaner for testing the null since they keep the original data intact and only shuffle labels:

```r
permutation_smd_threshold <- function(x, grp, wts = NULL,
                                       B = 1000, alpha = 0.05) {
  n <- length(x)
  if (is.null(wts)) wts <- rep(1, n)

  n1 <- sum(grp == levels(grp)[1])

  perm_smds <- replicate(B, {
    # Permute group labels (keeps x and wts paired)
    perm_idx <- sample(n)
    grp_perm <- grp[perm_idx]

    idx1 <- which(grp_perm == levels(grp)[1])
    idx2 <- which(grp_perm == levels(grp)[2])

    compute_smd_num(
      x1 = x[idx1], x2 = x[idx2],
      wts1 = wts[idx1], wts2 = wts[idx2],
      abs = TRUE
    )
  })

  quantile(perm_smds, 1 - alpha, na.rm = TRUE)
}
```

## Integration Idea

Add optional `threshold_method` parameter:

```r
asd_threshold <- function(n1, n2, x = NULL, grp = NULL, wts = NULL,
                          method = c("asymptotic", "bootstrap", "permutation"),
                          B = 1000, alpha = 0.05, digits = 3) {
  method <- match.arg(method)

  if (method == "asymptotic") {
    return(round(1.96 * sqrt(1/n1 + 1/n2), digits))
  }

  if (is.null(x) || is.null(grp)) {
    stop("x and grp required for bootstrap/permutation methods")
  }

  # ... bootstrap or permutation implementation
}
```

## When to Use

**Bootstrap/permutation preferred:**
- Small sample sizes (n < 50 per group)
- Complex weighting schemes
- Variable-specific thresholds needed
- Non-standard distributions

**Analytical is fine:**
- Large samples
- Unweighted or simple weights
- Quick computation needed
- Many pairwise comparisons
