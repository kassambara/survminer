# Internal weighted log-rank test implementation
# Replaces dependency on survMisc::ten() and survMisc::comp()
#
# Supports weight methods: "1" (log-rank), "n" (Gehan-Breslow),
# "sqrtN" (Tarone-Ware), "S1" (Peto-Peto), "S2" (modified Peto-Peto),
# "FH_p=1_q=1" (Fleming-Harrington)

# Compute a weighted log-rank test
#
# @param formula a survival formula (e.g. Surv(time, status) ~ group)
# @param data a data frame
# @param method weight method: "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1"
# @param test.for.trend logical; if TRUE, compute 1-df trend test
# @return list with pvalue and method_name
.weighted_logrank_test <- function(formula, data, method = "1",
                                   test.for.trend = FALSE) {
  # Build model frame
  mf <- stats::model.frame(formula, data)
  surv_obj <- mf[[1]]
  time <- surv_obj[, "time"]
  status <- surv_obj[, "status"]
  # Ensure status is 0/1
  if (max(status) == 2) status <- status - 1
  group <- as.factor(mf[[2]])
  groups <- levels(group)
  ngroups <- length(groups)

  # Distinct event times (where at least one event occurred)
  event_times <- sort(unique(time[status == 1]))

  # Build per-time-point, per-group counts
  n_times <- length(event_times)
  # n_jk[i, j] = number at risk in group j at time event_times[i]
  # d_jk[i, j] = number of events in group j at time event_times[i]
  n_jk <- matrix(0, nrow = n_times, ncol = ngroups)
  d_jk <- matrix(0, nrow = n_times, ncol = ngroups)

  for (j in seq_len(ngroups)) {
    g_mask <- (group == groups[j])
    g_time <- time[g_mask]
    g_status <- status[g_mask]
    for (i in seq_len(n_times)) {
      t_i <- event_times[i]
      n_jk[i, j] <- sum(g_time >= t_i)
      d_jk[i, j] <- sum(g_time == t_i & g_status == 1)
    }
  }

  # Totals across groups
  n_i <- rowSums(n_jk)  # total at risk at each time
  d_i <- rowSums(d_jk)  # total events at each time

  # Compute weights
  w <- switch(method,
    "1" = rep(1, n_times),
    "n" = n_i,
    "sqrtN" = sqrt(n_i),
    "S1" = {
      # Peto-Peto: w = prod(1 - d_k / (n_k + 1))
      cumprod(1 - d_i / (n_i + 1))
    },
    "S2" = {
      # Modified Peto-Peto: w = S1 * n / (n + 1)
      s1 <- cumprod(1 - d_i / (n_i + 1))
      s1 * n_i / (n_i + 1)
    },
    "FH_p=1_q=1" = {
      # Fleming-Harrington(p=1, q=1): w = S_KM(t-) * (1 - S_KM(t-))
      # S_KM(t-) is the left-continuous KM estimate (value just before t)
      surv_km <- cumprod(1 - d_i / n_i)
      # S(t-) for first time is 1, then lagged KM
      s_left <- c(1, surv_km[-n_times])
      s_left * (1 - s_left)
    },
    stop("Unknown weight method: ", method)
  )

  # Test name mapping
  test_names <- c(
    "1" = "Log-rank",
    "n" = "Gehan-Breslow",
    "sqrtN" = "Tarone-Ware",
    "S1" = "Peto-Peto",
    "S2" = "modified Peto-Peto",
    "FH_p=1_q=1" = "Fleming-Harrington (p=1, q=1)"
  )
  test_name <- test_names[method]

  if (test.for.trend) {
    # 1-df trend test using integer group scores
    scores <- seq_len(ngroups)
    # Compute Q = sum_j score_j * sum_i w_i * (d_ij - e_ij)
    # and Var = sum_j score_j^2 * var_j - (sum_j score_j * var_j)^2 / sum var_j
    # where e_ij = n_ij * d_i / n_i, var component from covariance matrix

    # U_j = sum_i w_i * (d_ij - n_ij * d_i / n_i) for each group j
    U <- numeric(ngroups)
    for (j in seq_len(ngroups)) {
      e_ij <- n_jk[, j] * d_i / n_i
      U[j] <- sum(w * (d_jk[, j] - e_ij))
    }

    # Covariance matrix V[j, k]
    V <- matrix(0, ngroups, ngroups)
    for (i in seq_len(n_times)) {
      if (n_i[i] <= 1) next
      factor_i <- w[i]^2 * d_i[i] * (n_i[i] - d_i[i]) / (n_i[i]^2 * (n_i[i] - 1))
      for (j in seq_len(ngroups)) {
        for (k in seq_len(ngroups)) {
          if (j == k) {
            V[j, k] <- V[j, k] + factor_i * n_jk[i, j] * (n_i[i] - n_jk[i, j])
          } else {
            V[j, k] <- V[j, k] - factor_i * n_jk[i, j] * n_jk[i, k]
          }
        }
      }
    }

    # Trend: Q_trend = sum(scores * U), Var_trend = scores' V scores
    Q_trend <- sum(scores * U)
    Var_trend <- as.numeric(t(scores) %*% V %*% scores)
    Z <- Q_trend / sqrt(Var_trend)
    pvalue <- 2 * stats::pnorm(abs(Z), lower.tail = FALSE)
    test_name <- paste0(test_name, ", tft")
  } else if (ngroups == 2) {
    # 2-group test: use Z-statistic (normal distribution)
    # U = sum w * (d_1 - e_1), Var = sum w^2 * n1*n2*d*(n-d) / (n^2*(n-1))
    e_1 <- n_jk[, 1] * d_i / n_i
    Q <- sum(w * (d_jk[, 1] - e_1))

    Var_Q <- sum(w^2 * n_jk[, 1] * n_jk[, 2] * d_i * (n_i - d_i) /
                   (n_i^2 * (n_i - 1)), na.rm = TRUE)
    Z <- Q / sqrt(Var_Q)
    pvalue <- 2 * stats::pnorm(abs(Z), lower.tail = FALSE)
  } else {
    # 3+ groups: chi-squared test
    # U_j = sum w * (d_j - e_j) for each group j (drop last group)
    U <- numeric(ngroups)
    for (j in seq_len(ngroups)) {
      e_ij <- n_jk[, j] * d_i / n_i
      U[j] <- sum(w * (d_jk[, j] - e_ij))
    }

    # Covariance matrix V[j, k] for j, k = 1..ngroups
    V <- matrix(0, ngroups, ngroups)
    for (i in seq_len(n_times)) {
      if (n_i[i] <= 1) next
      factor_i <- w[i]^2 * d_i[i] * (n_i[i] - d_i[i]) / (n_i[i]^2 * (n_i[i] - 1))
      for (j in seq_len(ngroups)) {
        for (k in seq_len(ngroups)) {
          if (j == k) {
            V[j, k] <- V[j, k] + factor_i * n_jk[i, j] * (n_i[i] - n_jk[i, j])
          } else {
            V[j, k] <- V[j, k] - factor_i * n_jk[i, j] * n_jk[i, k]
          }
        }
      }
    }

    # Drop last group (redundant) and compute chi-squared
    U_red <- U[-ngroups]
    V_red <- V[-ngroups, -ngroups, drop = FALSE]
    chiSq <- as.numeric(t(U_red) %*% solve(V_red) %*% U_red)
    df <- ngroups - 1
    pvalue <- stats::pchisq(chiSq, df = df, lower.tail = FALSE)
  }

  list(pvalue = pvalue, method_name = test_name)
}
