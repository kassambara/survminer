#' Fleming-Harrington Weighted Log-Rank Tests
#'
#' @description Computes Fleming-Harrington \eqn{G(\rho, \gamma)} weighted
#'   log-rank tests for one or more \code{(rho, gamma)} parameter pairs. At each
#'   event time the weight is \eqn{S(t^-)^{\rho}\,(1 - S(t^-))^{\gamma}}, where
#'   \eqn{S} is the pooled Kaplan-Meier estimate. Different weights emphasise
#'   different parts of the follow-up: \code{FH(0, 0)} is the ordinary log-rank
#'   test (equal weight), \code{FH(1, 0)} emphasises \emph{early} differences,
#'   \code{FH(0, 1)} \emph{late} differences and \code{FH(1, 1)} the middle. This
#'   is the base-R engine behind \code{\link{surv_pvalue}(method = "FH", rho =,
#'   gamma =)} and \code{\link{ggsurvplot}()}.
#' @param formula a survival formula, e.g. \code{Surv(time, status) ~ group}.
#' @param data a data frame.
#' @param rho,gamma numeric vectors of Fleming-Harrington parameters. They are
#'   recycled to a common length and one test is run per \code{(rho, gamma)}
#'   pair. Default \code{FH(0, 0)}, the log-rank test.
#' @return a data frame with one row per \code{(rho, gamma)} pair and the columns:
#'   \code{weight} (a \code{"FH(rho, gamma)"} label), \code{rho}, \code{gamma},
#'   \code{statistic} (the chi-square statistic), \code{df} (degrees of freedom)
#'   and \code{p.value}.
#' @seealso \code{\link{surv_pvalue}()}
#' @examples
#' library(survival)
#' # Log-rank, early- and late-emphasis tests in one call
#' weighted_logrank(Surv(time, status) ~ sex, data = lung,
#'                  rho = c(0, 1, 0), gamma = c(0, 0, 1))
#' @export
weighted_logrank <- function(formula, data, rho = 0, gamma = 0) {
  if (!is.numeric(rho) || !is.numeric(gamma))
    stop("`rho` and `gamma` must be numeric.", call. = FALSE)
  if (any(rho < 0) || any(gamma < 0))
    stop("Fleming-Harrington `rho` and `gamma` must be >= 0.", call. = FALSE)
  n <- max(length(rho), length(gamma))
  if (n == 0L) stop("`rho`/`gamma` must have at least one value.", call. = FALSE)
  rho <- rep_len(rho, n); gamma <- rep_len(gamma, n)
  rows <- lapply(seq_len(n), function(i) {
    t <- .weighted_logrank_test(formula, data, method = "FH",
                                rho = rho[i], gamma = gamma[i])
    data.frame(weight = sprintf("FH(%g, %g)", rho[i], gamma[i]),
               rho = rho[i], gamma = gamma[i],
               statistic = t$statistic, df = t$df, p.value = t$pvalue,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


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
# @param method weight method: "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1", or
#   "FH" for an arbitrary Fleming-Harrington G(rho, gamma) weight.
# @param rho,gamma Fleming-Harrington parameters used when method = "FH": the
#   weight is S(t-)^rho * (1 - S(t-))^gamma, where S is the pooled Kaplan-Meier
#   estimate. FH(0,0) is the log-rank test, FH(1,0) early emphasis, FH(0,1) late
#   emphasis, FH(1,1) mid emphasis. Ignored by the fixed-weight methods.
# @param test.for.trend logical; if TRUE, compute 1-df trend test
# @return list with statistic (chi-square), df, pvalue and method_name
.weighted_logrank_test <- function(formula, data, method = "1",
                                   rho = 0, gamma = 0,
                                   test.for.trend = FALSE) {
  # Build model frame
  mf <- stats::model.frame(formula, data)
  surv_obj <- mf[[1]]
  time <- surv_obj[, "time"]
  status <- surv_obj[, "status"]
  # Ensure status is 0/1
  if (max(status) == 2) status <- status - 1
  # drop unused factor levels so a two-group subset (e.g. from pairwise
  # comparisons) is treated as 2 groups, not the parent's full level set --
  # otherwise empty groups make the covariance matrix singular. For a full-data
  # fit there are no empty levels, so this is a no-op.
  group <- droplevels(as.factor(mf[[2]]))
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
    "FH" = {
      # Fleming-Harrington G(rho, gamma): w = S(t-)^rho * (1 - S(t-))^gamma,
      # S(t-) the left-continuous pooled KM estimate. FH(0,0) == log-rank.
      surv_km <- cumprod(1 - d_i / n_i)
      s_left <- c(1, surv_km[-n_times])
      s_left^rho * (1 - s_left)^gamma
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
  test_name <- if (method == "FH")
    sprintf("Fleming-Harrington (rho=%g, gamma=%g)", rho, gamma)
  else test_names[method]

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
    statistic <- Z^2; df <- 1
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
    statistic <- Z^2; df <- 1
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
    statistic <- chiSq
  }

  list(statistic = statistic, df = df, pvalue = pvalue, method_name = test_name)
}


# Resolve a user-supplied log-rank method name/alias to its canonical form.
# Returns "survdiff" (ordinary log-rank via survival::survdiff) or one of the
# weighted-test names accepted by .weighted_logrank_test() ("n", "sqrtN", "S1",
# "S2", "FH_p=1_q=1"). Case-insensitive; accepts the weight code, the full test
# name, or the abbreviation. Shared by surv_pvalue() and pairwise_survdiff() so
# the accepted names stay in sync.
.resolve_logrank_method <- function(method) {
  if (is.null(method)) method <- "survdiff"
  . <- NULL
  allowed.methods <- c("survdiff", "log-rank", "LR", "1",
                       "n", "Gehan-Breslow", "GB",
                       "sqrtN", "Tarone-Ware", "TW",
                       "S1", "Peto-Peto", "PP",
                       "S2", "modified Peto-Peto", "mPP",
                       "FH_p=1_q=1", "Fleming-Harrington(p=1, q=1)", "FH")
  method.names <- c(rep("survdiff", 4),
                    rep(c("n", "sqrtN", "S1", "S2", "FH_p=1_q=1"), each = 3))
  # don't use grep which will detect many positions for "n" or "FH"
  choosed.method <- which(tolower(allowed.methods) %in% tolower(method))
  if (.is_empty(choosed.method))
    stop("Don't support the choosed method: ", method, ". ",
         "Allowed methods include: ", .collapse(allowed.methods, sep = ", "))
  method.names[choosed.method] %>% .[1]
}
