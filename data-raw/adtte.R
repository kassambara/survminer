# Generates data/adtte.rda: a synthetic CDISC ADaM ADTTE (time-to-event) dataset
# for demonstrating surv_adtte(). Deliberately simulated from scratch (no external
# source, no attribution) with a STRONG treatment effect so the Kaplan-Meier
# curves separate clearly. Re-run with: source("data-raw/adtte.R").
#
# CNSR follows the CDISC convention: 0 = event, >= 1 = censored (1 = alive/ongoing
# at the data cutoff [administrative], 2 = lost to follow-up).

set.seed(2024)
n  <- 260L
id <- sprintf("01-701-%04d", seq_len(n))
trt <- factor(rep(c("Placebo", "Active Treatment"), each = n / 2L),
              levels = c("Placebo", "Active Treatment"))

cutoff  <- 48                       # administrative censoring at 48 months
med2rate <- function(m) log(2) / m  # exponential rate for a given median

sim_param <- function(paramcd, param, med_placebo, med_active) {
  rate <- ifelse(trt == "Active Treatment",
                 med2rate(med_active), med2rate(med_placebo))
  etime   <- rexp(n, rate)                    # true event time
  dropout <- rexp(n, med2rate(90))            # lost-to-follow-up time
  obs     <- pmin(etime, dropout, cutoff)
  cnsr    <- ifelse(etime <= pmin(dropout, cutoff), 0L,
             ifelse(cutoff <= dropout, 1L, 2L))
  evd <- c("0" = if (paramcd == "OS") "Death" else "Disease Progression",
           "1" = "Administratively censored (alive at data cutoff)",
           "2" = "Lost to follow-up")[as.character(cnsr)]
  data.frame(STUDYID = "SURVMINER-01", USUBJID = id, TRT01P = trt,
             PARAMCD = paramcd, PARAM = param,
             AVAL = round(obs, 1), AVALU = "MONTHS",
             CNSR = cnsr, EVNTDESC = unname(evd),
             stringsAsFactors = FALSE)
}

adtte <- rbind(
  sim_param("OS",  "Overall Survival (months)",          med_placebo = 12, med_active = 34),
  sim_param("PFS", "Progression-Free Survival (months)", med_placebo = 6,  med_active = 19)
)
rownames(adtte) <- NULL

# sanity: strong, significant separation
for (pc in c("OS", "PFS")) {
  d <- adtte[adtte$PARAMCD == pc, ]
  sd <- survival::survdiff(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = d)
  p <- 1 - pchisq(sd$chisq, length(sd$n) - 1)
  cat(pc, ": log-rank p =", format.pval(p, eps = 1e-6),
      " events =", sum(d$CNSR == 0), "/", nrow(d), "\n")
}

usethis_available <- requireNamespace("usethis", quietly = TRUE)
if (usethis_available) usethis::use_data(adtte, overwrite = TRUE) else
  save(adtte, file = "data/adtte.rda", compress = "bzip2")
