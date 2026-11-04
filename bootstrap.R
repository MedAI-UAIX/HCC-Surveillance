
library(readxl)

input_file <- "data.xlsx"
patient_col <- "patientID"
gold_col <- "diagnosis"

score_cols <- c("score1", "score2", "score3")
f1_cols <- c("model1", "model2", "model3")

B <- 5000
SEED <- 20261008
set.seed(SEED)

higher_positive <- setNames(rep(TRUE, length(score_cols)), score_cols)

df <- as.data.frame(read_excel(input_file))
names(df) <- trimws(gsub("\u00A0", " ", names(df)))

required <- unique(c(patient_col, gold_col, score_cols, f1_cols))
stopifnot(all(required %in% names(df)))

df$patient_id <- as.character(df[[patient_col]])
df$gold <- suppressWarnings(as.numeric(as.character(df[[gold_col]])))

if (any(!is.na(df$gold) & !df$gold %in% c(0, 1))) {
  stop("Gold standard must be binary (0/1).")
}

for (m in unique(c(score_cols, f1_cols))) {
  df[[m]] <- suppressWarnings(as.numeric(as.character(df[[m]])))
}

df <- df[!is.na(df$patient_id) &
           nzchar(df$patient_id) &
           !is.na(df$gold), ]

for (m in f1_cols) {
  if (any(!is.na(df[[m]]) & !df[[m]] %in% c(0, 1))) {
    stop(paste("Non-binary predictions:", m))
  }
}

calculate_auc <- function(y, score, higher = TRUE) {
  valid <- !is.na(y) & is.finite(score)
  y <- y[valid]
  score <- score[valid]

  n1 <- sum(y == 1)
  n0 <- sum(y == 0)

  if (n1 == 0 || n0 == 0) return(NA_real_)
  if (!higher) score <- -score

  (sum(rank(score, ties.method = "average")[y == 1]) -
     n1 * (n1 + 1) / 2) / (n1 * n0)
}

calculate_f1 <- function(y, pred) {
  tp <- sum(y == 1 & pred == 1)
  fp <- sum(y == 0 & pred == 1)
  fn <- sum(y == 1 & pred == 0)

  den <- 2 * tp + fp + fn
  if (den == 0) return(NA_real_)

  2 * tp / den
}

cluster_indices <- function(id, B, max_attempts = B * 50) {
  rows <- split(seq_along(id), id)
  ids <- names(rows)
  n <- length(ids)

  replicate(max_attempts, {
    selected <- sample(ids, n, replace = TRUE)
    unlist(rows[selected], use.names = FALSE)
  }, simplify = FALSE)
}

bootstrap_metric <- function(data, methods, metric, B = 5000) {
  valid <- complete.cases(data[, c("gold", methods), drop = FALSE])

  for (m in methods) {
    valid <- valid & is.finite(data[[m]])
  }

  d <- data[valid, , drop = FALSE]

  if (nrow(d) == 0) stop("No valid observations.")

  if (metric == "AUC" && length(unique(d$gold)) < 2) {
    stop("AUC requires both positive and negative observations.")
  }

  estimate <- function(x, m) {
    if (metric == "AUC") {
      calculate_auc(x$gold, x[[m]], higher_positive[[m]])
    } else {
      calculate_f1(x$gold, x[[m]])
    }
  }

  original <- vapply(methods, function(m) {
    estimate(d, m)
  }, numeric(1))

  if (anyNA(original)) {
    stop("Metric is undefined for the original data.")
  }

  boot <- matrix(NA_real_, nrow = B, ncol = length(methods))
  colnames(boot) <- methods

  rows <- split(seq_len(nrow(d)), d$patient_id)
  ids <- names(rows)

  valid_b <- 0
  attempts <- 0

  while (valid_b < B && attempts < B * 50) {
    attempts <- attempts + 1

    selected <- sample(ids, length(ids), replace = TRUE)
    ix <- unlist(rows[selected], use.names = FALSE)
    x <- d[ix, , drop = FALSE]

    values <- vapply(methods, function(m) {
      estimate(x, m)
    }, numeric(1))

    if (all(is.finite(values))) {
      valid_b <- valid_b + 1
      boot[valid_b, ] <- values
    }
  }

  if (valid_b < 2) stop("Insufficient valid bootstrap samples.")
  if (valid_b < B) warning("Fewer than B valid bootstrap samples.")

  boot <- boot[seq_len(valid_b), , drop = FALSE]

  list(original = original, boot = boot,
       n_patient = length(ids), n_visit = nrow(d))
}

single_analysis <- function(data, methods, metric) {
  do.call(rbind, lapply(methods, function(m) {
    res <- bootstrap_metric(data, m, metric, B)
    x <- res$boot[, 1]
    ci <- quantile(x, c(0.025, 0.975), names = FALSE)

    data.frame(
      Method = m,
      Estimate = unname(res$original[1]),
      Bootstrap_mean = mean(x),
      SE = sd(x),
      CI_lower = ci[1],
      CI_upper = ci[2],
      N_patient = res$n_patient,
      N_visit = res$n_visit,
      N_boot = length(x)
    )
  }))
}

pairwise_analysis <- function(data, methods, metric) {
  pairs <- combn(methods, 2, simplify = FALSE)

  results <- do.call(rbind, lapply(pairs, function(p) {
    res <- bootstrap_metric(data, p, metric, B)

    delta <- unname(res$original[1] - res$original[2])
    diff_boot <- res$boot[, 1] - res$boot[, 2]

    boot_mean <- mean(diff_boot)
    se <- sd(diff_boot)
    ci <- quantile(diff_boot, c(0.025, 0.975), names = FALSE)

    test_delta <- if (metric == "AUC") boot_mean else delta

    if (is.finite(se) && se > 0) {
      z <- test_delta / se
      p_value <- 2 * pnorm(-abs(z))
    } else if (metric == "F1" && se == 0 && delta == 0) {
      p_value <- 1
    } else {
      p_value <- NA_real_
    }

    data.frame(
      Method1 = p[1],
      Method2 = p[2],
      Estimate1 = unname(res$original[1]),
      Estimate2 = unname(res$original[2]),
      Delta = delta,
      Bootstrap_delta = boot_mean,
      SE = se,
      CI_lower = ci[1],
      CI_upper = ci[2],
      P_value = p_value,
      N_patient = res$n_patient,
      N_visit = res$n_visit,
      N_boot = nrow(res$boot)
    )
  }))

  results$P_Holm <- p.adjust(results$P_value, method = "holm")
  results
}

auc_results <- single_analysis(df, score_cols, "AUC")
auc_pairwise <- pairwise_analysis(df, score_cols, "AUC")

f1_results <- single_analysis(df, f1_cols, "F1")
f1_pairwise <- pairwise_analysis(df, f1_cols, "F1")

print(auc_results)
print(auc_pairwise)
print(f1_results)
print(f1_pairwise)
