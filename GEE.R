
library(readxl)
library(geepack)

input_file <- "data.xlsx"
patient_col <- "patientID"
gold_col <- "diagnosis"
result_cols <- c("GPT4", "GPT 4o", "GPTS", "...")

df <- as.data.frame(read_excel(input_file))
names(df) <- trimws(gsub("\u00A0", " ", names(df)))

required <- c(patient_col, gold_col, result_cols)
stopifnot(all(required %in% names(df)))

to_binary <- function(x) {
  y <- suppressWarnings(as.numeric(as.character(x)))
  if (any(!is.na(y) & !y %in% c(0, 1))) {
    stop("Values must be 0 or 1.")
  }
  y
}

df$patient_id <- as.character(df[[patient_col]])
df$gold <- to_binary(df[[gold_col]])
df$visit_id <- seq_len(nrow(df))

for (m in result_cols) {
  df[[m]] <- to_binary(df[[m]])
}

df <- df[!is.na(df$patient_id) &
           nzchar(df$patient_id) &
           !is.na(df$gold), ]

gee_ci <- function(data, method, metric) {
  d <- data[!is.na(data[[method]]) &
              data$gold == ifelse(metric == "SEN", 1, 0), ]

  if (nrow(d) == 0) {
    return(c(estimate = NA, lower = NA, upper = NA))
  }

  d$outcome <- if (metric == "SEN") {
    d[[method]]
  } else {
    1 - d[[method]]
  }

  est <- mean(d$outcome)

  if (est %in% c(0, 1)) {
    return(c(estimate = est, lower = NA, upper = NA))
  }

  d <- d[order(d$patient_id), ]

  fit <- tryCatch(
    geeglm(
      outcome ~ 1,
      id = patient_id,
      data = d,
      family = binomial("logit"),
      corstr = "independence",
      std.err = "san.se"
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(c(estimate = est, lower = NA, upper = NA))
  }

  beta <- coef(fit)[1]
  se <- sqrt(vcov(fit)[1, 1])

  c(
    estimate = est,
    lower = plogis(beta - 1.96 * se),
    upper = plogis(beta + 1.96 * se)
  )
}

gee_compare <- function(data, m1, m2, metric) {
  d <- data[
    !is.na(data[[m1]]) &
    !is.na(data[[m2]]) &
    data$gold == ifelse(metric == "SEN", 1, 0),
  ]

  if (nrow(d) == 0) return(NA_real_)

  y1 <- if (metric == "SEN") d[[m1]] else 1 - d[[m1]]
  y2 <- if (metric == "SEN") d[[m2]] else 1 - d[[m2]]

  if (length(unique(c(y1, y2))) < 2) {
    return(NA_real_)
  }

  long <- data.frame(
    patient_id = rep(d$patient_id, 2),
    visit_id = rep(d$visit_id, 2),
    group = rep(c(0, 1), each = nrow(d)),
    outcome = c(y1, y2)
  )

  long <- long[order(long$patient_id,
                     long$visit_id,
                     long$group), ]

  fit <- tryCatch(
    geeglm(
      outcome ~ group,
      id = patient_id,
      data = long,
      family = binomial("logit"),
      corstr = "independence",
      std.err = "san.se"
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) return(NA_real_)

  coefs <- summary(fit)$coefficients
  if (!"group" %in% rownames(coefs)) return(NA_real_)

  p_col <- grep("^Pr", colnames(coefs))
  if (length(p_col) == 0) return(NA_real_)

  as.numeric(coefs["group", p_col[1]])
}

performance <- do.call(rbind, lapply(result_cols, function(m) {
  sen <- gee_ci(df, m, "SEN")
  spe <- gee_ci(df, m, "SPE")

  data.frame(
    Method = m,
    SEN = sen["estimate"],
    SEN_lower = sen["lower"],
    SEN_upper = sen["upper"],
    SPE = spe["estimate"],
    SPE_lower = spe["lower"],
    SPE_upper = spe["upper"],
    row.names = NULL
  )
}))

pairs <- combn(result_cols, 2, simplify = FALSE)

pairwise <- do.call(rbind, lapply(pairs, function(p) {
  data.frame(
    Method1 = p[1],
    Method2 = p[2],
    SEN_p = gee_compare(df, p[1], p[2], "SEN"),
    SPE_p = gee_compare(df, p[1], p[2], "SPE")
  )
}))

pairwise$SEN_Holm <- p.adjust(pairwise$SEN_p, "holm")
pairwise$SEN_Bonferroni <- p.adjust(pairwise$SEN_p, "bonferroni")
pairwise$SPE_Holm <- p.adjust(pairwise$SPE_p, "holm")
pairwise$SPE_Bonferroni <- p.adjust(pairwise$SPE_p, "bonferroni")

print(performance)
print(pairwise)
