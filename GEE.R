# ============================================================
# 6种方法：
# 1. 每种方法计算 Sensitivity / Specificity + GEE 95% CI
# 2. 6种方法之间两两比较 SEN / SPE 的 GEE P值
#
# 数据结构：
# 每一行 = 1次 visit
# 同一患者可有多次 visit
#
# GEE设置：
# cluster = patientID
# working correlation = independence
# robust sandwich standard error
# ============================================================


# ============================================================
# 0. 加载包
# ============================================================

# install.packages(c("readxl", "dplyr", "geepack", "writexl"))

library(readxl)
library(dplyr)
library(geepack)
library(writexl)


# ============================================================
# 1. 用户需要修改的地方
# ============================================================

# 输入文件
input_file <- "C:\\Users\\10096\\Nutstore\\1\\HCC风险预测\\HCC风险预测GPT课题\\05 投稿\\diagnostics\\返修\\统计结果\\LXX\\最终统计+LLM.xlsx"

# 输出文件
output_file <- "C:\\Users\\10096\\Nutstore\\1\\HCC风险预测\\HCC风险预测GPT课题\\05 投稿\\diagnostics\\返修\\统计结果\\LXX\\sup-table3-GEE-new.xlsx"

# 患者ID列
patient_col <- "patientID"


# 金标准列
# 0 = 阴性
# 1 = 阳性
gold_col <- "diagnosis"


# 六个需要比较的结果列
# 每列必须：
# 0 = 阴性
# 1 = 阳性
result_cols <- c(
  "GPT4",
  "GPT 4o",
  # #table2
  "GPTS",
  # "思维链第一种prompt（1发生肝癌,0不发生肝癌）",
  # "第二次思维链重测",
  # "上下文+思维链第二种prompt",
  # "PAGE-B Score",
  # "mPAGE-B Score",
  # "aMAP score"
    # table3
  # "单模态预测",
  # "双模态预测结果",
  # "多模态预测结果",
  # "PAGE-B Score",
  # "mPAGE-B Score",
  # "aMAP score"
    "qwen3",
  "medgemma",
  "huatuo"
)

  # "单模态预测",
  # "双模态预测结果",
  # "多模态预测结果",
  # "PAGE-B Score",
  # "mPAGE-B Score",
  # "aMAP score"

# ============================================================
# 2. 读取数据
# ============================================================

df <- read_excel(input_file)

# 清理列名中的不可见空格
colnames(df) <- gsub("\u00A0", " ", colnames(df))
colnames(df) <- trimws(colnames(df))


# ============================================================
# 3. 检查列名
# ============================================================

needed_cols <- c(
  patient_col,
  gold_col,
  result_cols
)

missing_cols <- needed_cols[
  !needed_cols %in% colnames(df)
]

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Excel缺少以下列：\n",
      paste(missing_cols, collapse = "\n")
    )
  )
}


# ============================================================
# 4. 0/1转换函数
# ============================================================

to_binary <- function(x, variable_name = "") {

  y <- suppressWarnings(
    as.numeric(as.character(x))
  )

  invalid <- unique(
    y[
      !is.na(y) &
        !(y %in% c(0, 1))
    ]
  )

  if (length(invalid) > 0) {

    stop(
      paste0(
        "变量 ",
        variable_name,
        " 中存在非0/1值：",
        paste(invalid, collapse = ", ")
      )
    )
  }

  return(y)
}


# ============================================================
# 5. 整理数据
# ============================================================

analysis_df <- df

analysis_df$.patient_id <- as.character(
  analysis_df[[patient_col]]
)

analysis_df$.gold <- to_binary(
  analysis_df[[gold_col]],
  gold_col
)

# 每一行给一个visit编号
analysis_df$.visit_id <- seq_len(
  nrow(analysis_df)
)


# 六个预测结果全部转为0/1
for (col in result_cols) {

  analysis_df[[col]] <- to_binary(
    analysis_df[[col]],
    col
  )
}


# 删除 patientID 或 gold 缺失
analysis_df <- analysis_df %>%
  filter(
    !is.na(.patient_id),
    .patient_id != "",
    !is.na(.gold)
  )


# ============================================================
# 6. 工具函数
# ============================================================

inv_logit <- function(x) {

  1 / (1 + exp(-x))

}


format_ci <- function(low, high) {

  if (
    is.na(low) ||
    is.na(high)
  ) {
    return(NA_character_)
  }

  paste0(
    sprintf("%.1f", low * 100),
    "%–",
    sprintf("%.1f", high * 100),
    "%"
  )
}


format_p <- function(p) {

  if (is.na(p)) {
    return(NA_character_)
  }

  if (p < 0.001) {
    return("<0.001")
  }

  sprintf("%.3f", p)
}


# ============================================================
# 7. 单个方法：
# GEE计算 Sensitivity / Specificity + 95%CI
# ============================================================

gee_metric_ci <- function(
    data,
    pred_col,
    metric = c("sensitivity", "specificity")
) {

  metric <- match.arg(metric)


  tmp <- data %>%
    transmute(
      patient_id = .patient_id,
      gold = .gold,
      pred = .data[[pred_col]]
    ) %>%
    filter(
      !is.na(patient_id),
      !is.na(gold),
      !is.na(pred)
    )


  # ----------------------------------------------------------
  # Sensitivity
  #
  # 只看金标准阳性病例
  #
  # outcome:
  # 1 = TP
  # 0 = FN
  # ----------------------------------------------------------

  if (metric == "sensitivity") {

    tmp <- tmp %>%
      filter(
        gold == 1
      ) %>%
      mutate(
        outcome = pred
      )

  }


  # ----------------------------------------------------------
  # Specificity
  #
  # 只看金标准阴性病例
  #
  # outcome:
  # 1 = TN
  # 0 = FP
  # ----------------------------------------------------------

  if (metric == "specificity") {

    tmp <- tmp %>%
      filter(
        gold == 0
      ) %>%
      mutate(
        outcome = 1 - pred
      )

  }


  tmp <- tmp %>%
    mutate(
      patient_id = as.factor(patient_id),
      outcome = as.numeric(outcome)
    ) %>%
    arrange(patient_id)


  n_visit <- nrow(tmp)

  n_patient <- n_distinct(
    tmp$patient_id
  )


  # 没有数据
  if (n_visit == 0) {

    return(
      list(
        estimate = NA,
        ci_low = NA,
        ci_high = NA,
        numerator = NA,
        denominator = 0,
        n_patient = 0
      )
    )
  }


  numerator <- sum(
    tmp$outcome == 1
  )

  denominator <- n_visit

  estimate <- numerator / denominator


  message(
    "CI: ",
    pred_col,
    " | ",
    metric,
    " | visits=",
    denominator,
    " | patients=",
    n_patient,
    " | success=",
    numerator
  )


  # ----------------------------------------------------------
  # 如果全部为0或全部为1
  # Logistic GEE无法正常估计Wald CI
  # ----------------------------------------------------------

  if (
    numerator == 0 ||
    numerator == denominator
  ) {

    warning(
      paste0(
        pred_col,
        " - ",
        metric,
        " = ",
        round(estimate * 100, 2),
        "%，由于所有结果完全一致，",
        "普通logistic GEE无法可靠计算Wald 95%CI。"
      )
    )

    return(
      list(
        estimate = estimate,
        ci_low = NA,
        ci_high = NA,
        numerator = numerator,
        denominator = denominator,
        n_patient = n_patient
      )
    )
  }


  # ----------------------------------------------------------
  # Intercept-only GEE
  # ----------------------------------------------------------

  fit <- tryCatch({

    geeglm(
      outcome ~ 1,
      id = patient_id,
      data = tmp,
      family = binomial(link = "logit"),
      corstr = "independence",
      std.err = "san.se"
    )

  }, error = function(e) {

    message(
      "GEE CI failed: ",
      pred_col,
      " | ",
      metric,
      " | ",
      e$message
    )

    return(NULL)

  })


  if (is.null(fit)) {

    return(
      list(
        estimate = estimate,
        ci_low = NA,
        ci_high = NA,
        numerator = numerator,
        denominator = denominator,
        n_patient = n_patient
      )
    )
  }


  coef_table <- summary(fit)$coefficients


  beta <- coef_table[
    "(Intercept)",
    "Estimate"
  ]


  se <- coef_table[
    "(Intercept)",
    "Std.err"
  ]


  lower_logit <- beta - 1.96 * se

  upper_logit <- beta + 1.96 * se


  ci_low <- inv_logit(
    lower_logit
  )

  ci_high <- inv_logit(
    upper_logit
  )


  return(
    list(
      estimate = estimate,
      ci_low = ci_low,
      ci_high = ci_high,
      numerator = numerator,
      denominator = denominator,
      n_patient = n_patient
    )
  )
}


# ============================================================
# 8. 两个方法之间进行GEE比较
#
# Sensitivity：
# 只分析 gold = 1
#
# Specificity：
# 只分析 gold = 0
#
# 只保留两个方法都有结果的visit
#
# patientID作为cluster
# ============================================================

gee_compare_pair <- function(
    data,
    method1,
    method2,
    metric = c("sensitivity", "specificity")
) {

  metric <- match.arg(metric)


  # ----------------------------------------------------------
  # 两个方法都存在结果的visit
  # ----------------------------------------------------------

  tmp <- data %>%
    transmute(
      patient_id = .patient_id,
      visit_id = .visit_id,
      gold = .gold,
      pred1 = .data[[method1]],
      pred2 = .data[[method2]]
    ) %>%
    filter(
      !is.na(patient_id),
      !is.na(gold),
      !is.na(pred1),
      !is.na(pred2)
    )


  # ----------------------------------------------------------
  # Sensitivity只分析gold=1
  # Specificity只分析gold=0
  # ----------------------------------------------------------

  if (metric == "sensitivity") {

    tmp <- tmp %>%
      filter(
        gold == 1
      )

  } else {

    tmp <- tmp %>%
      filter(
        gold == 0
      )

  }


  n_visit <- nrow(tmp)

  n_patient <- n_distinct(
    tmp$patient_id
  )


  if (n_visit == 0) {

    return(
      list(
        method1_estimate = NA,
        method2_estimate = NA,
        p_value = NA,
        n_visit = 0,
        n_patient = 0
      )
    )
  }


  # ----------------------------------------------------------
  # 将预测转换为：
  #
  # sensitivity:
  # TP = 1
  # FN = 0
  #
  # specificity:
  # TN = 1
  # FP = 0
  # ----------------------------------------------------------

  if (metric == "sensitivity") {

    outcome1 <- tmp$pred1

    outcome2 <- tmp$pred2

  } else {

    outcome1 <- 1 - tmp$pred1

    outcome2 <- 1 - tmp$pred2

  }


  method1_estimate <- mean(
    outcome1
  )

  method2_estimate <- mean(
    outcome2
  )


  # ----------------------------------------------------------
  # 转成长格式
  #
  # group:
  # 0 = method1
  # 1 = method2
  # ----------------------------------------------------------

  long_method1 <- data.frame(
    patient_id = tmp$patient_id,
    visit_id = tmp$visit_id,
    group = 0,
    outcome = outcome1
  )


  long_method2 <- data.frame(
    patient_id = tmp$patient_id,
    visit_id = tmp$visit_id,
    group = 1,
    outcome = outcome2
  )


  long_df <- bind_rows(
    long_method1,
    long_method2
  ) %>%
    mutate(
      patient_id = as.factor(patient_id),
      group = as.numeric(group),
      outcome = as.numeric(outcome)
    ) %>%
    arrange(
      patient_id,
      visit_id,
      group
    )


  # ----------------------------------------------------------
  # 没有任何outcome变异
  # ----------------------------------------------------------

  if (
    length(unique(long_df$outcome)) < 2
  ) {

    warning(
      paste0(
        method1,
        " vs ",
        method2,
        " | ",
        metric,
        "：所有outcome完全相同，无法进行GEE比较。"
      )
    )

    return(
      list(
        method1_estimate = method1_estimate,
        method2_estimate = method2_estimate,
        p_value = NA,
        n_visit = n_visit,
        n_patient = n_patient
      )
    )
  }


  message(
    "Comparison: ",
    method1,
    " vs ",
    method2,
    " | ",
    metric,
    " | visits=",
    n_visit,
    " | patients=",
    n_patient
  )


  # ----------------------------------------------------------
  # GEE Logistic Regression
  #
  # outcome ~ group
  #
  # 检验group系数即可比较两个方法
  # ----------------------------------------------------------

  fit <- tryCatch({

    geeglm(
      outcome ~ group,
      id = patient_id,
      data = long_df,
      family = binomial(link = "logit"),
      corstr = "independence",
      std.err = "san.se"
    )

  }, error = function(e) {

    message(
      "GEE comparison failed: ",
      method1,
      " vs ",
      method2,
      " | ",
      metric,
      " | ",
      e$message
    )

    return(NULL)

  })


  if (is.null(fit)) {

    return(
      list(
        method1_estimate = method1_estimate,
        method2_estimate = method2_estimate,
        p_value = NA,
        n_visit = n_visit,
        n_patient = n_patient
      )
    )
  }


  coef_table <- summary(fit)$coefficients


  if (
    !"group" %in% rownames(coef_table)
  ) {

    return(
      list(
        method1_estimate = method1_estimate,
        method2_estimate = method2_estimate,
        p_value = NA,
        n_visit = n_visit,
        n_patient = n_patient
      )
    )
  }


  p_col <- grep(
    "^Pr",
    colnames(coef_table),
    value = TRUE
  )


  if (length(p_col) == 0) {

    return(
      list(
        method1_estimate = method1_estimate,
        method2_estimate = method2_estimate,
        p_value = NA,
        n_visit = n_visit,
        n_patient = n_patient
      )
    )
  }


  p_value <- as.numeric(
    coef_table[
      "group",
      p_col[1]
    ]
  )


  return(
    list(
      method1_estimate = method1_estimate,
      method2_estimate = method2_estimate,
      p_value = p_value,
      n_visit = n_visit,
      n_patient = n_patient
    )
  )
}


# ============================================================
# 9. 每一种方法计算 SEN / SPE + GEE 95% CI
# ============================================================

performance_results <- list()


for (method in result_cols) {

  message(
    "================================================"
  )

  message(
    "正在计算：",
    method
  )

  message(
    "================================================"
  )


  # Sensitivity
  sen <- gee_metric_ci(
    data = analysis_df,
    pred_col = method,
    metric = "sensitivity"
  )


  # Specificity
  spe <- gee_metric_ci(
    data = analysis_df,
    pred_col = method,
    metric = "specificity"
  )


  performance_results[[length(performance_results) + 1]] <- data.frame(

    方法 = method,


    # ========================================================
    # Sensitivity
    # ========================================================

    SEN_分子 = sen$numerator,

    SEN_分母 = sen$denominator,

    SEN_患者数 = sen$n_patient,

    SEN = sen$estimate,

    SEN_percent = ifelse(
      is.na(sen$estimate),
      NA,
      round(
        sen$estimate * 100,
        2
      )
    ),

    SEN_CI_lower = sen$ci_low,

    SEN_CI_upper = sen$ci_high,

    SEN_95CI = format_ci(
      sen$ci_low,
      sen$ci_high
    ),


    # ========================================================
    # Specificity
    # ========================================================

    SPE_分子 = spe$numerator,

    SPE_分母 = spe$denominator,

    SPE_患者数 = spe$n_patient,

    SPE = spe$estimate,

    SPE_percent = ifelse(
      is.na(spe$estimate),
      NA,
      round(
        spe$estimate * 100,
        2
      )
    ),

    SPE_CI_lower = spe$ci_low,

    SPE_CI_upper = spe$ci_high,

    SPE_95CI = format_ci(
      spe$ci_low,
      spe$ci_high
    )

  )
}


performance_df <- bind_rows(
  performance_results
)


# ============================================================
# 10. 生成六种方法的所有两两组合
#
# 6种方法：
# C(6,2) = 15组
# ============================================================

method_pairs <- combn(
  result_cols,
  2,
  simplify = FALSE
)


# ============================================================
# 11. 15组两两比较
# ============================================================

pairwise_results <- list()


for (pair in method_pairs) {

  method1 <- pair[1]

  method2 <- pair[2]


  message(
    "================================================"
  )

  message(
    "比较：",
    method1,
    " vs ",
    method2
  )

  message(
    "================================================"
  )


  # ----------------------------------------------------------
  # Sensitivity
  # ----------------------------------------------------------

  sen_cmp <- gee_compare_pair(
    data = analysis_df,
    method1 = method1,
    method2 = method2,
    metric = "sensitivity"
  )


  # ----------------------------------------------------------
  # Specificity
  # ----------------------------------------------------------

  spe_cmp <- gee_compare_pair(
    data = analysis_df,
    method1 = method1,
    method2 = method2,
    metric = "specificity"
  )


  pairwise_results[[length(pairwise_results) + 1]] <- data.frame(

    方法1 = method1,

    方法2 = method2,


    # ========================================================
    # Sensitivity comparison
    # ========================================================

    SEN_比较患者数 = sen_cmp$n_patient,

    SEN_比较visit数 = sen_cmp$n_visit,

    方法1_SEN = sen_cmp$method1_estimate,

    方法1_SEN_percent = ifelse(
      is.na(sen_cmp$method1_estimate),
      NA,
      round(
        sen_cmp$method1_estimate * 100,
        2
      )
    ),

    方法2_SEN = sen_cmp$method2_estimate,

    方法2_SEN_percent = ifelse(
      is.na(sen_cmp$method2_estimate),
      NA,
      round(
        sen_cmp$method2_estimate * 100,
        2
      )
    ),

    SEN_P_value = sen_cmp$p_value,

    SEN_P_value_format = format_p(
      sen_cmp$p_value
    ),


    # ========================================================
    # Specificity comparison
    # ========================================================

    SPE_比较患者数 = spe_cmp$n_patient,

    SPE_比较visit数 = spe_cmp$n_visit,

    方法1_SPE = spe_cmp$method1_estimate,

    方法1_SPE_percent = ifelse(
      is.na(spe_cmp$method1_estimate),
      NA,
      round(
        spe_cmp$method1_estimate * 100,
        2
      )
    ),

    方法2_SPE = spe_cmp$method2_estimate,

    方法2_SPE_percent = ifelse(
      is.na(spe_cmp$method2_estimate),
      NA,
      round(
        spe_cmp$method2_estimate * 100,
        2
      )
    ),

    SPE_P_value = spe_cmp$p_value,

    SPE_P_value_format = format_p(
      spe_cmp$p_value
    )

  )
}


pairwise_df <- bind_rows(
  pairwise_results
)


# ============================================================
# 12. 多重比较校正
#
# 六种方法共15次SEN比较
# 六种方法共15次SPE比较
#
# 同时提供：
# 原始P
# Holm校正P
# Bonferroni校正P
# ============================================================

pairwise_df <- pairwise_df %>%
  mutate(

    # Sensitivity
    SEN_P_Holm = p.adjust(
      SEN_P_value,
      method = "holm"
    ),

    SEN_P_Holm_format = sapply(
      SEN_P_Holm,
      format_p
    ),

    SEN_P_Bonferroni = p.adjust(
      SEN_P_value,
      method = "bonferroni"
    ),

    SEN_P_Bonferroni_format = sapply(
      SEN_P_Bonferroni,
      format_p
    ),


    # Specificity
    SPE_P_Holm = p.adjust(
      SPE_P_value,
      method = "holm"
    ),

    SPE_P_Holm_format = sapply(
      SPE_P_Holm,
      format_p
    ),

    SPE_P_Bonferroni = p.adjust(
      SPE_P_value,
      method = "bonferroni"
    ),

    SPE_P_Bonferroni_format = sapply(
      SPE_P_Bonferroni,
      format_p
    )

  )


# ============================================================
# 13. 制作Sensitivity P值矩阵
# ============================================================

sen_matrix <- matrix(
  NA,
  nrow = length(result_cols),
  ncol = length(result_cols),
  dimnames = list(
    result_cols,
    result_cols
  )
)


diag(sen_matrix) <- NA


for (i in seq_len(nrow(pairwise_df))) {

  m1 <- pairwise_df$方法1[i]

  m2 <- pairwise_df$方法2[i]

  p <- pairwise_df$SEN_P_value[i]

  sen_matrix[m1, m2] <- p

  sen_matrix[m2, m1] <- p
}


sen_matrix_df <- data.frame(
  方法 = rownames(sen_matrix),
  sen_matrix,
  check.names = FALSE,
  row.names = NULL
)


# ============================================================
# 14. 制作Specificity P值矩阵
# ============================================================

spe_matrix <- matrix(
  NA,
  nrow = length(result_cols),
  ncol = length(result_cols),
  dimnames = list(
    result_cols,
    result_cols
  )
)


diag(spe_matrix) <- NA


for (i in seq_len(nrow(pairwise_df))) {

  m1 <- pairwise_df$方法1[i]

  m2 <- pairwise_df$方法2[i]

  p <- pairwise_df$SPE_P_value[i]

  spe_matrix[m1, m2] <- p

  spe_matrix[m2, m1] <- p
}


spe_matrix_df <- data.frame(
  方法 = rownames(spe_matrix),
  spe_matrix,
  check.names = FALSE,
  row.names = NULL
)


# ============================================================
# 15. 数据概况
# ============================================================

patient_visit_count <- analysis_df %>%
  count(
    .patient_id,
    name = "n_visit"
  )


data_summary_df <- data.frame(

  指标 = c(
    "总患者数",
    "总visit数",
    "金标准阳性visit数",
    "金标准阴性visit数",
    "至少有1次阳性visit的患者数",
    "至少有1次阴性visit的患者数",
    "每位患者visit数中位数",
    "每位患者visit数最小值",
    "每位患者visit数最大值"
  ),

  数值 = c(

    n_distinct(
      analysis_df$.patient_id
    ),

    nrow(
      analysis_df
    ),

    sum(
      analysis_df$.gold == 1,
      na.rm = TRUE
    ),

    sum(
      analysis_df$.gold == 0,
      na.rm = TRUE
    ),

    analysis_df %>%
      filter(
        .gold == 1
      ) %>%
      summarise(
        n = n_distinct(
          .patient_id
        )
      ) %>%
      pull(n),

    analysis_df %>%
      filter(
        .gold == 0
      ) %>%
      summarise(
        n = n_distinct(
          .patient_id
        )
      ) %>%
      pull(n),

    median(
      patient_visit_count$n_visit
    ),

    min(
      patient_visit_count$n_visit
    ),

    max(
      patient_visit_count$n_visit
    )

  )
)


# ============================================================
# 16. 各方法缺失情况
# ============================================================

missing_results <- list()


for (method in result_cols) {

  missing_results[[length(missing_results) + 1]] <- data.frame(

    方法 = method,

    总visit数 = nrow(
      analysis_df
    ),

    有结果visit数 = sum(
      !is.na(
        analysis_df[[method]]
      )
    ),

    缺失visit数 = sum(
      is.na(
        analysis_df[[method]]
      )
    )

  )
}


missing_df <- bind_rows(
  missing_results
)


# ============================================================
# 17. 保存Excel
# ============================================================

write_xlsx(

  list(

    "SEN_SPE及95CI" = performance_df,

    "两两比较P值" = pairwise_df,

    "SEN_P值矩阵" = sen_matrix_df,

    "SPE_P值矩阵" = spe_matrix_df,

    "数据概况" = data_summary_df,

    "缺失情况" = missing_df

  ),

  path = output_file

)


message(
  "================================================"
)

message(
  "✅ 全部计算完成"
)

message(
  "6种方法共完成15组两两比较"
)

message(
  "结果已保存至："
)

message(
  output_file
)

message(
  "================================================"
)