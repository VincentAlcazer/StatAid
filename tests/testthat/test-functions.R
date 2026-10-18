context("Functions test")


data <- read.delim(system.file("extdata", "TCGA_AML_exemple.txt", package = "StatAid"),
                   dec = ",", na.strings = c("", "NA"), stringsAsFactors = T)


test_that("descriptive_table", {
  
  res <- descriptive_table(
    data, group = "Sex"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(res$param_pvalue[1],"0.2031")
  
})

test_that("autoplot", {
  
  res <- autoplot(
    data,variable="Age", group = "Sex"
  )
  
  expect_s3_class(res$graph, c("gg","ggplot"))
  
})

test_that("regression_dataframes_lm", {
  
  res <- regression_dataframes(
    y_var="Age", x_var = "WBC", data, model="lm"
  )
  
  expect_s3_class(res$tidy_df, "data.frame")
  expect_s3_class(res$augment_df, "data.frame")
  expect_equal(res$tidy_df$term[2],"WBC")
  expect_equal(res$augment_df$Age[1],76)
  
})

test_that("regression_dataframes_gam", {
  
  res <- regression_dataframes(
    y_var="Age", x_var = "WBC", data, model="gam"
  )
  
  expect_s3_class(res$tidy_df, "data.frame")
  expect_s3_class(res$augment_df, "data.frame")
  expect_equal(res$tidy_df$term[1],"s(WBC)")
  expect_equal(res$augment_df$Age[1],76)
  
})

test_that("regression_table_lm", {
  
  res <- regression_table(
    data[,2:7], y_var = "Age", family="gaussian"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Sex")
  expect_equal(res$`Beta Coeff.`[1],3.35)
  
})

test_that("regression_table_logit", {
  
  res <- regression_table(
    data[,2:7], y_var = "Sex", family="binomial"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Age")
  expect_equal(res$`Odds Ratio`[1],1.01)
  
})

test_that("regression_table_cox", {
  
  res <- regression_table_cox(
    data, y_var="DFS_STATUS", time_var="DFS_MONTHS"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Sex")
  expect_equal(res$HR[1],0.91)
  
})

test_that("regression_table_multi_lm", {
  
  res <- regression_table_multi(
    data[,2:7], y_var = "Age", family="gaussian"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Sex")
  expect_equal(res$`Beta Coeff.`[1],1.34)
  
})

test_that("regression_table_multi_logit", {
  
  res <- regression_table_multi(
    data[,2:7], y_var = "Sex", family="binomial"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Age")
  expect_equal(res$`Odds Ratio`[1],1.01)
  
})

test_that("regression_table_multi_cox", {
  
  res <- regression_table_multi_cox(
    data[,c(2:5,13:14)], y_var="DFS_STATUS", time_var="DFS_MONTHS"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$`X Variables`[1],"Sex")
  expect_equal(res$HR[1],0.85)
  
})


#################### ==================== Regression tests (bug fixes) ====================  ####################

test_that("odds ratios are exponentiated before rounding", {
  res <- regression_table(data[, c("NPM1_mut", "FLT3_mut")], y_var = "NPM1_mut", family = "binomial")
  fit <- glm(NPM1_mut ~ FLT3_mut, data = data, family = "binomial")

  expect_equal(res$`Odds Ratio`[1], round(exp(unname(coef(fit)[2])), 2))
})

test_that("multivariate terms are labelled with the right variable when names share a prefix", {
  d <- data[, c("Age", "WBC")]
  d$WBC_cat <- factor(ifelse(d$WBC > 20, "high", "low"))

  res <- regression_table_multi(d, y_var = "Age")
  expect_equal(res$`X Variables`, c("WBC", "WBC_cat"))
  expect_equal(res$Comparison, c("Continuous", "low_vs_high"))

  d$time <- data$OS_MONTHS
  d$status <- data$OS_STATUS
  res_cox <- regression_table_multi_cox(d, y_var = "status", time_var = "time")
  expect_equal(res_cox$`X Variables`, c("Age", "WBC", "WBC_cat"))
  expect_equal(res_cox$Comparison[3], "low_vs_high")
})

test_that("regression tables accept tibbles, ordered factors and non-syntactic names", {
  d <- tibble::as_tibble(data[, c("Age", "Sex", "ELN2017")])
  d$ELN2017 <- factor(d$ELN2017, ordered = TRUE)
  names(d)[2] <- "Patient sex"

  res <- regression_table(d, y_var = "Age")
  expect_equal(res$`X Variables`[1], "Patient sex")
  expect_equal(res$Comparison[1], "Male_vs_Female")
})

test_that("univariate tables skip variables that cannot be fitted", {
  d <- data[, c("Age", "Sex")]
  d$constant <- factor("A")
  expect_warning(res <- regression_table(d, y_var = "Age"), "constant")
  expect_equal(res$`X Variables`, "Sex")
})

test_that("autoplot ANOVA p-value uses the filtered groups", {
  res <- autoplot(data, variable = "Age", group = "FAB", group_filter_vector = c("M1", "M2", "M4"), stat = "param")
  d <- droplevels(subset(data, FAB %in% c("M1", "M2", "M4")))
  expected <- format.pval(summary(aov(Age ~ FAB, d))[[1]][["Pr(>F)"]][1], 3, eps = 0.001)

  label <- res$graph$layers[[length(res$graph$layers)]]$aes_params$label
  expect_equal(label, paste0("ANOVA p-value: ", expected))
})

test_that("autoplot draws histograms", {
  res <- autoplot(data, variable = "Age", group = "Sex", plot_type = "Histogram", stat = "no")
  expect_gt(length(res$graph$layers), 0)
})

test_that("regression_dataframes gam handles missing values", {
  d <- data
  d$WBC[1:3] <- NA
  res <- regression_dataframes(y_var = "Age", x_var = "WBC", d, model = "gam")
  expect_equal(nrow(res$augment_df), sum(!is.na(d$WBC)))
})

test_that("descriptive_table requires at least 2 groups", {
  d <- data
  d$single <- factor("A")
  expect_error(descriptive_table(d, group = "single"), "at least 2")
})

test_that("quantile_cut splits at quantiles", {
  y <- quantile_cut(data$WBC, 2, labels = c("Low", "High"))
  expect_equal(levels(y), c("Low", "High"))
  expect_lte(abs(diff(as.numeric(table(y)))), 1)
  expect_equal(nlevels(quantile_cut(c(1, 1, 1, 2), 4)), 2)
})

test_that("shapiro_pval returns NA when the test cannot be computed", {
  expect_true(is.na(shapiro_pval(c(1, 2))))
  expect_true(is.na(shapiro_pval(c(1, 1, 1, 1))))
  expect_true(is.na(shapiro_pval(c(1, 2, NA, NA))))
  expect_true(is.numeric(shapiro_pval(data$Age)))
})

test_that("ROC_calc computes AUC and rejects non binary outcomes", {
  res <- ROC_calc(data, y_var = "NPM1_mut", x_var = c("Age", "WBC"))
  expect_s3_class(res$plot, "ggplot")
  expect_equal(sort(unique(res$param_df$x_var)), c("Age", "WBC"))

  ## AUC = P(score of a positive > score of a negative), compared with the Mann-Whitney estimate
  d <- data[!is.na(data$Age), ]
  pos <- d$Age[d$NPM1_mut == "Yes"]
  neg <- d$Age[d$NPM1_mut == "No"]
  auc_mw <- unname(wilcox.test(pos, neg, exact = FALSE)$statistic) / (length(pos) * length(neg))
  label <- res$plot$layers[[length(res$plot$layers)]]$aes_params$label
  expect_equal(label[1], paste("AUC =", round(auc_mw, 2)))
  expect_error(ROC_calc(data, y_var = "FAB", x_var = "Age"), "exactly 2")
})
