context("Functions test")


data <- read.delim("../../datasets/TCGA_AML_exemple.txt", dec = ",", na.strings = c("", "NA"), stringsAsFactors = T)


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
