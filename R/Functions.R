
#' Anova p value pipe
#' Allows the extraction of an ANOVA p value in a pipe
#'
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param variable A character string corresponding to the studied variable.
#' @param group A character string corresponding to the comparative groups.
#'
#' @export

aov_pipe_pval <- function(data, variable, group) {
  dat <- data %>% select(Group = all_of(group), Variable = all_of(variable))

  anova <- aov(Variable ~ Group, data = dat)
  summary(anova)[[1]][["Pr(>F)"]][[1]]
  return(summary(anova)[[1]][["Pr(>F)"]][[1]])
}

#' Cut a numeric vector into groups of (approximately) equal size
#' Breaks are placed at quantiles (2 = median, 3 = terciles, 4 = quartiles...).
#' Duplicated quantiles (heavily tied data) are merged, so fewer groups may be returned.
#'
#' @param x A numeric vector.
#' @param n Number of groups.
#' @param labels Optional labels. Used only if the number of groups obtained matches.
#'
#' @noRd
quantile_cut <- function(x, n, labels = NULL) {
  breaks <- unique(quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE, names = FALSE))
  if (length(breaks) < 2) {
    return(factor(ifelse(is.na(x), NA, "All")))
  }
  y <- cut(x, breaks = breaks, include.lowest = TRUE, dig.lab = 4)
  if (!is.null(labels) && nlevels(y) == length(labels)) {
    levels(y) <- labels
  }
  y
}

#' Make missing values an explicit "NA" factor level
#'
#' @noRd
explicit_na <- function(x, na_level = "NA") {
  x <- factor(x)
  if (any(is.na(x))) {
    levels(x) <- c(levels(x), na_level)
    x[is.na(x)] <- na_level
  }
  x
}

#' Safe Shapiro-Wilk p-value (NA when the test cannot be computed)
#'
#' @noRd
shapiro_pval <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(x) > 5000 || length(unique(x)) < 2) {
    return(NA_real_)
  }
  shapiro.test(x)$p.value
}

#' Label regression terms with their source variable and comparison
#' Works for glm and coxph fits, relying on the model matrix "assign" attribute
#' rather than on partial name matching (which mislabels variables sharing a prefix, e.g. WBC / WBC_cat).
#'
#' @param fit A fitted glm or coxph model.
#' @param terms Coefficient names to label (e.g. the "term" column of broom::tidy(fit)).
#'
#' @noRd
label_model_terms <- function(fit, terms) {
  mm <- model.matrix(fit)
  term_labels <- attr(terms(fit), "term.labels")
  coef_var <- c("(Intercept)", term_labels)[attr(mm, "assign") + 1]
  names(coef_var) <- colnames(mm)

  var <- unname(coef_var[terms])
  level <- substring(terms, nchar(var) + 1)
  clean_var <- gsub("^`|`$", "", var)

  reference <- vapply(seq_along(terms), function(i) {
    if (!is.null(fit$xlevels[[clean_var[i]]])) fit$xlevels[[clean_var[i]]][1] else "FALSE"
  }, character(1))

  data.frame(
    x_var = clean_var,
    Comparison = ifelse(level == "", "Continuous", paste0(level, "_vs_", reference)),
    stringsAsFactors = FALSE
  )
}

#' All-in-one publication ready descriptive table
#' This function output a publication-ready descriptive table from a dataset. Each columns of the dataset is compared between the defined groups.
#'
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param group A character string corresponding to the comparative groups.
#' @param na.include Should missing values be included in groups?
#' @param percent_type For categorical variables, should percentage be calculated on the row (1) or columns (2)?
#' @param padj_method Select the p.value adjustment method (none, fdr, holm or bonferonni).
#' @param show_methods Should statistical tests names be precised in a supplementary column?
#' @param exclude_vector columns to exclude from the analysis.
#'
#' @export

descriptive_table <- function(data, group, na.include = F, percent_type = 1, padj_method = "none", show_methods = F,
                              exclude_vector = c("Patient_id", "patient_id", "Sample_ID","Whole_cohort")) {

  if (group != "Whole_cohort" && nlevels(factor(data[[group]])) < 2) {
    stop("The group variable '", group, "' must contain at least 2 different values.")
  }

  if (group == "Whole_cohort") {

    ## Filter out var with >80% NA
    na_var <- colnames(data)[colSums(is.na(data)) >= 0.8 * nrow(data)]
    if (length(na_var) > 0) {
      message(paste0(length(na_var), " variables with >80% missing values were removed from the analysis "))
    }

    var_num <- colnames(data)[vapply(data, is.numeric, logical(1))]
    var_num <- var_num[!var_num %in% c(exclude_vector, na_var)]

    var_cat <- colnames(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
    var_cat <- var_cat[!var_cat %in% c(exclude_vector, na_var)]


    ########## ========== Variables numériques

    list_num <- list()
    for (v in var_num) {

      ##### ===== DF
      {
        df <- data.frame(variable = data[, v])
        colnames(df)[1] <- "variable"
      }


      ##### ===== Mean (sd) and Median (IQR)
      temp <- df %>%
        summarise(
          "Mean (sd)" = paste0(round(mean(variable, na.rm = T), 2), " (", round(sd(variable, na.rm = T), 2), ")"),
          "Median [IQR]" = paste0(
            round(median(variable, na.rm = T),2), " [", 
            round(quantile(variable, 0.25, na.rm = T), 2), "-",
            round(quantile(variable, 0.75, na.rm = T), 2), "]"
          )
        )



      ##### ===== Mise en forme résultats
      {
        merged_results <- data.frame("Whole cohort" = t(temp), check.names = F) %>%
          rownames_to_column("Type")


        merged_results[1, "Variable"] <- v

        merged_results <- select(merged_results, "Variable", everything())
      }


      list_num[[v]] <- merged_results
    }


    ########## ========== Variables cat
    list_cat <- list()
    for (v in var_cat) {

      ##### ===== DF
      {
        df <- data.frame(variable = data[, v])
       colnames(df)[1] <- "variable"


        if (na.include == T) {
          df <- df %>% mutate_if(is.factor, explicit_na)
        } else {
          df <- df %>% filter(is.na(variable) == F)
        }
      }


      ##### ===== Contingency tables

      table <- table(df)
      frequencies <- round((prop.table(table) * 100), 2)
      levels <- nlevels(factor(df$variable))
      table_freq <- data.frame(cbind(table, frequencies))

      ##### ===== Mise en forme résultats
      {
        merged_results <- table_freq %>%
          rownames_to_column("Type") %>%
          transmute(
            Type = Type,
            "Whole cohort" = paste0(table, " (", table_freq$frequencies, ")")
          )

        merged_results[1, "Variable"] <- v

        merged_results <- select(merged_results, "Variable", everything())
      }




      list_cat[[v]] <- merged_results
    }


    ########## ========== Final df
    final_df <- rbind(bind_rows(list_num), bind_rows(list_cat))
  } else {

    ## Filter out var with >80% NA
    na_var <- colnames(data)[colSums(is.na(data)) >= 0.8 * nrow(data)]
    if (length(na_var) > 0) {
      message(paste0(length(na_var), " variables with >80% missing values were removed from the analysis "))
    }


    var_num <- colnames(data)[vapply(data, is.numeric, logical(1))]
    var_num <- var_num[!var_num %in% c(exclude_vector, na_var)]

    var_cat <- colnames(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
    var_cat <- var_cat[!var_cat %in% c(exclude_vector, group, na_var)]

    ########## ========== Variables numériques

    list_num <- list()
    for (v in var_num) {

      ##### ===== DF
      {
        df <- data[, c(group, v)]
        colnames(df) <- c("group", "variable")
        df$group <- factor(df$group)


        if (na.include == T) {
          df <- df %>% mutate_if(is.factor, explicit_na)
        } else {
          df <- df %>% filter(is.na(group) == F)
        }
      }


      ##### ===== Mean (sd) and Median (IQR)
      temp <- df %>%
        group_by(group) %>%
        summarise(
          "Mean (sd)" = paste0(round(mean(variable, na.rm = T), 2), " (", round(sd(variable, na.rm = T), 2), ")"),
          "Median [IQR]" = paste0(
            round(median(variable, na.rm = T),2), " [", 
            round(quantile(variable, 0.25, na.rm = T), 2), "-",
            round(quantile(variable, 0.75, na.rm = T), 2), "]"
          )
        ) %>%
        column_to_rownames("group")

      ##### ===== Statistics
      {
        if (nlevels(df$group) == 2) {
          param <- t.test(df$variable ~ df$group, paired = F)
          non_param <- wilcox.test(df$variable ~ df$group, paired = F)
        } else {
          param <- anova(lm(df$variable ~ df$group))
          non_param <- kruskal.test(df$variable, df$group)
        }
      }




      ##### ===== Mise en forme résultats
      {
        merged_results <- data.frame(t(temp), check.names = F) %>%
          rownames_to_column("Type")

        ## Ajout ligne NA
        if (na.include == T) {
          N_NA <- df %>%
            group_by(group) %>%
            summarise("NA n (%)" = paste0(sum(is.na(variable)), " (", round(sum(is.na(variable)) * 100 / n(), 2), ")")) %>%
            column_to_rownames("group") %>%
            t() %>%
            data.frame(check.names = F) %>%
            rownames_to_column("Type")
          merged_results <- rbind(merged_results, N_NA)
        }

        merged_results[1, "Variable"] <- v
        merged_results[1, "param_pvalue"] <- ifelse(nlevels(df$group) > 2, param$`Pr(>F)`[1], param$p.value)
        merged_results[1, "param_method"] <- ifelse(nlevels(df$group) > 2, "ANOVA", param$method)
        merged_results[1, "non_param_pvalue"] <- non_param$p.value
        merged_results[1, "non_param_method"] <- non_param$method

        merged_results <- select(merged_results, "Variable", everything())
      }


      list_num[[v]] <- merged_results
    }


    ########## ========== Variables cat
    list_cat <- list()
    for (v in var_cat) {

      ##### ===== DF
      {
        df <- data[, c(group, v)]
        colnames(df) <- c("group", "variable")
        df$group <- factor(df$group)

        if (na.include == T) {
          df <- df %>% mutate_if(is.factor, explicit_na)
        } else {
          df <- df %>% filter(is.na(group) == F)
        }
      }


      ##### ===== Contingency tables

      table <- table(df)
      frequencies <- round((prop.table(table, percent_type) * 100), 2) ## Col percentage
      group_levels <- levels(df$group)
      table_freq <- data.frame(cbind(table, frequencies))
      mylist <- list()
      for (nm in group_levels) {
        temp <- data.frame(cbind(table[nm, ], frequencies[nm, ]))
        temp[, 2] <- gsub("^", " (", temp[, 2])
        temp[, 2] <- gsub("$", ") ", temp[, 2])
        temp <- temp %>%
          unite(nm, X1, X2, sep = "")
        colnames(temp)[1] <- paste0(nm)

        mylist[[nm]] <- temp
      }
      table_freq <- bind_cols(mylist)
      rownames(table_freq) <- rownames(mylist[[1]])

      ##### ===== Statistics
      {
        param <- chisq.test(table)
        non_param <- fisher.test(table, simulate.p.value = T)
      }


      ##### ===== Mise en forme résultats
      {
        merged_results <- table_freq %>%
          rownames_to_column("Type")

        merged_results[1, "Variable"] <- v
        merged_results[1, "param_pvalue"] <- param$p.value
        merged_results[1, "param_method"] <- param$method
        merged_results[1, "non_param_pvalue"] <- non_param$p.value
        merged_results[1, "non_param_method"] <- gsub("for Count Data ", "", non_param$method)
        merged_results[1, "non_param_method"] <- gsub("\\n.*$", "", merged_results[1, "non_param_method"])
        merged_results <- select(merged_results, "Variable", everything())
      }




      list_cat[[v]] <- merged_results
    }


    ########## ========== Final df
    final_df <- rbind(bind_rows(list_num), bind_rows(list_cat))
  }



  final_df$Variable[is.na(final_df$Variable)] <- ""
  if (group != "Whole_cohort") {
    final_df$param_pvalue_adj <- final_df$param_pvalue
    final_df$param_pvalue_adj[which(is.na(final_df$param_pvalue_adj) == F)] <- p.adjust(final_df$param_pvalue_adj[which(is.na(final_df$param_pvalue_adj) == F)], method = padj_method)
    final_df$non_param_pvalue_adj <- final_df$non_param_pvalue
    final_df$non_param_pvalue_adj[which(is.na(final_df$non_param_pvalue) == F)] <- p.adjust(final_df$non_param_pvalue[which(is.na(final_df$non_param_pvalue) == F)], method = padj_method)

    pval_cols <- c("param_pvalue", "param_pvalue_adj", "non_param_pvalue", "non_param_pvalue_adj")
    na_to_remove_cols <- c(pval_cols, "param_method", "non_param_method")

    final_df[, pval_cols] <- apply(final_df[, pval_cols], 2, function(x) {
      format.pval(x, 2)
    })

    final_df[, na_to_remove_cols][final_df[, na_to_remove_cols] == "NA"] <- ""


    if (padj_method == "none") {
      final_df <- final_df %>% select(-all_of("param_pvalue_adj"), -all_of("non_param_pvalue_adj"))
    } else {
      final_df <- final_df %>% select(-all_of("param_pvalue"), -all_of("non_param_pvalue"))
    }

    if (show_methods == F) {
      final_df <- final_df %>% select(-all_of("param_method"), -all_of("non_param_method"))
    }
  }
  return(final_df)
}



#' Automatised plot
#' This is a wrapper function to set a plot with different parameters from ggplot.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param variable A character string corresponding to the studied variable.
#' @param group A character string corresponding to the comparative groups.
#' @param group_filter_vector A character string/vector defining groups sublevels to be included in the analysis.
#' @param na_exclude_group Should missing values be excluded from groups?
#' @param plot_type Set the plot type (Boxplot, Barchart_mean, Barchart_count or Histogram).
#' @param add_points Add data points as a scatter plot to the graph.
#' @param error_bar Type of error bar to be shown on the barcharts (IC95, sd, se or hide).
#' @param stat Type of statistics to show (param, non_param or no).
#' @export


autoplot <- function(data, variable, group, group_filter_vector = NULL, na_exclude_group = T,
                     plot_type = "Boxplot", add_points = T, error_bar = "IC95",
                     stat = "param") {

  ## DF
  data_plot <- data %>% select(Group = all_of(group), Variable = all_of(variable))

  if (na_exclude_group == T) {
    data_plot <- data_plot %>%
      na.omit("Group") %>%
      droplevels()
  }
  if (is.null(group_filter_vector) == F) {
    data_plot <- data_plot %>%
      filter(Group %in% group_filter_vector) %>%
      droplevels()
  }

  ### Basic plot
  p <- ggplot(data_plot, aes(x = Group, y = Variable, group = Group, fill = Group))


  ## Aesthetics
  if (plot_type == "Boxplot") {
    if (add_points == T) {
      p <- p + geom_boxplot() + geom_point(position = position_jitterdodge(0.2))
    } else {
      p <- p + geom_boxplot()
    }
  }

  if (plot_type == "Barchart_mean") {
    data_stat <- data_plot %>%
      group_by(Group) %>%
      summarise(
        n = sum(!is.na(Variable)),
        mean = mean(Variable, na.rm = TRUE),
        sd = sd(Variable, na.rm = TRUE),
        hide = 0
      ) %>%
      mutate(se = sd / sqrt(n)) %>%
      mutate(IC95 = se * qt((1 - 0.05) / 2 + 0.5, n - 1))

    data_stat$error <- unlist(as.vector(data_stat[, error_bar]))

    p <- data_stat %>%
      ggplot(aes(x = Group, y = mean, fill = Group)) +
      geom_bar(stat = "identity", color = "black", size = 0.75)

    if (error_bar != "hide") {
      p <- p +
        geom_errorbar(aes(x = Group, ymin = mean - error, ymax = mean + error), width = 0.4, colour = "black", alpha = 0.9, size = 1.5)
    }

    if (add_points == T) {
      p <- p + geom_point(data = data_plot, aes(x = Group, y = Variable), position = position_jitterdodge(0.2))
    }
  }

  if (plot_type == "Barchart_count") {
    p <- ggplot(data_plot, aes(x = Group, fill = Variable)) +
      geom_bar(position = "stack", color = "black", size = 0.75)
  }

  if (plot_type == "Histogram") {
    if (is.numeric(data_plot$Variable)) {
      p <- ggplot(data_plot, aes(x = Variable, fill = Group)) +
        geom_histogram(position = "identity", bins = 30, color = "black", alpha = 0.5)
    } else {
      p <- ggplot(data_plot, aes(x = Variable, fill = Group)) +
        geom_bar(position = "dodge", color = "black", size = 0.75)
    }
  }



  ## Add statistics
  if (stat != "no") {
    var_class <- class(data_plot$Variable)

    if (nlevels(data_plot$Group) == 2 & var_class %in% c("numeric", "double", "integer")) {
      p_value <- ifelse(stat == "param",
        paste0("T-test p-value: ", format.pval(summarise(data_plot, pval = t.test(Variable ~ Group, paired = F)$p.value), 3, eps = 0.001)),
        paste0("Wilcoxon's test p-value: ", format.pval(summarise(data_plot, pval = wilcox.test(Variable ~ Group, paired = F)$p.value), 3, eps = 0.001))
      )
    }

    if (nlevels(data_plot$Group) > 2 & var_class %in% c("numeric", "double", "integer")) {
      p_value <- ifelse(stat == "param",
        paste0("ANOVA p-value: ", format.pval(aov_pipe_pval(data_plot, "Variable", "Group"), 3, eps = 0.001)),
        paste0("Kruskal-Wallis p-value: ", format.pval(kruskal.test(data_plot$Variable, data_plot$Group)$p.value, 3, eps = 0.001))
      )
    }

    if (!var_class %in% c("numeric", "double", "integer")) {
      p_value <- ifelse(stat == "param",
        paste0("Chi-squared test p-value: ", format.pval(chisq.test(table(data_plot))$p.value, 3, eps = 0.001)),
        paste0("Fisher's test p-value: ", format.pval(fisher.test(table(data_plot), simulate.p.value = T)$p.value, 3, eps = 0.001))
      )
    }

    ylim_inf <- unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])[1]
    ylim_sup <- unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])[2]

    y_pos <- 0.95 * ylim_sup
    x_pos <- mean(unlist(ggplot_build(p)$layout$panel_params[[1]][("x.range")]))

    # x_pos = ((nlevels(data_plot$Group)+1)/2)
    # y_pos = ifelse((plot_type == "Barchart_count" | (plot_type == "Barchart_mean" & add_points == F)),
    #   (max(df_y$ymax) + 0.10* max(df_y$ymax)),
    #   (max(data.frame(ggplot_build(p)[[1]][[3]])$y) + 0.10* max(data.frame(ggplot_build(p)[[1]][[3]])$y))
    # )

    p <- p + annotate("text", x = x_pos, y = y_pos, label = p_value, size = 6)
  }





  ## final plot with theme layer

  res <- list()
  res[["graph"]] <- p
  res[["lim"]] <- data.frame(
    xlim = unlist(ggplot_build(p)$layout$panel_params[[1]][("x.range")]),
    ylim = unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])
  )


  return(res)
}



#' Automatised plot for paired data
#' This is a wrapper function to set a plot with different parameters from ggplot for paired data.
#'
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param timepoints A character vector corresponding to the studied paired variables.
#' @param group A character string corresponding to the comparative groups.
#' @param plot_type Set the plot type (Boxplot, Barchart_mean or  Barchart_count).
#' @param add_points Add data points as a scatter plot to the graph.
#' @param add_lines Add line to paired data
#' @param add_individual_lines Add each individual (samples) lines to the plot.
#' @param error_bar Type of error bar to be shown on the barcharts (IC95 or se or hide).
#' @param stat Type of statistics to show (param, non_param or no).
#' @param alpha_line Transparency of the paired lines
#'
#' @export


autoplot_paired <- function(data, timepoints, group,
                            plot_type = "Mean_lines", add_points = F, add_lines = T, add_individual_lines = F,
                            error_bar = "IC95",
                            stat = "param",
                            alpha_line = 0.5) {

  ## DF
  if (group == "None") {
    group <- "Whole_cohort"
  }
  data_plot <- data %>%
    select(Patient_id, Group = all_of(group), all_of(timepoints)) %>%
    na.omit() %>%
    droplevels() %>%
    pivot_longer(all_of(timepoints), names_to = "Timepoint", values_to = "value")

  data_plot$Timepoint <- factor(data_plot$Timepoint, levels = timepoints)
  
  data_stat <- data_plot %>%
    group_by(Group, Timepoint) %>%
    summarise(
      n = n(),
      mean = mean(value),
      sd = sd(value),
      hide = factor(1)
    ) %>%
    mutate(se = sd / sqrt(n)) %>%
    mutate(IC95 = se * qt((1 - 0.05) / 2 + 0.5, n - 1))

  data_stat$error <- unlist(as.vector(data_stat[, error_bar]))


  ### Basic plot
  # p <- ggplot(data_plot, aes(x=Timepoint, y=value, fill = Timepoint))


  if (plot_type == "Mean_lines") {
    p <- ggplot(data_stat, aes(x = Timepoint, y = mean, color = Group, group = Group)) +
      geom_pointrange(aes(ymin = mean - error, ymax = mean + error), size = 1)

    if (add_lines == T) {
      p <- p + geom_line(size = 1)
    }

    if (add_points == T) {
      p <- p +
        geom_point(data = data_plot, aes(x = Timepoint, y = value, color = Group))
    }

    if (add_individual_lines == T) {
      p <- p + geom_line(
        data = data_plot,
        aes(x = Timepoint, group = factor(Patient_id), y = value), alpha = alpha_line
      )
    }
  }

  ## Aesthetics
  if (plot_type == "Boxplot") {
    p <- ggplot(data_plot, aes(x = Timepoint, y = value, color = Group)) +
      geom_boxplot(outlier.shape = NA, size = 0.75)

    if (add_points == T) {
      p <- p + geom_point()
    }

    if (add_individual_lines == T) {
      p <- p +
        geom_line(aes(group = factor(Patient_id), color = Group), alpha = alpha_line)
    }

    if (add_lines == T) {
      p <- p + geom_line(data = data_stat, aes(x = Timepoint, y = mean, color = Group, group = Group), size = 1)
    }
  }



  ## Add statistics
  if (stat != "no") {

    # var_class <- class(data_plot$Timepoint)

    if (nlevels(factor(data_plot$Timepoint)) == 2 & nlevels(factor(data_plot$Group)) == 1) {
      p_value <- ifelse(stat == "param",
        paste0("T-test p-value: ", format.pval(t.test(data[, timepoints[1]], data[, timepoints[2]], paired = T)$p.value, 3)),
        paste0("Wilcoxon test p-value: ", format.pval(wilcox.test(data[, timepoints[1]], data[, timepoints[2]], paired = T)$p.value, 3))
      )
    }

    if (nlevels(factor(data_plot$Timepoint)) > 2 & nlevels(factor(data_plot$Group)) == 1) {
      anova <- aov(value ~ Timepoint + Error(Patient_id / Timepoint), data = data_plot)

      p_value <- ifelse(stat == "param",
        paste0("ANOVA p-value: ", format.pval(broom::tidy(anova)$p.value[which(is.na(broom::tidy(anova)$p.value) == F)], 3)),
        paste0("Friedman test p-value: ", format.pval(friedman.test(value ~ Timepoint | Patient_id, data = data_plot)$p.value, 3))
      )
    }

    if (nlevels(factor(data_plot$Group)) >= 2) {
      anova <- aov(value ~ Group * Timepoint + Error(Patient_id / (Group * Timepoint)), data = data_plot)
      groups_pval <- format.pval(broom::tidy(anova)$p.value[broom::tidy(anova)$term == "Group"], 3)
      timept_pval <- format.pval(broom::tidy(anova)$p.value[broom::tidy(anova)$term == "Timepoint"], 3)
      interact_pval <- format.pval(broom::tidy(anova)$p.value[broom::tidy(anova)$term == "Group:Timepoint"], 3)

      p_value <- paste0("
                        Two way ANOVA p-values: 
                        Groups: ", groups_pval, " 
                        Timepoints: ", timept_pval, " 
                        Interaction: ", interact_pval)
    }

    ylim_inf <- unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])[1]
    ylim_sup <- unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])[2]

    y_pos <- 0.95 * ylim_sup
    x_pos <- mean(unlist(ggplot_build(p)$layout$panel_params[[1]][("x.range")]))

    stat_size <- 6

    if (nlevels(factor(data_plot$Group)) >= 2) {
      y_pos <- 0.90 * ylim_sup
      x_pos <- 1.4 * min(unlist(ggplot_build(p)$layout$panel_params[[1]][("x.range")]))
      stat_size <- 5
    }

    # x_pos = ((nlevels(data_plot$Group)+1)/2)
    # y_pos = ifelse((plot_type == "Barchart_count" | (plot_type == "Barchart_mean" & add_points == F)),
    #   (max(df_y$ymax) + 0.10* max(df_y$ymax)),
    #   (max(data.frame(ggplot_build(p)[[1]][[3]])$y) + 0.10* max(data.frame(ggplot_build(p)[[1]][[3]])$y))
    # )

    p <- p + annotate("text", x = x_pos, y = y_pos, label = p_value, size = stat_size)
  }



  ## final plot with theme layer

  res <- list()
  res[["graph"]] <- p
  res[["lim"]] <- data.frame(
    xlim = unlist(ggplot_build(p)$layout$panel_params[[1]][("x.range")]),
    ylim = unlist(ggplot_build(p)$layout$panel_params[[1]][("y.range")])
  )


  return(res)
}




#################### ==================== Models ====================  ####################

#' Regression base and diagnosis dataframe
#' Provides augmented data for regression diagnosis
#' @param y_var A character string corresponding to the y variable.
#' @param x_var A character string corresponding to the x variable(s).
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param model Type of generalised linear model (lm or gam).
#' @param cor_type Type of correlation to show on the graph (pearson or spearman).
#' @export

regression_dataframes <- function(y_var, x_var, data, model = "lm",
                                  cor_type = "pearson") {
  res <- list()
  if (model == "lm") {
    res <- list()
    if (!is.numeric(data[[x_var]])) {
      data[, x_var] <- as.numeric(as.factor(data[, x_var]))
    }

    formula <- as.formula(paste0("`", y_var, "` ~ `", x_var, "`"))
    model <- glm(formula, data = data, family = "gaussian")
    res[["model"]] <- model
    res[["augment_df"]] <- broom::augment(model)
    res[["tidy_df"]] <- broom::tidy(model)
    res[["cor_df"]] <- rbind(broom::tidy(cor.test(data[, y_var], data[, x_var], method = cor_type)))
  } else {
    res <- list()
    formula <- as.formula(paste0("`", y_var, "` ~ s(`", x_var, "`)"))
    model <- mgcv::gam(formula, data = data, method = "REML")
    res[["model"]] <- model
    ## model$y only keeps complete cases (same length as fitted values)
    res[["augment_df"]] <- data.frame(y_var=model$y,
                                      ".fitted"=fitted(model),
                                      ".resid"=residuals(model))
    colnames(res[["augment_df"]])[1] <- y_var
    res[["tidy_df"]] <- broom::tidy(model)
    res[["cor_df"]] <- broom::tidy(cor.test(data[, y_var], data[, x_var], method = cor_type))
  }

  res[["augment_df"]]$Std_residuals <- scale(res[["augment_df"]]$.resid)
  return(res)
}


#' Format a tidy regression output into a publication-ready table
#'
#' @param res A broom::tidy(conf.int = TRUE) data frame with x_var and Comparison columns.
#' @param coef_name Name of the coefficient column (Beta Coeff., Odds Ratio, HR).
#' @param exponentiate Should estimates and confidence intervals be exponentiated (OR, HR)?
#'
#' @noRd
format_regression_table <- function(res, coef_name, exponentiate = FALSE,
                                    pval_digits = 2, pval_eps = .Machine$double.eps) {
  if (exponentiate) {
    ## Exponentiate before rounding to keep OR/HR precision
    res <- res %>% mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high))
  }

  res <- res %>%
    select(
      "X Variables" = x_var, Comparison,
      estimate, "CI95_low" = conf.low, "CI95_high" = conf.high,
      "P.Value" = p.value
    ) %>%
    mutate(multiv_graph = paste0(`X Variables`, "_", Comparison))

  res$Adj_P.Value <- p.adjust(res$P.Value, method = "fdr")
  res$estimate <- round(res$estimate, 2)
  res$CI95_low <- round(res$CI95_low, 2)
  res$CI95_high <- round(res$CI95_high, 2)

  res$P.Value <- format.pval(res$P.Value, pval_digits, eps = pval_eps)
  res$Adj_P.Value <- format.pval(res$Adj_P.Value, pval_digits, eps = pval_eps)

  colnames(res)[3] <- coef_name
  return(res)
}

#' Tidy a fitted model and label its terms
#'
#' @noRd
tidy_labelled <- function(fit) {
  res <- broom::tidy(fit, conf.int = TRUE) %>% filter(term != "(Intercept)")
  bind_cols(res, label_model_terms(fit, res$term))
}

#' All-in-one publication ready regression table for glm models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is independantly tested as a x variable in a univariate model.
#' Variables for which the model cannot be fitted (e.g. a single level factor) are skipped with a warning.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable.
#' @param family Generalized linear model family (gaussian or binomial)
#' @export

regression_table <- function(data, y_var, family = "gaussian") {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), everything(), -any_of(exclude_vector))

  x_vars <- colnames(data_reg)[-1]

  res_temp <- list()

  for (v in x_vars) {
    formula <- as.formula(paste0("y_var ~ `", v, "`"))

    res_temp[[v]] <- tryCatch(
      tidy_labelled(glm(formula, family = family, data_reg)),
      error = function(e) {
        warning("Variable '", v, "' skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  format_regression_table(
    bind_rows(res_temp),
    coef_name = ifelse(family == "binomial", "Odds Ratio", "Beta Coeff."),
    exponentiate = family == "binomial"
  )
}


#' All-in-one publication ready regression table for cox models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is independantly tested as a x variable in a univariate model.
#' Variables for which the model cannot be fitted (e.g. a single level factor) are skipped with a warning.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable = time-dependant outcome (0-1 or dead-alive for example).
#' @param time_var A numeric variable corresponding to the time variable.
#' @export

regression_table_cox <- function(data, y_var, time_var) {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), time_var = all_of(time_var), everything(), -any_of(exclude_vector))

  x_vars <- colnames(data_reg)[-c(1:2)]

  res_temp <- list()

  for (v in x_vars) {
    formula <- as.formula(paste0("Surv(time_var,y_var) ~ `", v, "`"))

    res_temp[[v]] <- tryCatch(
      tidy_labelled(coxph(formula, data_reg)),
      error = function(e) {
        warning("Variable '", v, "' skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  format_regression_table(bind_rows(res_temp), coef_name = "HR", exponentiate = TRUE)
}

#' All-in-one publication ready regression table for glm models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is included as a x variable in a multivariate model.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable.
#' @param family Generalized linear model family (gaussian or binomial)
#' @export

regression_table_multi <- function(data, y_var, family = "gaussian") {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), everything(), -any_of(exclude_vector))

  fit <- glm(y_var ~ ., family = family, data_reg)

  format_regression_table(
    tidy_labelled(fit),
    coef_name = ifelse(family == "binomial", "Odds Ratio", "Beta Coeff."),
    exponentiate = family == "binomial",
    pval_digits = 3, pval_eps = 0.001
  )
}

#' All-in-one publication ready regression table for cox models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is included as a x variable in a multivariate model.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable.
#' @param time_var A numeric variable corresponding to the time variable.
#' @export

regression_table_multi_cox <- function(data, y_var, time_var) {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), time_var = all_of(time_var), everything(), -any_of(exclude_vector))

  fit <- coxph(Surv(time_var, y_var) ~ ., data_reg)

  format_regression_table(tidy_labelled(fit), coef_name = "HR", exponentiate = TRUE)
}

#' Dedicated function to plot ROC Curves & compute parameters
#' This function output individual ROC Curves for each x-variable provided.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable. !!Must be a 2-level factor (TRUE/FALSE, 1/0)
#' @param x_var A vector of numeric variable to test
#' @param value Use raw parameters value or predictions from logit model?
#' @export

ROC_calc <- function(data, y_var, x_var, value = "raw"){
  
  res_list <- list()
  
  n_scores = length(x_var)

  if (length(unique(na.omit(data[[y_var]]))) != 2) {
    stop("The response variable '", y_var, "' must have exactly 2 different values (e.g. Yes/No, TRUE/FALSE, 1/0).")
  }
  
  if(value == "raw"){
    AUC_df <- data %>% select(Response = all_of(y_var), all_of(x_var)) %>%
      pivot_longer(-Response, names_to = "x_var", values_to = "Prediction")
    
  } else {
    
    predictions <- list()
    
    for(var in x_var){
      
      formula <- as.formula(paste0("`", y_var, "` ~ `", var, "`"))
      
      model <- glm(formula, data, family = "binomial")
      
      predictions[[var]] <- data.frame(Response = data[,y_var],
                                       x_var = var,
                                       Prediction=predict(model, data))
      
    }
    
    AUC_df <- dplyr::bind_rows(predictions)
    
    
  }
  
  
  AUC_df <- AUC_df %>% filter(!is.na(Response), !is.na(Prediction))

  plot <- ggplot(AUC_df, aes(d = Response, m = Prediction, color = x_var)) +
    geom_roc(size = 1.5) + style_roc() 
  
  ## AUC computed with ROCit: plotROC::calc_auc() relies on `%||%`, only available in base R >= 4.4
  ## (first sorted level of the response = negative class, as in geom_roc)
  auc <- AUC_df %>%
    group_by(x_var) %>%
    summarise(AUC = ROCit::rocit(score = Prediction, class = Response,
                                 negref = as.character(sort(unique(Response))[1]))$AUC)

  plot <- plot + annotate("text", x = 0.80, y = c(seq(0.1,0.50,length.out  = n_scores)), 
                          label = paste("AUC =", round(auc$AUC, 2)), colour = c(scales::hue_pal()(n_scores)), size = 6) +
    labs(title = paste0(y_var," - ROC curves"),
         color = "Variables") +
    default_theme +
    theme(legend.position = "right")
  
  res_list$plot <- plot
  
  
  ##### ===== Calculate parameters
  
  param_list <- list()
  
  for(var in x_var){
    
    param_df <- AUC_df %>% filter(x_var == var)
    
    param_list[[var]] <- ROCit::measureit(score =  param_df$Prediction, 
                                          class = param_df$Response,
                                          measure = c("ACC", "SENS","SPEC", "PPV","NPV"))
    
    class(param_list[[var]]) = "data.frame"
  }
  
  res_list$param_df <- bind_rows(param_list, .id = "x_var") %>%
    mutate_at(-1, function(x){round(x,2)})
  
  return(res_list)
  
  
}
