
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

cut2 <- function(x) {
  y <- quantcut(x, q = 2)
  y <- factor(y,
    order = FALSE,
    labels = c("Low", "High")
  )
  return(y)
}
cut3 <- function(x) {
  y <- quantcut(x, q = 3)

  if (nlevels(y) < 3) {
    y <- factor(y, order = F, labels = c("Low", "High"))
  } else {
    y <- factor(y, order = F, labels = c("Low", "Intermediate", "High"))
  }

  return(y)
}
cut4 <- function(x) {
  quantcut(x, q = 4) %>%
    factor(
      order = FALSE,
      labels = c("Low", "Intermediate1", "Intermediate2", "High")
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

  
  if (group == "Whole_cohort") {

    ## Filter out var with >80% NA
    na_var <- colnames(data)[colSums(is.na(data)) >= 0.8 * nrow(data)]
    if (length(na_var) > 0) {
      message(paste0(length(na_var), " variables with >80% missing values were removed from the analysis "))
    }

    var_num <- colnames(data[sapply(data, class) %in% c("numeric", "integer", "double")])
    var_num <- var_num[!var_num %in% c(exclude_vector, na_var)]

    var_cat <- colnames(data[sapply(data, class) %in% c("factor", "character")])
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
            round(median(variable, na.rm = T) - IQR(variable, na.rm = T), 2), "-",
            round(median(variable, na.rm = T) + IQR(variable, na.rm = T), 2), "]"
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
          df <- df %>% mutate_if(is.factor,
            fct_explicit_na,
            na_level = "NA"
          )
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


    var_num <- colnames(data[sapply(data, class) %in% c("numeric", "integer", "double")])
    var_num <- var_num[!var_num %in% c(exclude_vector, na_var)]

    var_cat <- colnames(data[sapply(data, class) %in% c("factor", "character")])
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
          df <- df %>% mutate_if(is.factor,
            fct_explicit_na,
            na_level = "NA"
          )
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
            median(variable, na.rm = T), " [", round(median(variable, na.rm = T) - IQR(variable, na.rm = T), 2), "-",
            round(median(variable, na.rm = T) + IQR(variable, na.rm = T), 2), "]"
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
          df <- df %>% mutate_if(is.factor,
            fct_explicit_na,
            na_level = "NA"
          )
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
#' @param plot_type Set the plot type (Boxplot, Barchart_mean or  Barchart_count).
#' @param add_points Add data points as a scatter plot to the graph.
#' @param error_bar Type of error bar to be shown on the barcharts (IC95 or hide).
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
        n = n(),
        mean = mean(Variable),
        sd = sd(Variable),
        hide = factor(1)
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
        paste0("ANOVA p-value: ", format.pval(aov_pipe_pval(data, variable, group), 3, eps = 0.001)),
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
    if (!class(data[, x_var]) %in% c("numeric", "integer", "double")) {
      data[, x_var] <- as.numeric(as.factor(data[, x_var]))
    }

    formula <- as.formula(paste(y_var, "~", x_var))
    model <- glm(formula, data = data, family = "gaussian")
    res[["model"]] <- model
    res[["augment_df"]] <- broom::augment(model)
    res[["tidy_df"]] <- broom::tidy(model)
    res[["cor_df"]] <- rbind(broom::tidy(cor.test(data[, y_var], data[, x_var], method = cor_type)))
  } else {
    res <- list()
    formula <- as.formula(paste(y_var, "~ s(", x_var, ")"))
    model <- mgcv::gam(formula, data = data, method = "REML")
    res[["model"]] <- model
    res[["augment_df"]] <- data.frame(y_var=data[,y_var],
                                      ".fitted"=fitted(model),
                                      ".resid"=residuals(model))
    colnames(res[["augment_df"]])[1] <- y_var
    res[["tidy_df"]] <- broom::tidy(model)
    res[["cor_df"]] <- broom::tidy(cor.test(data[, y_var], data[, x_var], method = cor_type))
  }

  res[["augment_df"]]$Std_residuals <- scale(res[["augment_df"]]$.resid)
  return(res)
}


#' All-in-one publication ready regression table for glm models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is independantly tested as a x variable in a univariate model.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable.
#' @param family Generalized linear model family (gaussian or binomial)
#' @export

regression_table <- function(data, y_var, family = "gaussian") {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), everything(), -one_of(exclude_vector))

  x_vars <- colnames(data_reg)[-1]

  res_temp <- list()

  for (v in x_vars) {
    formula <- as.formula(paste0("y_var ~ ", v))

    fit <- glm(formula, family = family, data_reg)
    res <- broom::tidy(fit, conf.int = TRUE) %>% filter(term != "(Intercept)")
    if (class(data_reg[, v]) %in% c("factor", "character")) {
      res$term <- paste0(gsub(v, "", res$term), "_vs_", fit$xlevels[[1]][1])
    }
    if (class(data_reg[, v]) %in% c("numeric", "double", "integer")) {
      res$term <- "Continuous"
    }

    res_temp[[v]] <- res
  }


  res <- bind_rows(res_temp, .id = "x_var") %>%
    filter(term != "(Intercept)") %>%
    select(
      "X Variables" = x_var, "Comparison" = term,
      "Beta Coeff." = estimate, "CI95_low" = conf.low, "CI95_high" = conf.high,
      "P.Value" = p.value
    ) %>%
    mutate(multiv_graph = paste0(`X Variables`, "_", Comparison))


  res$Adj_P.Value <- p.adjust(res$P.Value, method = "fdr")
  res$`Beta Coeff.` <- round(res$`Beta Coeff.`, 2)
  res$CI95_low <- round(res$CI95_low, 2)
  res$CI95_high <- round(res$CI95_high, 2)

  res$P.Value <- format.pval(res$P.Value, 2)
  res$Adj_P.Value <- format.pval(res$Adj_P.Value, 2)

  if (family == "binomial") {
    res$`Beta Coeff.` <- round(exp(res$`Beta Coeff.`), 2)
    res$CI95_low <- round(exp(res$CI95_low), 2)
    res$CI95_high <- round(exp(res$CI95_high), 2)

    colnames(res)[3] <- "Odds Ratio"
  }


  return(res)
}


#' All-in-one publication ready regression table for cox models
#' This function output a publication-ready regression table from a dataset. Each column of the dataset is independantly tested as a x variable in a univariate model.
#' @param data A dataframe with row corresponding to samples/patients and columns to variables.
#' @param y_var A character string corresponding to the y variable = time-dependant outcome (0-1 or dead-alive for example).
#' @param time_var A numeric variable corresponding to the time variable.
#' @export

regression_table_cox <- function(data, y_var, time_var) {

  ## DF
  exclude_vector <- c("Patient_id", "patient_id", "Sample_ID", "sample_id", "Whole_cohort")

  data_reg <- data %>% select(y_var = all_of(y_var), time_var = all_of(time_var), everything(), -one_of(exclude_vector))

  x_vars <- colnames(data_reg)[-c(1:2)]

  res_temp <- list()

  for (v in x_vars) {
    formula <- as.formula(paste0("Surv(time_var,y_var) ~ ", v))

    fit <- coxph(formula, data_reg)
    res <- broom::tidy(fit, conf.int = TRUE) %>% filter(term != "(Intercept)")
    if (class(data_reg[, v]) %in% c("factor", "character")) {
      res$term <- paste0(gsub(v, "", res$term), "_vs_", fit$xlevels[[1]][1])
    }
    if (class(data_reg[, v]) %in% c("numeric", "double", "integer")) {
      res$term <- "Continuous"
    }

    res_temp[[v]] <- res
  }


  res <- bind_rows(res_temp, .id = "x_var") %>%
    filter(term != "(Intercept)") %>%
    select(
      "X Variables" = x_var, "Comparison" = term,
      "HR" = estimate, "CI95_low" = conf.low, "CI95_high" = conf.high,
      "P.Value" = p.value
    ) %>%
    mutate(
      multiv_graph = paste0(`X Variables`, "_", Comparison),
      HR = exp(HR), CI95_low = exp(CI95_low), CI95_high = exp(CI95_high)
    )


  res$Adj_P.Value <- p.adjust(res$P.Value, method = "fdr")
  res$HR <- round(res$HR, 2)
  res$CI95_low <- round(res$CI95_low, 2)
  res$CI95_high <- round(res$CI95_high, 2)

  res$P.Value <- format.pval(res$P.Value, 2)
  res$Adj_P.Value <- format.pval(res$Adj_P.Value, 2)


  return(res)
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

  data_reg <- data %>% select(all_of(y_var), everything(), -one_of(exclude_vector))


  res_list <- list()

  formula <- as.formula(paste0(y_var, " ~ ."))

  x_var <- colnames(data_reg)[which(colnames(data_reg) != y_var)]

  fit <- glm(formula, family = family, data_reg)
  res <- broom::tidy(fit, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(x_var = "", Comparison = "")


  for (var in x_var) {
    res$x_var[grep(var, res$term)] <- var
  }


  for (cat_var in names(fit$xlevels)) {
    res$Comparison[grep(cat_var, res$term)] <- gsub(cat_var, "", paste0(res$term[grep(cat_var, res$term)], "_vs_", fit$xlevels[[cat_var]][1]))
  }

  for (num_var in x_var[which(!x_var %in% names(fit$xlevels))]) {
    res$Comparison[grep(num_var, res$term)] <- "Continuous"
  }


  res <- res %>%
    filter(term != "(Intercept)") %>%
    select(
      "X Variables" = x_var, Comparison,
      "Beta Coeff." = estimate, "CI95_low" = conf.low, "CI95_high" = conf.high,
      "P.Value" = p.value
    ) %>%
    mutate(multiv_graph = paste0(`X Variables`, "_", Comparison))

  res$Adj_P.Value <- p.adjust(res$P.Value, method = "fdr")
  res$`Beta Coeff.` <- round(res$`Beta Coeff.`, 2)
  res$CI95_low <- round(res$CI95_low, 2)
  res$CI95_high <- round(res$CI95_high, 2)

  res$P.Value <- format.pval(res$P.Value, 3, eps = 0.001)
  res$Adj_P.Value <- format.pval(res$Adj_P.Value, 3, eps = 0.001)

  if (family == "binomial") {
    res$`Beta Coeff.` <- round(exp(res$`Beta Coeff.`), 2)
    res$CI95_low <- round(exp(res$CI95_low), 2)
    res$CI95_high <- round(exp(res$CI95_high), 2)

    colnames(res)[3] <- "Odds Ratio"
  }


  return(res)
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

  data_reg <- data %>% select(y_var = all_of(y_var), time_var = all_of(time_var), everything(), -one_of(exclude_vector))


  res_list <- list()

  x_var <- colnames(data_reg)[-c(1:2)]

  fit <- coxph(Surv(time_var, y_var) ~ ., data_reg)
  res <- broom::tidy(fit, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(x_var = "", Comparison = "")


  for (var in x_var) {
    res$x_var[grep(var, res$term)] <- var
  }


  for (cat_var in names(fit$xlevels)) {
    res$Comparison[grep(cat_var, res$term)] <- gsub(cat_var, "", paste0(res$term[grep(cat_var, res$term)], "_vs_", fit$xlevels[[cat_var]][1]))
  }

  for (num_var in x_var[which(!x_var %in% names(fit$xlevels))]) {
    res$Comparison[grep(num_var, res$term)] <- "Continuous"
  }


  res <- res %>%
    filter(term != "(Intercept)") %>%
    select(
      "X Variables" = x_var, Comparison,
      "HR" = estimate, "CI95_low" = conf.low, "CI95_high" = conf.high,
      "P.Value" = p.value
    ) %>%
    mutate(
      multiv_graph = paste0(`X Variables`, "_", Comparison),
      HR = exp(HR), CI95_low = exp(CI95_low), CI95_high = exp(CI95_high)
    )

  res$Adj_P.Value <- p.adjust(res$P.Value, method = "fdr")
  res$HR <- round(res$HR, 2)
  res$CI95_low <- round(res$CI95_low, 2)
  res$CI95_high <- round(res$CI95_high, 2)

  res$P.Value <- format.pval(res$P.Value, 2)
  res$Adj_P.Value <- format.pval(res$Adj_P.Value, 2)


  return(res)
}
