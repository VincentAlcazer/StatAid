
#' Global parameters
#'
#' @import ggplot2
#' @import ggrepel
#' @import ggpubr
#' @import tidyr
#' @import tibble
#' @import forcats
#' @import RColorBrewer
#' @import broom
#' @import survival
#' @import survminer
#' @import mgcv
#' @import plotROC
#' @import readxl

library(ggplot2)

########## ========== Datasets

data_aml <- read.delim("datasets/TCGA_AML_exemple.txt", dec = ",", na.strings = c("", "NA"), stringsAsFactors = T)


########## ========== Parameters

### Ggplot 2 default theme

default_theme <- theme_bw() + theme(
  plot.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 12, color = "black"),
  axis.title = element_text(size = 14, face = "bold"),
  legend.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12),
  legend.position = "none"
)
