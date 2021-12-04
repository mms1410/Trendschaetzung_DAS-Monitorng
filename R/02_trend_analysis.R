# 0. Preliminaries
## load packages
library(ggplot2)
library(ggpubr)
library(zoo)
library(rugarch)
library(MASS)
library(mgcv)
## load data
### WARNINGS are created due to handling of column names in source code
### because there is no obvious way internally in the package function
### new names are created by <columnnumber> preceded by '...' 
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
gg_theme <-ggplot2::theme_minimal() +
           ggplot2::theme(text = element_text(size = 16,
                                              family = "Bookman"))

