# 0. Preliminaries
## load packages
library(ggplot2)
library(ggpubr)
library(zoo)
library(rugarch)
library(MASS)
## load data
### WARNINGS are created due to handling of column names in source code
### because there is no obvious way internally in the package function
### new names are created by <columnnumber> preceded by '...' 
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
# 1. trend analysis

