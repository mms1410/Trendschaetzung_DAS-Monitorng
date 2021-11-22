## Preliminaries
library(tidyverse)
## Read data and create data variables
path_data <- dirname(rstudioapi::getSourceEditorContext()$path)
path_data <- dirname(path_data) # folder down from 'R' folder
path_data <- paste0(path_data, .Platform$file.sep, "data") 
filenames <- list.files(path_data) # relative path
for (filename in filenames){
  # create data reference
  # assumes data name of form '<alnum>_Daten<alnum>'
  # variable with name of <alnum> string preceding '_Daten' string will be created
  pattern <- pattern <- "(?:(?!_Daten).)*" # neg look ahead needed
  varname <- regmatches(filename, gregexpr(pattern = pattern,
                                                filename, perl = TRUE))[[1]][1]
  tmp_path <- paste0(path_data,.Platform$file.sep, filename)
  #tmp_colnames <- read_excel(tmp_path,
  #                           na = c("NA", "<NA>"),
  #                           skip = 3,
  #                           n_max = 1)
  #tmp_colnames <- c("count","Jahr", colnames(tmp_colnames))
  tmp_colnames <- FALSE
  assign(varname, read_excel(tmp_path, # absolute path
                             na = c("NA", "<NA>"),
                             skip = 4,
                             col_types = "numeric",
                             col_names = tmp_colnames)
                             )
  get(varname)
}
