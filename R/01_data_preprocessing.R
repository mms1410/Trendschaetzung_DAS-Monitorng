## Preliminaries
library(tidyverse)
library(readxl)
### below for other approach
#library(tidyxl)
#library(data.table)
## Read data and create data variables
path_data <- dirname(rstudioapi::getSourceEditorContext()$path)
path_data <- dirname(path_data) # folder down from 'R' folder
path_data <- paste0(path_data, .Platform$file.sep, "data") 
filenames <- list.files(path_data) # relative path
for (filename in filenames){
   #filename <- filenames[1]
  # create data reference
  # assumes data name of form '<alnum>_Daten<alnum>'
  # variable with name of <alnum> string preceding '_Daten' string will be created
  pattern <- pattern <- "(?:(?!_Daten).)*" # neg look ahead needed
  varname <- regmatches(filename, gregexpr(pattern = pattern,
                                                filename, perl = TRUE))[[1]][1]
  tmp_path <- paste0(path_data,.Platform$file.sep, filename)
  tmp_colnames <- read_excel(tmp_path,skip = 3, n_max = 1)
  tmp_colnames <- FALSE
  assign(varname, read_excel(tmp_path, # absolute path
                             na = c("NA", "<NA>"),
                             skip = 4,
                             col_types = "numeric",
                             col_names = tmp_colnames)
                             )
  #get(varname)
  ######################## tidyxl /data.table #################################
  # tmp_data <- as.data.table(xlsx_cells(tmp_path, sheets = 1))
  # tmp_data <- tmp_data[row != 1]
  # # construct colnames
  # # if name in col 1 then no column but meta info
  # tmp_skip_rows <- tmp_data[col == 1 & data_type == "character"]$row
  # tmp_data <-tmp_data[tmp_data$row > tmp_skip_rows]
  # # get colnames
  # tmp_colnames <- tmp_data[!is.na(character),c("row", "col", "character")]
  # tmp_colnames <- tmp_colnames[order(col, row)]
  # tmp_colnames <- tmp_colnames[,c("col", "character")]
  # tmp_cols <- unique(tmp_colnames$col)
  # # merge chars for each col
  # tmp_colnames
  # tmp_colnames <- dcast(tmp_colnames, col ~ character, value.var = "character")
  # tmp_colnames <- unite(tmp_colnames[,-1], "name", na.rm = TRUE)
  # # create association row and year (local_format_id == 19)
  # # format id:
  # # 19, 20
  # tmp_row_year <- tmp_data[tmp_data$local_format_id == 19 | tmp_data$local_format_id == 20, .(row, numeric)]
  # colnames(tmp_row_year) <- c("row", "year")
  # tmp_data <- tmp_row_year[tmp_data, on = "row"]
  # tmp_data <- tmp_data[!is.na(year) & col != 1] # headers do not have a year column
  # # add column names for wide format
  # tmp_col_name <- cbind(col = unique(tmp_data$col), tmp_colnames)
  # tmp_data <- tmp_col_name[tmp_data, on = "col"]
  # dcast(tmp_data, formula =  year ~ name, value.var = numeric)
}
