## Preliminaries
library(readxl)
library(checkmate)
## Read data and create data variables
folderpath <- "~/03_data/06_Trendschaetzung_DAS-Monitoring/" # TO CHANGE #
filenames <- list.files(folderpath) # relative path
for (filename in filenames){
  assertCharacter(filename) # ToDo: check pattern ?
  # create data reference
  # assumes data name of form '<alnum>_Daten<alnum>'
  # variable with name of <alnum> string preceding '_Daten' string will be created
  pattern <- pattern <- "(?:(?!_Daten).)*" # neg look ahead nedded
  varname <- regmatches(filename, gregexpr(pattern = pattern,
                                                filename, perl = TRUE))[[1]][1]
  assign(varname, read_excel(paste0(folderpath, filename))) # absolute path
  # ToDo:
  # clean data
}


