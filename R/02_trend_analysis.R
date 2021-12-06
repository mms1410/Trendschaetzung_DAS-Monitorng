# 0. Preliminaries
## load packages
library(ggplot2)
library(reshape2)
library(xts)
## load data
### WARNINGS are created due to handling of column names in source code
### because there is no obvious way internally in the package function
### new names are created by <columnnumber> preceded by '...' 
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
# 1. generic plot function(s)
make_ts_plot <- function(ts_xts, names_axis, name_legend, name_title, var_names = NULL){
  
  # convert time series in tall format
  tall_format <- as.data.table(ts_xts)
  tall_format <- reshape2::melt(tall_format, id.vars = "index") # index exists since xts object
  # plot data
  ggplot(tall_format, aes(index, value, col = variable)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, linetype = "solid") +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    ggtitle(name_title) +
    labs(x = names_axis[["x"]], y = names_axis[["y"]], color = name_legend) +
    theme_minimal() +
    theme(text = element_text(size = 16, family = "Bookman"))
  # ToDo:
  # legend for dashed and solid
  # reshape2 exchangeable with dcast/melt from data.table?
}

## test here:
#colnames(`BAU_I-5`)
#colnames(`BAU_I-5`)[c(3,4)] <- c("Sturm_und_Hagel", "Elementarschaeden")
#ts_xts <- xts(cbind(`BAU_I-5`$Sturm_und_Hagel, `BAU_I-5`$Elementarschaeden), order.by = as.Date(ISOdate(`BAU_I-5`$Jahr, 1, 1)))
#colnames(ts_xts) <- c("Sturm und Hagel", "Elementarschaeden")
#make_ts_plot(ts_xts,
#             names_axis = c("x" = "Jahr", "y" = "Schaden in Mrd. €"),
#             name_legend = "Schaden",
#             name_title = "Schadenaufwand in der Sachversicherung")


