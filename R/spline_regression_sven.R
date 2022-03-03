library(xts)
library(data.table)
library(ggplot2)
library(checkmate)
# source data
################################################################################
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
################################################################################
library(splines)
################################################################################
# plot for each index group (e.g. spreadsheet)
make_plot <- function(ts, data_name) {
  # better names ? 
  cnames <- colnames(ts)
  ## remove double whitespace
  cnames <- gsub("^ *|(?<= ) | *$", "", cnames, perl = TRUE) 
  colnames(ts) <- cnames
  ## common elements
  title <- Reduce(intersect, strsplit(cnames," "))
  if (length(title) > 1 ){
    short_names <- strsplit(cnames, " ")
    ## not in common elements
    short_names <- unlist(
      lapply(X = short_names, FUN = function(name) {
        out <- paste0(name[!name %in% title], collapse = " ")
        ## check for "(" <something> ")" pattern
        if (grepl(pattern = "^\\(.*\\)$", x = out)) {
          substring(out, 2, nchar(out)-1)
        } else {
          out 
        }
      })
    )
    title <- paste0(title, collapse = " ")
    colnames(ts) <- short_names
  } else {
    short_names <- colnames(ts)
    title <- data_name
  }
  
  #
  splineNfit <- function(ts, N) {
    # assert xts
    out <- lm(coredata(ts) ~ bs(index(ts), df = N))$fitted.values
    xts(x = out, order.by = as.Date(names(out)))
  }
  ## create time series of fitted values
  bs3_fit <- apply(X = ts, MARGIN = 2, FUN = function(x) {splineNfit(x, 3)}) 
  if ("matrix" %in% class(bs3_fit)) { # multiple classes possible
    bs3_fit <- xts(x = bs3_fit, order.by = index(ts))
  } else if ("list" %in% class(bs3_fit)){
    ## merge time series into one multiple time series
    bs3_fit <- do.call(xts::merge.xts, bs3_fit) #  make xts series
  }
  colnames(bs3_fit) <- paste0("Spline_3_", colnames(ts))
  assert(length(ts) == length(bs3_fit))
  ts_all <- do.call(xts::merge.xts, list(ts, bs3_fit)) #  replaces whitespace with "."
  colnames(ts_all) <- gsub(x = colnames(ts_all), pattern = "\\.", replacement = " ")
  ## setup data table for ggplot
  dtbl <- as.data.table(ts_all)
  variable_names <- colnames(dtbl)
  variable_names <- variable_names[variable_names != "index"]
  ##  number of models + 1
  n_models <- 2
  ## ggplot needs data in long format and not typical wide format
  ts_long <- melt(data = dtbl, id.vars = "index", value.name = "val",
                  variable.name = "var")
  ## assign colors to variables is ts_long
  ggcolors <- scales::hue_pal()(ncol(ts))
  ggcolors_original <- ggcolors
  ggcolors <- rep(ggcolors, 2) # first original data point, second spline fit
  names(ggcolors) <- variable_names
  names(ggcolors_original) <- short_names
  ## create ggplot
  ggplot() +
    geom_line(data = ts_long[grepl(pattern = "^Spline_[[:alnum:]]*", var,
                                   perl = TRUE)],
              aes(x = index, y = val, color = var)) +
    geom_point(data = ts_long[!ts_long[grepl(pattern = "^Spline_[[:alnum:]]*", var,
                                             perl = TRUE)], on = c("index", "var")],
               aes(x = index, y = val, color = var)) +
    scale_color_manual(values = ggcolors, labels = names(ggcolors)) +
    guides(color = guide_legend(nrow = length(short_names),
                                title = "Daten")) +
    xlab("Jahr") +
    ylab(title) +
    ggtitle(title)
}
####
# test
####
n <- 51
make_plot(ts_list[[n]], names(ts_list[[n]]))
# 3, 4, 5, 6, 8, 11, 12, 13, 20, 24, 26, 29, 34, 35, 36, 44, 50, 51
################################################################################
