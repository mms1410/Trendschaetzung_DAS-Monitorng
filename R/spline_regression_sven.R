# source data
################################################################################
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
################################################################################
################################################################################
# plot for each index group (e.g. spreadsheet)
# function not used anymore
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
fit_models <- function(ts, span = 0.5, ar = 1, title = NULL) {
  ## set up params
  ts <- na.omit(ts)
  if (is.null(title)) {
    title <- names(ts)
  }
  ## set up models
  model_pspline <- gamlss::gamlss(coredata(ts) ~ ps(num_index(ts)))
  model_ar <- arima(ts, order = c(ar, 0, 0))
  model_loess <- loess(formula = coredata(ts) ~ num_index(ts),
                       span = span)
 list(model_pspline = model_pspline,
      model_ar = model_ar,
      model_loess = model_loess) 
}
model <- fit_models(ts_flat_1[[1]])



plot_splines <- function(ts, span = 0.5, ar = 1,
                         title = NULL,
                         x_lab = "Zeit",
                         y_lab = "Indexwert",
                         legend = "Legende") {
  ## set up params
  if (is.null(title)) {
    title <- names(ts)
  }
  idx_seq <- seq(from = min(index(ts)), to = max(index(ts)), by = "day")
  ## set up models
  model_pspline <- gamlss::gamlss(coredata(ts) ~ ps(as.numeric(index(ts))))
  model_loess <- loess(formula = coredata(ts) ~ as.numeric(index(ts)),
                       span = span)
  #model_loess <- gamlss::lo(formula = coredata(ts) ~ index(ts))
  model_arima <- forecast::Arima(ts, order = c(ar, 0, 0))
  
  ## plot fitted values
  ggplot() +
    geom_point(aes(x = index(ts), y = coredata(ts))) +
    geom_line(aes(x = index(ts), y = fitted(model_pspline),
                  color = "P-Spline")) +
    geom_point(aes(x = index(ts), y = fitted(model_pspline),
                   color = "P-Spline"), shape = 5) +
    geom_line(aes(x = index(ts), y = fitted(model_loess),
                  color = paste0("LOESS [span=", span, "]"))) +
    geom_point(aes(x = index(ts), y = fitted(model_loess),
              color = paste0("LOESS [span=", round(span,2), "]")), shape = 6) +
    geom_line(aes(x = index(ts), y = fitted(model_arima),
                  color = paste0("AR(", ar, ")"))) +
    geom_point(aes(x = index(ts), y = fitted(model_arima),
                   color = paste0("AR(", ar, ")")), shape = 2) +
    #geom_smooth(aes(x = index(ts), y = coredata(ts),
    #                color = "LOESS Regression"), se = FALSE) +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(title)+
    guides(colour = guide_legend(title = legend)) +
    default_theme
}
####
ts <- ts_flat_2[[7]]


p1 <- plot_splines(ts = ts,span = span, ar = ar, title = "")
p2 <- plot_splines(ts = log(ts), span = span, ar = ar, title = "",
                   y_lab = "Indexwert \n logarithmiert")

combo_plot(p1, p2, names(ts))

combo_plot <- function(p1, p2, title){
  p <- p1 / p2 & theme(legend.position = "right")
  p + plot_layout(guides = "collect") +
    plot_annotation(title) & default_theme
}