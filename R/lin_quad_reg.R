##
library(ggplot2)
################################################################################
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "defaults_sven.R"))
################################################################################
plot_linreg <- function(ts, title = NULL, y_lab = "Indexwert", x_lab = "Zeit",
                        legend = "Legende") {
  ts <- na.omit(ts)
  quad <- time(ts)
  model_lin <- lm(formula = coredata(ts) ~ as.numeric(index(ts)))
  model_quad <- lm(coredata(ts) ~ poly(as.numeric(index(ts)), 2, raw = TRUE))
  
  p <- ggplot() +
    geom_point(aes(x = index(ts), y = coredata(ts))) +
    geom_line(aes(x = index(ts), y = fitted(model_lin),
                  colour = "Lineare Regression")) +
    geom_line(aes(x = index(ts), y = fitted(model_quad),
                  colour = "Quadratische Regression")) +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(title) +
    guides(colour = guide_legend(title = legend)) +
    default_theme
  p
}

ts <- ts_flat_4[[7]]

p1 <- plot_linreg(ts = ts, title = "")
p2 <- plot_linreg(ts = log(ts + 1), title = "", y_lab = "Indexwert \n logarithmiert")

combo_plot(p1, p2, names(ts))
