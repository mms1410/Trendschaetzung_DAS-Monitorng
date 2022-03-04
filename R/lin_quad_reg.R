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
plot_linreg <- function(ts, title = NULL) {
  ## params
  title <- names(ts)
  ts <- na.omit(ts)
  quad <- time(ts)
  model_lin <- lm(formula = coredata(ts) ~ as.numeric(index(ts)))
  model_quad <- lm(coredata(ts) ~ poly(as.numeric(index(ts)), 2, raw = TRUE))
  
  ggplot() +
    geom_point(aes(x = index(ts), y = coredata(ts))) +
    geom_line(aes(x = index(ts), y = fitted(model_lin),
                  colour = "Lineare Regression")) +
    geom_line(aes(x = index(ts), y = fitted(model_quad),
                  colour = "Quadratische Regression")) +
    xlab("Zeit") +
    ylab("Index") +
    ggtitle(title) +
    guides(colour = guide_legend(title = "Legende")) +
    default_theme
}
plot_linreg(ts_flat_4[[3]])
