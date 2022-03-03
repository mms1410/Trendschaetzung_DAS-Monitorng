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
default_theme <- theme_minimal() +
  theme(text = element_text(family = "Bookman", size = 15)) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.3)) +
  theme(axis.line = element_line(colour = "black", size = 0.4))


################################################################################
#TODO:
# Kommas einleisen ?
################################################################################
library(splines)
#library(stringr)
#
data_name <- names(ts_list)[[47]]
ts <- ts_list[[47]]
# better names ? 
cnames <- colnames(ts)
## remove double whitespace
## TODO: if only ONE time series intersection is whole column name
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
## assign colors to variables in ts_long
ggcolors <- scales::hue_pal()(ncol(ts))
ggcolors_original <- ggcolors
ggcolors <- rep(ggcolors, n_models) # first original data point, second spline fit
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
  ggtitle(title) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#
# ts_flat <- flatten_ts_list(ts_list)
short_long_filter_list <- function(ts_flat, n_short) {
  #
  #
  #
  idx <- lapply(X = ts_flat, nrow) 
  idx <- unlist(unname(idx))
  ts_long <- ts_flat[idx > n_short]
  ts_short <- ts_flat[idx <= n_short]
  list("ts_long" = ts_long,
       "ts_short" = ts_short)
}
short_long_ts <- short_long_filter_list(ts_flat, 16)
ts_short <- short_long_ts[["ts_short"]]
ts_long <- short_long_ts[["ts_long"]]
################################################################################
ts <- ts_flat[[7]]

# create spline objects
pspline::
# 15 März Sevag -> 
# 14  

model_autoarima <- forecast::
ggplot(data = ts) +
  geom_point(mapping = aes(x = index(ts), y = coredata(ts))) +
  geom_spline(mapping = aes(x = index(ts),
                            y = coredata(ts)))

####




x <- fortify.zoo(ts)

base::plot(ts)
ts.spl <- smooth.spline(x = index(ts), y = coredata(ts))
lines(ts.spl, col = "blue")

ggplot(data = fortify.zoo(ts)) +
  geom_point(mapping = aes(x = index(ts), y = coredata(ts))) +
  geom_smooth(method = lm,
              formula = coredata(ts) ~ splines::bs(index(ts), 3),
              se = FALSE)



dtbl <- as.data.table(ts)
dtbl[, !"index", with = FALSE]
colnames(dtbl)[2]

library(ggformula)
dtbl[,1]
ggplot(data = dtbl) +
  geom_point(mapping = aes(x = dtbl[, "index", with = FALSE],
                           y = dtbl[, !"index", with = FALSE]))


fit_polyspline <- lm(coredata(ts) ~ poly(index(ts)))
fit_sspline <- smooth.spline(coredata(ts) ~ poly(index(ts)))
mgcv::gam(coredata(ts) ~ s(index(ts), bs = "ps", ))
################################################################################
## time series splits
tp <- lapply(X = ts_flat, FUN = length)
tp <- unlist(tp)
tp <- sort(tp)

quantile(tp)[2]
quantile(tp)[3]
quantile(tp)[4]

ggplot() +
  stat_ecdf(aes(unname(tp)),geom = "step") +
  geom_vline(xintercept = quantile(tp)[2], color = "red", linetype = 2) +
  geom_text(aes(x = quantile(tp)[2]),
            label = paste("0.25 Quantil (", quantile(tp)[2], ")",
                          collapse = "",
                          sep = ""),
            y = 0.6,
            angle = 90, vjust = -1, color = "red") +
  geom_vline(xintercept = quantile(tp)[3], color = "red", linetype = 2) +
  geom_text(aes(x = quantile(tp)[3]),
            label = paste("0.5 Quantil (", quantile(tp)[3], ")",
            collapse = "",
            sep = ""),
            y = 0.6,
              angle = 90, vjust = -1, color = "red") +
  geom_vline(xintercept = quantile(tp)[4], color = "red", linetype = 2) +
  geom_text(aes(x = quantile(tp)[4]),
            label = paste("0.75 Quantil (", quantile(tp)[4], ")",
                          collapse = "",
                          sep = ""),
            y = 0.6,
            angle = 90, vjust = -1, color = "red") +
  ggtitle(label = "Empirische Verteilngsfunktion der Zeitreihenlänge aller Indizes") +
  xlab("Zeitreihenlänge") +
  ylab("Kummulierte rel. Häufigkeit") +
  defaut_theme



ts_flat_1 <- Filter(function(ts) {
  length(ts) <= quantile(tp)[1]
  }, ts_flat)
ts_flat_2 <- Filter(function(ts) {
  (length(ts) > quantile(tp)[1]) && (length(ts) <= quantile(tp)[2])
  }, ts_flat)
ts_flat_3 <- Filter(function(ts) {
  (length(ts) > quantile(tp)[2]) && (length(ts) <= quantile(tp)[3])
  }, ts_flat)
ts_flat_4 <- Filter(function(ts) {
  length(ts) > quantile(tp)[3]
  }, ts_flat)
################################################################################

fit <- gamlss::fitDist(ts, type = "realplus")
fit$failed
fit$family[[1]]
m1 <- gamlss::gamlssML(ts, family = fit$family[[1]])
chooseDist(m1, type = "realplus")

m1 <- gamlss::gamlss(coredata(ts) ~ pb(as.numeric(index(ts))), family = fit$family[[1]])
summary(m1)
plot(m1)
wp(m1)
plot(coredata(ts) ~ index(ts))
lines(fitted(m1) ~ index(ts))

min(unlist(lapply(X = ts_flat_4, FUN = function(ts) {
  length(ts)
})))


model <- mgcv::gam(coredata(ts) ~ s(as.numeric(index(ts))))

df <- 1
title <- names(ts)
qq <- quantile(as.numeric(index(ts)), seq(0,1, length.out = 10))
model_smooth <- smooth.spline(coredata(ts) ~ as.numeric(index(ts)),
                              cv = TRUE)
model_cubic <- lm(formula = coredata(ts) ~ poly(as.numeric(ts),
                                                degree = 3,
                                                raw = 3))
model_bspline <- lm(formula = coredata(ts) ~ bs(as.numeric(index(ts))))
model_cc <- mgcv::gamm(formula = coredata(ts) ~ s(as.numeric(index(ts)),
                                                  bs = "cc",
                                                  k = qq
                                                  ))

ggplot() +
  geom_point(aes(x = index(ts), y = coredata(ts))) +
  geom_line(aes(x = index(ts),
                y = predict(model_smooth, as.numeric(index(ts)))$y,
                color = "smoothing spline")) +
  geom_line(aes(x = index(ts),
                y = predict(model_cubic, x = as.numeric(index(ts))),
                color = "cubic spline")) +
  geom_line(aes(x = index(ts),
                y = predict(model_bspline, x = as.numeric(index(ts))),
                color = "B-Spline")) +
  xlab("Zeit") +
  ylab("Index") +
  guides(color = guide_legend(title = "Legende")) +
  ggtitle(title)+
  
  default_theme

################################################################################
library(fpp2)
model_ar1 <- Arima(ts, order = c(1, 0, 0))
forecast(ts, h = length(ts)) %>% 
  autoplot()

library(anomaly)
anomaly <- capa.uv(ts)
plot(anomaly)
anomaly