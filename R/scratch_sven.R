# # WW-I-10
# tmp_data <- `WW-I-10`$`Cuxhaven (Nordsee) cm`
# t <- as.numeric(index(tmp_data))
# k <- floor(length(t) / 3)
# model_gamm_WW_AR1 <- mgcv::gamm(formula = coredata(tmp_data) ~ s(t, k = k, bs = "cs"),
#                                 correlation = corAR1())
# 
# 
# # Sturm und Hagel
# model_gamm_SuH <- gamlss::fitDist(ts_xts$`Sturm und Hagel`, type = "realline")
# model_gamm_SuH_AR1 <- ""
# 
# t <- as.numeric(index(ts_xts$`Sturm und Hagel`))
# k <- floor(length(t) / 3)
# model_gamm_AR1 <- mgcv::gamm(formula = coredata(ts_xts$`Sturm und Hagel`) ~ s(t, k = k, bs = "cs" ),
#                              correlation = corAR1())
# 
# title_gamm_SuH <- paste0(model_gamm_SuH$family, ", ")
# trend_gamm_SuH <- ts_xts$`Sturm und Hagel` - model_gamm_SuH$residuals
# res_gamm_SuH <- model_gamm_SuH$residuals
# model_lm_SuH <- lm(formula = ts_xts$`Sturm und Hagel` ~ index(ts_xts))
# trend_lm_SuH <- predict(model_lm)
# res_lm_SuH <- model_lm_SuH$residuals
# 
# # Elementarschaeden
# model_gamm_ES <- gamlss::fitDist(ts_xts$Elementarschaeden, type = "realline")
# title_gamm_ES <- paste0(model_gamm_ES$family, ", ")
# trend_gamm_ES <- ts_xts$Elementarschaeden - model_gamm_ES$residuals
# res_gamm_ES <- model_gamm_ES$residuals
# model_lm_ES <- lm(formula = ts_xts$Elementarschaeden ~ index(ts_xts))
# trend_lm_ES <- predict(model_lm_ES)
# 
# ggplot(data = ts_xts$`Sturm und Hagel`) +
#   geom_point(aes(x = idx, y = ts_xts$`Sturm und Hagel`)) +
#   geom_line(aes(x = idx, y = trend_lm_SuH, color = "linearer Trend")) +
#   geom_smooth(aes(x = idx, y = predict(model_gamm_SuH), color = "GAMM"))
# 
# ggplot(data = ts_xts$Elementarschaeden) +
#   geom_point(aes(x = idx, y = ts_xts$Elementarschaeden)) +
#   geom_line(aes(x = idx, y = trend_lm_ES, color = "linearer Trend")) +
#   geom_smooth(aes())
################################################################################
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_data_preprocessing.R"))
################################################################################

dat <- xts(x = `WW-I-10`[,3:ncol(`WW-I-10`)],
           order.by = as.Date(ISOdate(`WW-I-10`$Jahr, 1, 1)))

library(mgcViz)
colname <- "Cuxhaven (Nordsee) cm"
for(colname in colnames(dat)){
  ts <- subset(dat, ,colname)
  t <- as.numeric(index(ts))
  #t <- t + abs(min(t))
  k <- floor(nrow(ts) / 3)
  model_gamm_AR1 <- mgcv::gamm(formula = coredata(ts) ~ s(t, k = k, bs = "cs"),
                              correlation = corAR1())
  model_gamm_AR1_fitted <- fitted(model_gam_AR1$lme)
  model_gamm_AR1$gam$data <- cbind(x = as.numeric(index(ts)),coredata(ts))
  viz <- mgcViz::getViz(model_gamm_AR1$gam)
  model_lm <- lm(formula = coredata(ts) ~ t)
  model_evgam <- ecvgam::evgam()
  
  model_gamm__auto <- gamlss::fitDist(y = ts, type = "realplus")
  model_gamm__auto
  histDist(ts, family = model_gamm__auto$family[1], density = TRUE,
           line.col =c(1,1), line.ty = c(1,2))^
  plot(histDist(ts, family = model_gamm__auto$family[1], density = TRUE,
                line.col =c(1,1), line.ty = c(1,2)))
  wp(model_gamm__auto)
  
  m1 <- gamlss(ts~1, data = ts, family = model_gamm__auto$family[1], trace = FALSE)
  summary(m1)
  cat(paste0("The fitted distribution for the response variable is: ",
             "Y_{i}$ ~ ", model_gamm__auto$family[1], "("))
  
  model_gamm__auto$coef.gamlss
  model_gamm__auto$fits
  
  
  ggplot(data = ts, aes(x = t)) +
    geom_point(aes(y = coredata(ts)), alpha = 0.5, colour = "red", shape = 7) +
    geom_line(aes(y = fitted(model_lm), color = "Linear Regression")) +
    geom_point(aes(y = model_gamm_AR1$gam$fitted.values, colour = "GAM"), alpha = 0.9, shape = 25) +
    theme_minimal() +
    theme(text = element_text(size = 16, family = "Bookman")) +
    labs(x = "Jahr [numeric]", y = colname, color = "")
    
  plot(viz, allTerms = TRUE) +
    l_fitLine(linetype = 3)
  
}
################################################################################
#
# Man Kendall Test applied on list of (multivariate) time series
#
################################################################################
library(Kendall)
## for each multivariate time series ...
mk_test <- function(mult_ts){
  mk_list <- list()
  for( ts_idx in 1:ncol(mult_ts)){
    mk_list[[colnames(mult_ts[, ts_idx])]] = Kendall::MannKendall(mult_ts[, ts_idx])
  }
  mk_list
}
##  for each element in list of multivariate time series ...
list_mk_test <- function(ts_list){
  ml_list <- list()
  for( idx in 1:length(ts_list)){ # for( (idx, object) in list) ??
    #print(idx)
    ml_list[[names(ts_list)[idx]]] = mk_test(ts_list[[idx]])
  }
  ml_list
}
list_out <- list_mk_test(ts_list)
###############################################################################
#
# Ljung Box Test H_{0}: independent
#
###############################################################################
lb_test <- function(mult_ts){
  lb_list <- list()
  for( ts_idx in 1:ncol(mult_ts)){
    ts_list[[colnames(mult_ts[, ts_idx])]] = Box.test(mult_ts[, ts_idx], lag = 3, type = "Ljung-Box")
  }
  lb_list
}
list_lb_test <- function(ts_list){
  lb_list <- list()
  for( idx in 1:length(ts_list)){
    lb_list[[name(ts_list)[idx]]] = lb_test(ts_list[[idx]])
  }
  lb_list
}

################################################################################
#
# Desc. plots
#
################################################################################
library(ggplot)
plot_ts <- function(mult_ts){
  ggplot( data = mult_ts) +
    geom_point(aes(x = index(mult_ts), y = value, colour = variable, group = variable))
}