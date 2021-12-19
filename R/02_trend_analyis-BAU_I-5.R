# 02_trend_analysis_BAU_I-5
##
library(ggplot2)
library(xts)
library(mfilter)
##
##
colnames(`BAU_I-5`)
colnames(`BAU_I-5`)[c(3,4)] <- c("Sturm_und_Hagel", "Elementarschaeden")
## 
ggplot(data = `BAU_I-5`, aes(x = Jahr)) +
  geom_point(mapping = aes(y = Elementarschaeden,
            color = "Elementarschäden"), shape = 15) +
  geom_line(mapping = aes(y = Elementarschaeden,
            color = "Elementarschäden")) +
  geom_smooth(mapping = aes(y = Elementarschaeden,
            color = "Elementarschäden"),
            method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(mapping = aes(y = Sturm_und_Hagel,
            color = "Sturm und Hagel"), shape = 10) +
geom_line(mapping = aes(y = Sturm_und_Hagel,
            color = "Sturm und Hagel")) +
geom_smooth(mapping = aes(y = Sturm_und_Hagel  ,
            color = "Sturm und Hagel"),
            method = "lm", se = FALSE,
            linetype = "dashed") +
  ggtitle("Schadenaufwand in der Sachversicherung") +
  labs(x = "Jahr", y = "Schaden [Mrd.€]", color = "Schaden") +
  gg_theme
##
model_gam_ar1 <- gam(formula = Jahr ~ s(Elementarschaeden), data = `BAU_I-5`,
                   correlation = corARMA(form = ~ Jahr, p = 2))

acf(residuals(model_gam_ar1))
plot(model_gam_ar1)

ts_xts <- xts(cbind(`BAU_I-5`$Sturm_und_Hagel, `BAU_I-5`$Elementarschaeden), order.by = as.Date(ISOdate(`BAU_I-5`$Jahr, 1, 1)))
colnames(ts) <- c("Sturm und Hagel", "Elementarschaeden")
for (series in ts){
  print(series)
  print(colnames(series))
}

outlier_hagel <- forecast::tsoutliers(ts_hagel)
outlier_hagel

ts_hagel
ts_elements
ts_list <- list("Sturm und Hagel" = ts_hagel, "Elementarschaeden" = ts_elements)
