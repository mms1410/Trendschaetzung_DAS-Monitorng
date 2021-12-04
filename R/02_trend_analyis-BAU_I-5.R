# 02_trend_analysis_BAU_I-5
##
library(ggplot)
library(trend)
library(mgcv)
library(forecast)
#library(lubridate)
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
model_gamm_ar1 <- gamm(formula = Jahr ~ s(Elementarschaeden), data = `BAU_I-5`,
                   correlation = corARMA(form = ~ Jahr, p = 1))
model_gamm_ar2 <- gamm(formula = Jahr ~ s(Elementarschaeden), data = `BAU_I-5`,
                       correlation = corARMA(form = ~ Jahr, p = 2)) # corrmat not invertible
acf(resid(model_gamm_ar1$lme))
