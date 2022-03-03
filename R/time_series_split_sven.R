library(ggplot2)
################################################################################
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
  ggtitle(label = "Empirische Verteilungsfunktion der Zeitreihenlänge aller Indizes") +
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