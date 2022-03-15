# set default values
span <- 0.75
ar <- 2
num_index <- function(ts) {
  as.numeric(index(ts)) - min(as.numeric(index(ts)))
}
default_theme <- theme_minimal() +
  theme(text = element_text(family = "Decima WE", size = 15)) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.3)) +
  theme(axis.line = element_line(color = "black", size = 0.4))