my_render_cat <- function(x, ...) {
  c("", sapply(stats.default(x, ...), function(y) {
    with(y, sprintf("%d (%s%%)", FREQ, ifelse(round(PCT / 0.5) * 0.5 %% 1 == 0,
      as.integer(round(PCT / 0.5) * 0.5),
      round(PCT / 0.5) * 0.5
    ))) # Round percentages to integers
  }))
}