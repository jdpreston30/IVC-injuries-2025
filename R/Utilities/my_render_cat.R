my_render_cat <- function(x, ...) {
  c("", sapply(stats.default(x, ...), function(y) {
    with(y, sprintf("%d (%d%%)", FREQ, floor(PCT + 0.5)))
  }))
}
