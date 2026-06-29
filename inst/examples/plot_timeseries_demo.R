# Demo: plot_timeseries() — any per-frame variable against time.
#
# Run interactively. Each plot is printed and a combined PNG is written to a
# temp file at the end. plot_timeseries() works on any numeric column of an
# aniframe (speed, acceleration, a temperature channel, ...); the variable is
# always named explicitly.

set.seed(42)

library(anivis)
library(aniframe)
library(ggplot2)
library(patchwork)


# Helper: add a per-group `speed` column (step distance between frames).
add_speed <- function(df, by) {
  parts <- split(df, df[by], drop = TRUE)
  parts <- lapply(parts, function(d) {
    d <- d[order(d$time), , drop = FALSE]
    d$speed <- c(0, sqrt(diff(d$x)^2 + diff(d$y)^2))
    d
  })
  do.call(rbind, parts)
}


# -------------------------------------------------------------------------
# 1. Single trajectory: speed over time (one line, no legend)
# -------------------------------------------------------------------------
single <- data.frame(
  time = 1:120,
  x = cumsum(rnorm(120)),
  y = cumsum(rnorm(120))
)
single$speed <- c(0, sqrt(diff(single$x)^2 + diff(single$y)^2))
single <- as_aniframe(single)

p1 <- plot_timeseries(single, variable = "speed") +
  ggtitle("1. Single trajectory speed")
print(p1)


# -------------------------------------------------------------------------
# 2. Multi-keypoint, inline: overlaid coloured lines + legend
# -------------------------------------------------------------------------
multi_kp <- data.frame(
  keypoint = rep(c("head", "thorax", "abdomen"), each = 120),
  time = rep(1:120, 3),
  x = c(cumsum(rnorm(120)), cumsum(rnorm(120)) + 5, cumsum(rnorm(120)) - 5),
  y = c(cumsum(rnorm(120)), cumsum(rnorm(120)) + 5, cumsum(rnorm(120)) - 5)
)
multi_kp <- as_aniframe(add_speed(multi_kp, "keypoint"))

p2 <- plot_timeseries(multi_kp, variable = "speed") +
  ggtitle("2. Multi-keypoint (inline)")
print(p2)


# -------------------------------------------------------------------------
# 3. Same data, layout = "facet": one panel per keypoint
# -------------------------------------------------------------------------
p3 <- plot_timeseries(multi_kp, variable = "speed", layout = "facet") +
  ggtitle("3. Multi-keypoint (facet)")
print(p3)


# -------------------------------------------------------------------------
# 4. Matrix: 2 individuals x 3 trials, faceted (facet_grid)
# -------------------------------------------------------------------------
matrix_df <- expand.grid(
  time = 1:120,
  trial = 1:3,
  individual = c("a", "b")
)
matrix_df$x <- ave(
  rnorm(nrow(matrix_df)),
  matrix_df$individual,
  matrix_df$trial,
  FUN = cumsum
)
matrix_df$y <- ave(
  rnorm(nrow(matrix_df)),
  matrix_df$individual,
  matrix_df$trial,
  FUN = cumsum
)
matrix_df <- add_speed(matrix_df, c("individual", "trial"))
matrix_af <- as_aniframe(matrix_df, variables_when = c("trial", "time"))

p4 <- plot_timeseries(matrix_af, variable = "speed", layout = "facet") +
  ggtitle("4. Individual x trial (facet_grid)")
print(p4)


# -------------------------------------------------------------------------
# 5. Dark mode
# -------------------------------------------------------------------------
p5 <- plot_timeseries(multi_kp, variable = "speed", mode = "dark") +
  ggtitle("5. Multi-keypoint, dark mode")
print(p5)


# -------------------------------------------------------------------------
# 6. Any variable: a non-movement channel (e.g. body temperature)
# -------------------------------------------------------------------------
temp_df <- data.frame(
  time = 1:120,
  x = cumsum(rnorm(120)),
  y = cumsum(rnorm(120)),
  temperature = 37 + cumsum(rnorm(120, 0, 0.05))
)
temp_af <- as_aniframe(temp_df)

p6 <- plot_timeseries(temp_af, variable = "temperature") +
  ggtitle("6. Any variable (temperature)")
print(p6)


# -------------------------------------------------------------------------
# Combined PNG via anivis::plots()
# -------------------------------------------------------------------------
combined <- plots(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  n_columns = 3,
  tags = TRUE,
  title = "anivis::plot_timeseries demo"
)

out <- file.path(tempdir(), "anivis_plot_timeseries_demo.png")
ggsave(out, combined, width = 15, height = 8, dpi = 150)
message("Saved combined demo to: ", out)
