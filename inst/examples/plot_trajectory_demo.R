# Demo: plot_trajectory() colour scheme & start/end markers
#
# Run this script interactively (Rstudio / Positron / VS Code R) to inspect
# the trajectory plots for each combination of variables_what and
# variables_when. The script prints each plot and finally writes a combined
# PNG to a temp file so you can share it.
#
# Reproducible random data: change the seed to get a different sample.

set.seed(42)

library(anivis)
library(aniframe)
library(ggplot2)
library(patchwork)


# -------------------------------------------------------------------------
# 1. Single trajectory (no `what`, default `when` = time only)
# -------------------------------------------------------------------------
single <- data.frame(
  time = 1:100,
  x = cumsum(rnorm(100)),
  y = cumsum(rnorm(100))
) |>
  as_aniframe()

p1 <- plot_trajectory(single) + ggtitle("1. Single trajectory")
print(p1)


# -------------------------------------------------------------------------
# 2. Multi-keypoint (`what` has 2+ levels, `when` is just time)
# -------------------------------------------------------------------------
multi_kp <- data.frame(
  keypoint = rep(c("head", "thorax", "abdomen"), each = 100),
  time = rep(1:100, 3),
  x = c(cumsum(rnorm(100)), cumsum(rnorm(100)) + 5, cumsum(rnorm(100)) - 5),
  y = c(cumsum(rnorm(100)), cumsum(rnorm(100)) + 5, cumsum(rnorm(100)) - 5)
) |>
  as_aniframe()

p2 <- plot_trajectory(multi_kp) + ggtitle("2. Multi-keypoint (hue per `what`)")
print(p2)


# -------------------------------------------------------------------------
# 3. Multi-trial (`when` has 2+ non-time levels, `what` is the default centroid)
# -------------------------------------------------------------------------
multi_trial <- data.frame(
  trial = rep(1:4, each = 100),
  time = rep(1:100, 4),
  x = unlist(lapply(1:4, function(i) cumsum(rnorm(100)) + i * 2)),
  y = unlist(lapply(1:4, function(i) cumsum(rnorm(100)) + i * 2))
) |>
  as_aniframe(variables_when = c("trial", "time"))

p3 <- plot_trajectory(multi_trial) +
  ggtitle("3. Multi-trial (gradient per `when`)")
print(p3)


# -------------------------------------------------------------------------
# 4. Matrix: 3 individuals x 4 trials (hue x shade)
# -------------------------------------------------------------------------
inds <- 3
trials <- 4
n_per <- 100
matrix_df <- expand.grid(
  time = 1:n_per,
  trial = seq_len(trials),
  individual = letters[seq_len(inds)]
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

matrix_af <- as_aniframe(matrix_df, variables_when = c("trial", "time"))

p4 <- plot_trajectory(matrix_af) +
  ggtitle("4. Matrix: 3 individuals (hue) x 4 trials (shade)")
print(p4)


# -------------------------------------------------------------------------
# 5. Dark mode
# -------------------------------------------------------------------------
p5 <- plot_trajectory(matrix_af, mode = "dark") +
  ggtitle("5. Same matrix, dark mode")
print(p5)


# -------------------------------------------------------------------------
# 6. Alternative qualitative palette
# -------------------------------------------------------------------------
p6 <- plot_trajectory(matrix_af, palette = "Set 2") +
  ggtitle("6. Same matrix, palette = 'Set 2'")
print(p6)


# -------------------------------------------------------------------------
# Save a combined overview PNG to a temp file
# -------------------------------------------------------------------------
combined <- (p1 | p2 | p3) /
  (p4 | p5 | p6) +
  plot_annotation(title = "anivis::plot_trajectory demo")

out <- file.path(tempdir(), "anivis_plot_trajectory_demo.png")
ggsave(out, combined, width = 14, height = 8, dpi = 150)
message("Saved combined demo to: ", out)
