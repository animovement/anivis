# Demo: plot.anievent() / plot_events() default state + point event plots.
#
# Run interactively. Each anievent is plotted and finally a combined
# patchwork PNG is written to tempdir() for sharing.

library(anivis)
library(aniframe)
library(ggplot2)
library(patchwork)


# 1. Single channel, only state events
ae1 <- anievent(
  channel = rep("behaviour", 6),
  label = c("REM", "wake", "REM", "wake", "REM", "wake"),
  start = c(3, 14, 22, 30, 42, 55),
  stop = c(9, 19, 28, 38, 50, 62)
)
p1 <- plot_events(ae1) + ggtitle("1. State events only")
print(p1)


# 2. Single channel, only point events (raster style)
ae2 <- anievent(
  channel = rep("call", 8),
  label = rep(c("alarm", "song"), each = 4),
  start = c(2, 8, 17, 28, 4, 12, 22, 33),
  stop = c(2, 8, 17, 28, 4, 12, 22, 33)
)
p2 <- plot_events(ae2) + ggtitle("2. Point events (raster ticks)")
print(p2)


# 3. Mixed state + point in a single channel
ae3 <- anievent(
  channel = rep("behaviour", 7),
  label = c("REM", "wake", "REM", "wake", "twitch", "twitch", "twitch"),
  start = c(3, 14, 22, 30, 6, 17, 33),
  stop = c(9, 19, 28, 38, 6, 17, 33)
)
p3 <- plot_events(ae3) + ggtitle("3. Mixed (state bars + point ticks)")
print(p3)


# 4. Multi-channel mixing types
ae4 <- anievent(
  channel = c(
    "behaviour",
    "behaviour",
    "behaviour",
    "call",
    "call",
    "call",
    "call"
  ),
  label = c(
    "REM",
    "wake",
    "REM",
    "alarm",
    "song",
    "alarm",
    "song"
  ),
  start = c(3, 14, 22, 7, 12, 24, 30),
  stop = c(9, 19, 28, 7, 12, 24, 30)
)
p4 <- plot_events(ae4) + ggtitle("4. Multi-channel (facet_wrap)")
print(p4)


# 5. Multi-individual x multi-channel (facet_grid)
ae5 <- anievent(
  individual = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
  channel = c(
    "behaviour",
    "behaviour",
    "behaviour",
    "behaviour",
    "call",
    "call",
    "call",
    "call"
  ),
  label = c(
    "REM",
    "wake",
    "REM",
    "wake",
    "alarm",
    "song",
    "song",
    "alarm"
  ),
  start = c(3, 14, 1, 7, 5, 12, 8, 20),
  stop = c(9, 19, 6, 12, 5, 12, 8, 20)
)
p5 <- plot_events(ae5) + ggtitle("5. Multi-channel x individual (facet_grid)")
print(p5)


# 6. Default method: hand-rolled state + point data frames
state_df <- data.frame(
  label = c("a", "b", "a", "b"),
  start = c(0, 0, 12, 12),
  stop = c(8, 10, 18, 20)
)
point_df <- data.frame(
  label = c("c", "c", "c"),
  start = c(3, 9, 15)
)
p6 <- plot_events(state_df, point = point_df) +
  ggtitle("6. Default method (manual state + point)")
print(p6)


# 7. Numeric labels (e.g. neuron / channel IDs) — sorted numerically on
#    the y axis instead of lexically (1, 10, 11, ... 2, 20, ...).
set.seed(7)
n_ids <- 20
events_per_id <- 6
times <- runif(n_ids * events_per_id, 0, 60)
ae7 <- anievent(
  channel = rep("spikes", n_ids * events_per_id),
  label = rep(as.character(seq_len(n_ids)), each = events_per_id),
  start = times,
  stop = times
)
p7 <- plot_events(ae7) + ggtitle("7. Numeric labels (sorted numerically)")
print(p7)


# Combined PNG
combined <- (p1 | p2 | p3) /
  (p4 | p5 | p6) /
  p7 +
  plot_annotation(title = "anivis::plot_events demo")

out <- file.path(tempdir(), "anivis_plot_events_demo.png")
ggsave(out, combined, width = 16, height = 12, dpi = 150)
message("Saved combined demo to: ", out)
