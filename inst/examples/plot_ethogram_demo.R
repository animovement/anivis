# Demo: plot.anievent() / plot_ethogram() default ethogram plots
#
# Run interactively. Each anievent is plotted and finally a combined
# patchwork PNG is written to tempdir() for sharing.

library(anivis)
library(aniframe)
library(ggplot2)
library(patchwork)


# 1. Single-channel, single-individual
ae1 <- anievent(
  channel = rep("behaviour", 6),
  label = c("REM", "wake", "REM", "wake", "REM", "wake"),
  start = c(3, 14, 22, 30, 42, 55),
  stop = c(9, 19, 28, 38, 50, 62)
)
p1 <- plot_ethogram(ae1) + ggtitle("1. Single channel / individual")
print(p1)


# 2. Multi-channel (behaviour + call)
ae2 <- anievent(
  channel = c("behaviour", "behaviour", "behaviour", "call", "call", "call"),
  label = c("REM", "wake", "REM", "alarm", "song", "song"),
  start = c(3, 14, 22, 7, 12, 25),
  stop = c(9, 19, 28, 8, 15, 30)
)
p2 <- plot_ethogram(ae2) + ggtitle("2. Multi-channel (facet_wrap by channel)")
print(p2)


# 3. Multi-individual, single channel
ae3 <- anievent(
  individual = rep(1:3, each = 4),
  channel = rep("behaviour", 12),
  label = rep(c("REM", "wake"), 6),
  start = c(3, 14, 22, 30, 1, 8, 15, 22, 5, 13, 20, 28),
  stop = c(9, 19, 28, 38, 6, 12, 21, 27, 11, 18, 26, 33)
)
p3 <- plot_ethogram(ae3) +
  ggtitle("3. Multi-individual (facet_wrap by individual)")
print(p3)


# 4. Multi-channel x multi-individual (facet_grid)
ae4 <- anievent(
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
  stop = c(9, 19, 6, 12, 8, 15, 11, 22)
)
p4 <- plot_ethogram(ae4) + ggtitle("4. Multi-channel x individual (facet_grid)")
print(p4)


# 5. Dark mode
p5 <- plot_ethogram(ae2, mode = "dark") + ggtitle("5. Multi-channel, dark mode")
print(p5)


# Combined PNG
combined <- (p1 | p2) /
  (p3 | p4) /
  p5 +
  plot_annotation(title = "anivis::plot_ethogram demo")

out <- file.path(tempdir(), "anivis_plot_ethogram_demo.png")
ggsave(out, combined, width = 14, height = 12, dpi = 150)
message("Saved combined demo to: ", out)
