# Tests for plot.anievent, plot_ethogram and facet dispatch.

make_anievent_single <- function() {
  aniframe::anievent(
    channel = rep("behaviour", 4),
    label = c("REM", "wake", "REM", "wake"),
    start = c(3, 14, 22, 30),
    stop = c(9, 19, 28, 38)
  )
}

make_anievent_multi_channel <- function() {
  aniframe::anievent(
    channel = c("behaviour", "behaviour", "call", "call"),
    label = c("REM", "wake", "alarm", "song"),
    start = c(3, 14, 7, 12),
    stop = c(9, 19, 8, 15)
  )
}

make_anievent_multi_individual <- function() {
  aniframe::anievent(
    individual = c(1L, 1L, 2L, 2L),
    channel = rep("behaviour", 4),
    label = c("REM", "wake", "REM", "wake"),
    start = c(3, 14, 1, 7),
    stop = c(9, 19, 6, 12)
  )
}

make_anievent_grid <- function() {
  aniframe::anievent(
    individual = c(1L, 1L, 2L, 2L, 1L, 2L),
    channel = c(
      "behaviour",
      "behaviour",
      "behaviour",
      "behaviour",
      "call",
      "call"
    ),
    label = c("REM", "wake", "REM", "wake", "song", "song"),
    start = c(3, 14, 1, 7, 5, 6),
    stop = c(9, 19, 6, 12, 8, 9)
  )
}


# --- basics ------------------------------------------------------------------

test_that("plot.anievent returns a patchwork object", {
  p <- plot(make_anievent_single())
  expect_s3_class(p, "patchwork")
})

test_that("plot_ethogram returns a ggplot", {
  p <- plot_ethogram(make_anievent_single())
  expect_s3_class(p, "ggplot")
})

test_that("plot_ethogram errors when data is not an anievent", {
  data <- data.frame(start = 1:5, stop = 2:6, label = letters[1:5])
  expect_error(plot_ethogram(data), "must be an anievent")
})

test_that("plot_ethogram contains a GeomEthogram (GeomRect subclass) layer", {
  p <- plot_ethogram(make_anievent_single())
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEthogram" %in% geom_classes)
})

test_that("plot_ethogram in dark mode applies a theme", {
  p <- plot_ethogram(make_anievent_single(), mode = "dark")
  expect_s3_class(p, "ggplot")
})


# --- facet dispatch ----------------------------------------------------------

test_that("plot_ethogram does not facet a single-channel single-individual anievent", {
  p <- plot_ethogram(make_anievent_single())
  expect_s3_class(p$facet, "FacetNull")
})

test_that("plot_ethogram facet_wraps on channel when only channel varies", {
  p <- plot_ethogram(make_anievent_multi_channel())
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_ethogram facet_wraps on what when only what varies", {
  p <- plot_ethogram(make_anievent_multi_individual())
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_ethogram uses facet_grid when both channel and what vary", {
  p <- plot_ethogram(make_anievent_grid())
  expect_s3_class(p$facet, "FacetGrid")
})


# --- axis labels and time unit ----------------------------------------------

test_that("plot_ethogram labels the x axis with unit_time when set", {
  data <- make_anievent_single() |>
    aniframe::set_unit_time("s")
  p <- plot_ethogram(data)
  expect_match(p$labels$x, "s")
})

test_that("plot_ethogram leaves the time label unit-free for frame / unknown", {
  p <- plot_ethogram(make_anievent_single())
  expect_equal(p$labels$x, "time")
})


# --- geom_ethogram direct usage ---------------------------------------------

test_that("geom_ethogram accepts an explicit data argument", {
  p <- ggplot2::ggplot() +
    geom_ethogram(data = make_anievent_single())
  expect_s3_class(p, "ggplot")
})

test_that("geom_ethogram merges user mapping on top of the default xmin/xmax", {
  p <- ggplot2::ggplot(make_anievent_single()) +
    geom_ethogram(ggplot2::aes(fill = label))
  layer_mapping <- p$layers[[1]]$mapping
  expect_true("fill" %in% names(layer_mapping))
  expect_true("xmin" %in% names(layer_mapping))
  expect_true("xmax" %in% names(layer_mapping))
})

test_that("geom_ethogram accepts a function as the data argument", {
  drop_first <- function(d) d[-1, , drop = FALSE]
  p <- ggplot2::ggplot(make_anievent_single()) +
    geom_ethogram(data = drop_first)
  expect_s3_class(p, "ggplot")
})
