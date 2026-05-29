# Tests for plot.anievent, plot_events and the underlying geoms.

make_anievent_state_only <- function() {
  aniframe::anievent(
    channel = rep("behaviour", 4),
    label = c("REM", "wake", "REM", "wake"),
    start = c(3, 14, 22, 30),
    stop = c(9, 19, 28, 38)
  )
}

make_anievent_point_only <- function() {
  aniframe::anievent(
    channel = rep("call", 3),
    label = c("alarm", "song", "alarm"),
    start = c(5, 12, 22),
    stop = c(5, 12, 22)
  )
}

make_anievent_mixed <- function() {
  aniframe::anievent(
    channel = c("behaviour", "behaviour", "call", "call"),
    label = c("REM", "wake", "alarm", "song"),
    start = c(3, 14, 7, 12),
    stop = c(9, 19, 7, 12)
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
  p <- plot(make_anievent_state_only())
  expect_s3_class(p, "patchwork")
})

test_that("plot_events.anievent returns a ggplot", {
  p <- plot_events(make_anievent_state_only())
  expect_s3_class(p, "ggplot")
})

test_that("plot_events.default errors when both data and point are NULL", {
  expect_error(plot_events(NULL), "must be supplied")
})

test_that("plot_events.anievent in dark mode applies a theme", {
  p <- plot_events(make_anievent_state_only(), mode = "dark")
  expect_s3_class(p, "ggplot")
})


# --- state vs point detection -----------------------------------------------

test_that("plot_events.anievent adds only a state layer when all events are state", {
  p <- plot_events(make_anievent_state_only())
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventState" %in% geom_classes)
  expect_false("GeomEventPoint" %in% geom_classes)
})

test_that("plot_events.anievent adds only a point layer when all events are point", {
  p <- plot_events(make_anievent_point_only())
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventPoint" %in% geom_classes)
  expect_false("GeomEventState" %in% geom_classes)
})

test_that("plot_events.anievent stacks both layers when the anievent mixes types", {
  p <- plot_events(make_anievent_mixed())
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventState" %in% geom_classes)
  expect_true("GeomEventPoint" %in% geom_classes)
})


# --- facet dispatch ----------------------------------------------------------

test_that("plot_events.anievent does not facet a single-channel single-individual anievent", {
  p <- plot_events(make_anievent_state_only())
  expect_s3_class(p$facet, "FacetNull")
})

test_that("plot_events.anievent facet_wraps on channel when only channel varies", {
  p <- plot_events(make_anievent_multi_channel())
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_events.anievent facet_wraps on what when only what varies", {
  p <- plot_events(make_anievent_multi_individual())
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_events.anievent uses facet_grid when both channel and what vary", {
  p <- plot_events(make_anievent_grid())
  expect_s3_class(p$facet, "FacetGrid")
})


# --- axis labels, time scale and theme overrides ----------------------------

test_that("plot_events includes the unit in the x-axis label when set", {
  data_s <- make_anievent_state_only() |>
    aniframe::set_unit_time("s")
  expect_equal(plot_events(data_s)$labels$x, "time (s)")

  data_m <- make_anievent_state_only() |>
    aniframe::set_unit_time("m")
  expect_equal(plot_events(data_m)$labels$x, "time (m)")
})

test_that("plot_events leaves the x-axis label unit-free for 'unknown'", {
  data <- make_anievent_state_only() |>
    aniframe::set_unit_time("unknown")
  expect_equal(plot_events(data)$labels$x, "time")
})

test_that("plot_events applies scale_x_time when the unit is convertible", {
  data <- make_anievent_state_only() |>
    aniframe::set_unit_time("s")
  p <- plot_events(data)
  x_scale <- p$scales$get_scales("x")
  expect_equal(x_scale$trans$name, "hms")
})

test_that("plot_events drops the panel border and y-axis gridlines", {
  p <- plot_events(make_anievent_state_only())
  expect_s3_class(p$theme$panel.border, "element_blank")
  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
  expect_s3_class(p$theme$panel.grid.minor.y, "element_blank")
})


# --- unit-time hms conversion -----------------------------------------------

test_that("seconds_per_unit returns the expected factor for known time units", {
  expect_equal(seconds_per_unit("s"), 1)
  expect_equal(seconds_per_unit("m"), 60)
  expect_equal(seconds_per_unit("h"), 3600)
  expect_equal(seconds_per_unit("ms"), 1e-3)
  expect_equal(seconds_per_unit("us"), 1e-6)
  expect_equal(seconds_per_unit("ns"), 1e-9)
})

test_that("seconds_per_unit treats NULL as seconds", {
  expect_equal(seconds_per_unit(NULL), 1)
})

test_that("seconds_per_unit returns NA for 'unknown' regardless of sampling_rate", {
  expect_true(is.na(seconds_per_unit("unknown")))
  expect_true(is.na(seconds_per_unit("unknown", sampling_rate = 30)))
})

test_that("seconds_per_unit returns NA for 'frame' without a sampling_rate", {
  expect_true(is.na(seconds_per_unit("frame")))
  expect_true(is.na(seconds_per_unit("frame", sampling_rate = NA_real_)))
  expect_true(is.na(seconds_per_unit("frame", sampling_rate = 0)))
})

test_that("seconds_per_unit converts frames to seconds via sampling_rate", {
  expect_equal(seconds_per_unit("frame", sampling_rate = 30), 1 / 30)
  expect_equal(seconds_per_unit("frame", sampling_rate = 60), 1 / 60)
})

test_that("to_hms_columns scales numeric columns by `factor`", {
  df <- data.frame(label = "a", start = 1, stop = 2)
  out_s <- to_hms_columns(df, c("start", "stop"), 1)
  expect_s3_class(out_s$start, "hms")
  expect_equal(as.numeric(out_s$start), 1)
  expect_equal(as.numeric(out_s$stop), 2)

  out_min <- to_hms_columns(df, c("start", "stop"), 60)
  expect_equal(as.numeric(out_min$start), 60)
  expect_equal(as.numeric(out_min$stop), 120)
})

test_that("to_hms_columns is a no-op on NULL data", {
  expect_null(to_hms_columns(NULL, c("start", "stop"), 1))
})


# --- frame-unit fallback in plot_events -------------------------------------

test_that("plot_events on frame data without sampling_rate uses continuous scale", {
  ae <- make_anievent_state_only() |>
    aniframe::set_unit_time("frame")
  p <- plot_events(ae)
  x_scale <- p$scales$get_scales("x")
  expect_false(identical(x_scale$trans$name, "hms"))
  expect_equal(p$labels$x, "time (frame)")
})

test_that("plot_events on frame data WITH sampling_rate uses scale_x_time", {
  ae <- make_anievent_state_only() |>
    aniframe::set_unit_time("frame") |>
    aniframe::set_metadata(sampling_rate = 30)
  p <- plot_events(ae)
  x_scale <- p$scales$get_scales("x")
  expect_equal(x_scale$trans$name, "hms")
  expect_equal(p$labels$x, "time (frame)")

  # And the state layer's xmin was scaled to seconds: first bout starts
  # at frame 3, which at 30 fps is 0.1 seconds (i.e. 3/30).
  layer_data <- p$layers[[1]]$layer_data(p$data)
  if (is.null(layer_data) || !nrow(layer_data)) {
    layer_data <- p$layers[[1]]$data
  }
  expect_true(any(abs(as.numeric(layer_data$start) - 3 / 30) < 1e-9))
})

test_that("plot_events on 'unknown' unit falls back to a continuous scale", {
  ae <- make_anievent_state_only() |>
    aniframe::set_unit_time("unknown")
  p <- plot_events(ae)
  x_scale <- p$scales$get_scales("x")
  expect_false(identical(x_scale$trans$name, "hms"))
  expect_equal(p$labels$x, "time")
})


# --- plot_events.default ----------------------------------------------------

test_that("plot_events.default accepts a state-only data frame", {
  state_df <- data.frame(
    label = c("a", "b"),
    start = c(0, 5),
    stop = c(3, 8)
  )
  p <- plot_events(state_df)
  expect_s3_class(p, "ggplot")
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventState" %in% geom_classes)
})

test_that("plot_events.default accepts a point-only argument", {
  point_df <- data.frame(label = c("a", "b"), start = c(1, 4))
  p <- plot_events(point = point_df)
  expect_s3_class(p, "ggplot")
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventPoint" %in% geom_classes)
})

test_that("plot_events.default accepts both state and point", {
  state_df <- data.frame(label = c("a"), start = 0, stop = 5)
  point_df <- data.frame(label = c("b"), start = 3)
  p <- plot_events(state_df, point = point_df)
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomEventState" %in% geom_classes)
  expect_true("GeomEventPoint" %in% geom_classes)
})


# --- direct geom usage ------------------------------------------------------

test_that("geom_event_state accepts an explicit data argument", {
  p <- ggplot2::ggplot() +
    geom_event_state(data = make_anievent_state_only())
  expect_s3_class(p, "ggplot")
})

test_that("geom_event_state merges user mapping on top of xmin/xmax", {
  p <- ggplot2::ggplot(make_anievent_state_only()) +
    geom_event_state(ggplot2::aes(fill = label))
  layer_mapping <- p$layers[[1]]$mapping
  expect_true("fill" %in% names(layer_mapping))
  expect_true("xmin" %in% names(layer_mapping))
  expect_true("xmax" %in% names(layer_mapping))
})

test_that("geom_event_state accepts a function as the data argument", {
  drop_first <- function(d) d[-1, , drop = FALSE]
  p <- ggplot2::ggplot(make_anievent_state_only()) +
    geom_event_state(data = drop_first)
  expect_s3_class(p, "ggplot")
})

test_that("geom_event_point accepts an explicit data argument", {
  p <- ggplot2::ggplot() +
    geom_event_point(data = make_anievent_point_only())
  expect_s3_class(p, "ggplot")
})

test_that("geom_event_point merges user mapping on top of x = start", {
  p <- ggplot2::ggplot(make_anievent_point_only()) +
    geom_event_point(ggplot2::aes(colour = label))
  layer_mapping <- p$layers[[1]]$mapping
  expect_true("colour" %in% names(layer_mapping))
  expect_true("x" %in% names(layer_mapping))
})

test_that("geom_event_point accepts a function as the data argument", {
  drop_first <- function(d) d[-1, , drop = FALSE]
  p <- ggplot2::ggplot(make_anievent_point_only()) +
    geom_event_point(data = drop_first)
  expect_s3_class(p, "ggplot")
})
