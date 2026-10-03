# Tests for reading positions and time from the frame's declarations (#37)
# ------------------------------------------------------------------------
# anicore lets the axis roles live in columns of any name (get_axes()) and the
# index be any column (get_index()). The plots must read both from the frame
# rather than assume `x`, `y` and `time`: these frames used to fail with
# "Column `x` not found" and "argument 2 is not a vector".

# One trajectory whose values are all distinct, so each can be traced to the
# column it came from.
xs <- c(10, 11, 12, 13, 14)
ys <- c(20, 22, 21, 23, 25)

renamed_axes <- function() {
  anicore::as_anipoint(
    data.frame(time = 0:4, individual = "a", u = xs, v = ys),
    variables_where = c(x = "u", y = "v")
  )
}

renamed_index <- function() {
  anicore::as_anipoint(
    data.frame(frame = 0:4, individual = "a", x = xs, y = ys),
    index = "frame"
  )
}

renamed_both <- function() {
  anicore::as_anipoint(
    data.frame(frame = 0:4, individual = "a", u = xs, v = ys),
    variables_where = c(x = "u", y = "v"),
    index = "frame"
  )
}

renamed_frames <- function() {
  list(axes = renamed_axes(), index = renamed_index(), both = renamed_both())
}

# Two keypoints, so the trajectory is grouped ("what" mode).
renamed_both_grouped <- function() {
  anicore::as_anipoint(
    data.frame(
      frame = rep(0:4, 2),
      keypoint = rep(c("head", "tail"), each = 5),
      u = c(xs, xs + 100),
      v = c(ys, ys + 100)
    ),
    variables_where = c(x = "u", y = "v"),
    index = "frame"
  )
}

layer_of <- function(p, geom) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  which(geoms == geom)[[1]]
}


# --- plot_trajectory() -------------------------------------------------------

test_that("plot_trajectory() draws from the declared axis and index columns", {
  for (af in renamed_frames()) {
    built <- ggplot2::ggplot_build(plot_trajectory(af))
    path <- built$data[[1]]
    expect_equal(path$x, xs)
    expect_equal(path$y, ys)
  }
})

test_that("plot_trajectory() labels the axes by role, with units", {
  for (af in renamed_frames()) {
    af <- anicore::set_metadata(af, unit_space = "mm")
    built <- ggplot2::ggplot_build(plot_trajectory(af))
    expect_equal(built$plot$labels$x, "x (mm)")
    expect_equal(built$plot$labels$y, "y (mm)")
  }
})

test_that("plot_trajectory() marks the start and end of a renamed trajectory", {
  for (af in renamed_frames()) {
    p <- plot_trajectory(af)
    built <- ggplot2::ggplot_build(p)
    ends <- built$data[[layer_of(p, "GeomPoint")]]
    expect_equal(ends$x, c(xs[1], xs[5]))
    expect_equal(ends$y, c(ys[1], ys[5]))
  }
})

test_that("plot_trajectory() colours a single trajectory by its index", {
  p <- plot_trajectory(renamed_both())
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$colour), "frame")
  expect_equal(p$scales$get_scales("colour")$name, "time")
  built <- ggplot2::ggplot_build(p)
  expect_length(unique(built$data[[1]]$colour), 5)
})

test_that("plot_trajectory() fades grouped trajectories in along their index", {
  af <- renamed_both_grouped()
  p <- plot_trajectory(af)
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$alpha), "frame")
  expect_equal(p$scales$get_scales("alpha")$name, "time")

  built <- ggplot2::ggplot_build(p)
  path <- built$data[[1]]
  expect_equal(sort(path$x), sort(c(xs, xs + 100)))
  # Each line fades in from its first frame to its last.
  for (g in unique(path$group)) {
    expect_false(is.unsorted(path$alpha[path$group == g]))
  }
})

test_that("plot_trajectory() formats a renamed index as HH:MM:SS for time units", {
  af <- anicore::set_metadata(renamed_both(), unit_time = "s")
  p <- plot_trajectory(af)
  expect_match(p$scales$get_scales("colour")$labels(60), "00:01:00")
})

test_that("plot_trajectory() bridges gaps between declared columns", {
  af <- renamed_both()
  af$u[3] <- NA
  af$v[3] <- NA
  p <- plot_trajectory(af)
  built <- ggplot2::ggplot_build(p)
  bridge <- built$data[[layer_of(p, "GeomSegment")]]
  expect_equal(bridge$x, xs[2])
  expect_equal(bridge$y, ys[2])
  expect_equal(bridge$xend, xs[4])
  expect_equal(bridge$yend, ys[4])
})

test_that("plot_trajectory() reads the axes from the declarations, not the names", {
  # Columns called `x` and `y` that carry the other axis: the roles win.
  af <- suppressWarnings(anicore::as_anipoint(
    data.frame(time = 0:4, individual = "a", x = ys, y = xs),
    variables_where = c(x = "y", y = "x")
  ))
  path <- ggplot2::ggplot_build(plot_trajectory(af))$data[[1]]
  expect_equal(path$x, xs)
  expect_equal(path$y, ys)
})

test_that("plot_trajectory() draws a 3D frame in its x-y plane", {
  af <- anicore::as_anipoint(
    data.frame(frame = 0:4, individual = "a", p = xs, q = ys, r = xs * 2),
    variables_where = c(x = "p", y = "q", z = "r"),
    index = "frame"
  )
  path <- ggplot2::ggplot_build(plot_trajectory(af))$data[[1]]
  expect_equal(path$x, xs)
  expect_equal(path$y, ys)
})

test_that("plot_trajectory() refuses a frame without x and y axes", {
  polar <- anicore::as_anipoint(
    data.frame(time = 0:4, individual = "a", rho = 1:5, phi = 0:4 / 10)
  )
  expect_error(plot_trajectory(polar), "x and y axes", class = "rlang_error")
  expect_error(plot_trajectory(polar), "map_to_cartesian")

  undeclared <- suppressWarnings(anicore::as_anipoint(
    data.frame(time = 0:4, individual = "a", u = xs, v = ys),
    variables_where = c("u", "v")
  ))
  # anicore warns, on reading the axes, that it could not infer them.
  expect_error(
    suppressWarnings(plot_trajectory(undeclared)),
    "set_variables"
  )

  one_d <- anicore::as_anipoint(
    data.frame(time = 0:4, individual = "a", x = xs)
  )
  expect_error(plot_trajectory(one_d), "cartesian_1d")
})


# --- plot() ------------------------------------------------------------------

test_that("plot() draws an anipoint with renamed axes and index", {
  for (af in renamed_frames()) {
    p <- plot(af)
    expect_s3_class(p, "patchwork")
    path <- ggplot2::ggplot_build(p[[1]])$data[[1]]
    expect_equal(path$x, xs)
    expect_equal(path$y, ys)
  }
})


# --- plot_timeseries() -------------------------------------------------------

test_that("plot_timeseries() puts the index on the x axis", {
  for (af in renamed_frames()) {
    af <- anicore::set_metadata(af, unit_time = "frame")
    y_col <- anicore::get_axes(af)[["y"]]
    built <- ggplot2::ggplot_build(plot_timeseries(af, variable = y_col))
    line <- built$data[[1]]
    expect_equal(line$x, 0:4)
    expect_equal(line$y, ys)
    expect_equal(built$plot$labels$x, "time (frames)")
    expect_equal(built$plot$labels$y, y_col)
  }
})

test_that("plot_timeseries() converts a renamed index to hms for time units", {
  af <- anicore::set_metadata(renamed_both(), unit_time = "ms")
  p <- plot_timeseries(af, variable = "v")
  expect_equal(p$scales$get_scales("x")$trans$name, "hms")
  expect_s3_class(p$data$frame, "hms")
  line <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(line$x, (0:4) / 1000)
})

test_that("plot_timeseries() stacks and facets a frame with a renamed index", {
  af <- renamed_both_grouped()

  stacked <- plot_timeseries(af, variable = c("u", "v"))
  expect_s3_class(stacked, "patchwork")
  for (panel in list(stacked[[1]], stacked[[2]])) {
    line <- ggplot2::ggplot_build(panel)$data[[1]]
    expect_equal(sort(unique(line$x)), 0:4)
  }

  faceted <- plot_timeseries(af, variable = "u", layout = "facet")
  expect_s3_class(faceted$facet, "FacetWrap")
  built <- ggplot2::ggplot_build(faceted)
  expect_equal(nrow(built$layout$layout), 2)
})


# --- grouping ----------------------------------------------------------------

test_that("trajectory groups are the frame's keys, whatever the index is called", {
  af <- anicore::as_anipoint(
    data.frame(
      frame = rep(0:4, 4),
      keypoint = rep(c("head", "tail"), each = 10),
      trial = rep(rep(1:2, each = 5), 2),
      x = seq_len(20),
      y = seq_len(20)
    ),
    variables_when = "trial",
    index = "frame"
  )
  keys <- aniframe_group_keys(af)
  expect_identical(keys$what_cols, "keypoint")
  expect_identical(keys$when_cols, "trial")
  expect_identical(c(keys$what_cols, keys$when_cols), anicore::get_keys(af))
  expect_identical(keys$mode, "matrix")
  expect_length(palette_animovement(af), 4)
})
