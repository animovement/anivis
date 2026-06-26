# Tests for plot_timeseries.

with_speed <- function(af) {
  af$speed <- sqrt(af$x^2 + af$y^2)
  af
}

make_ts_single <- function() {
  with_speed(
    aniframe::as_aniframe(data.frame(time = 1:10, x = rnorm(10), y = rnorm(10)))
  )
}

make_ts_multi_keypoint <- function() {
  with_speed(aniframe::as_aniframe(data.frame(
    keypoint = rep(c("head", "tail"), each = 10),
    time = rep(1:10, 2),
    x = rnorm(20),
    y = rnorm(20)
  )))
}

make_ts_matrix <- function() {
  with_speed(aniframe::as_aniframe(
    data.frame(
      individual = rep(c("a", "b"), each = 30),
      trial = rep(rep(c(1L, 2L, 3L), each = 10), 2),
      time = rep(1:10, 6),
      x = rnorm(60),
      y = rnorm(60)
    ),
    variables_when = c("trial", "time")
  ))
}


test_that("plot_timeseries returns a ggplot and maps y to the variable", {
  p <- plot_timeseries(make_ts_single(), variable = "speed")
  expect_s3_class(p, "ggplot")
  expect_equal(rlang::as_label(p$mapping$y), "speed")
})

test_that("plot_timeseries errors when data is not an aniframe", {
  df <- data.frame(time = 1:5, x = rnorm(5), y = rnorm(5), speed = runif(5))
  expect_error(plot_timeseries(df, variable = "speed"), "must be an aniframe")
})

test_that("plot_timeseries requires an explicit variable (no auto-detection)", {
  expect_error(plot_timeseries(make_ts_single()), "variable.*required")
})

test_that("plot_timeseries rejects an unknown or non-numeric variable", {
  expect_error(
    plot_timeseries(make_ts_single(), variable = "nope"),
    "unknown column"
  )
  af <- make_ts_single()
  af$tag <- "x"
  expect_error(
    plot_timeseries(af, variable = "tag"),
    "must be numeric"
  )
})

test_that("plot_timeseries stacks several variables into a patchwork", {
  af <- make_ts_single()
  af$accel <- c(0, diff(af$speed))
  p <- plot_timeseries(af, variable = c("speed", "accel"))
  expect_s3_class(p, "patchwork")
})

test_that("plot_timeseries layout = 'inline' overlays groups in one panel", {
  p <- plot_timeseries(make_ts_multi_keypoint(), variable = "speed")
  expect_s3_class(p$facet, "FacetNull")
})

test_that("plot_timeseries layout = 'facet' facet_wraps a single varying axis", {
  p <- plot_timeseries(
    make_ts_multi_keypoint(),
    variable = "speed",
    layout = "facet"
  )
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_timeseries layout = 'facet' uses facet_grid when both vary", {
  p <- plot_timeseries(make_ts_matrix(), variable = "speed", layout = "facet")
  expect_s3_class(p$facet, "FacetGrid")
})
