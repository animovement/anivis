# Tests for the diagnostic check plots (plot.check_*), their as_plot_data()
# staging methods, and the imputeTS-style theme they share. Genuine check
# objects are built by the make_check_*() helpers in helper-check-objects.R.

# --- fixtures ----------------------------------------------------------------

af_na_single <- function(unit = NULL) {
  af <- aniframe::as_aniframe(data.frame(
    time = 1:20,
    x = c(1:3, NA, NA, 6:10, NA, 12:18, NA, 20),
    y = rnorm(20)
  ))
  if (!is.null(unit)) {
    af <- aniframe::set_unit_time(af, unit)
  }
  af
}

af_na_multi <- function(unit = NULL) {
  af <- aniframe::as_aniframe(data.frame(
    keypoint = rep(c("head", "tail"), each = 20),
    time = rep(1:20, 2),
    x = c(c(1:3, NA, NA, 6:20), c(1:10, NA, NA, NA, 14:20)),
    y = rnorm(40)
  ))
  if (!is.null(unit)) {
    af <- aniframe::set_unit_time(af, unit)
  }
  af
}

af_na_none <- function() {
  aniframe::as_aniframe(data.frame(time = 1:10, x = 1:10, y = 1:10))
}

af_conf_single <- function() {
  af <- aniframe::as_aniframe(data.frame(
    keypoint = rep(c("head", "tail"), each = 60),
    time = rep(1:60, 2),
    x = rnorm(120),
    y = rnorm(120)
  ))
  af$confidence <- c(runif(60, 0.6, 1), runif(60, 0.1, 0.9))
  af
}

af_conf_facet <- function() {
  af <- aniframe::as_aniframe(data.frame(
    individual = rep(c("a", "b"), each = 120),
    keypoint = rep(rep(c("head", "tail"), each = 60), 2),
    time = rep(1:60, 4),
    x = rnorm(240),
    y = rnorm(240)
  ))
  af$confidence <- runif(240, 0.2, 1)
  af
}


# --- as_plot_data dispatch ---------------------------------------------------

test_that("as_plot_data.default errors for objects with no method", {
  expect_error(as_plot_data(1L), "no method")
  expect_error(as_plot_data(data.frame(a = 1)), "no method")
})


# --- plot.check_na_timing ----------------------------------------------------

test_that("plot.check_na_timing returns a ggplot", {
  p <- plot(make_check_na_timing(af_na_single()))
  expect_s3_class(p, "ggplot")
})

test_that("plot.check_na_timing draws one geom_col layer", {
  p <- plot(make_check_na_timing(af_na_single()))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomCol" %in% geoms)
})

test_that("plot.check_na_timing measure switches the y label", {
  expect_equal(
    plot(make_check_na_timing(af_na_single()), measure = "percent")$labels$y,
    "Percent"
  )
  expect_equal(
    plot(make_check_na_timing(af_na_single()), measure = "count")$labels$y,
    "Count"
  )
})

test_that("plot.check_na_timing facets only when there are several groups", {
  expect_s3_class(plot(make_check_na_timing(af_na_single()))$facet, "FacetNull")
  expect_s3_class(plot(make_check_na_timing(af_na_multi()))$facet, "FacetWrap")
})

test_that("plot.check_na_timing formats true time units as HH:MM:SS", {
  p <- plot(make_check_na_timing(af_na_single(unit = "s")))
  lab_fun <- p$scales$get_scales("x")$labels
  expect_true(is.function(lab_fun))
  expect_match(lab_fun(60), "00:01:00")
})

test_that("plot.check_na_timing uses raw axis labels for non-time units", {
  p <- plot(make_check_na_timing(af_na_single(unit = "frame")))
  expect_s3_class(p$scales$get_scales("x")$labels, "waiver")
})

test_that("plot.check_na_timing handles data with no missing values", {
  p <- plot(make_check_na_timing(af_na_none()))
  expect_s3_class(p, "ggplot")
})

test_that("plot.check_na_timing honours an explicit n_intervals", {
  pd <- as_plot_data(make_check_na_timing(af_na_multi()), n_intervals = 4)
  # 4 intervals over 20 frames -> interval size of 5.
  expect_equal(attr(pd, "interval_size"), 5)
})


# --- as_plot_data.check_na_timing --------------------------------------------

test_that("as_plot_data.check_na_timing returns the staged class with attrs", {
  pd <- as_plot_data(make_check_na_timing(af_na_multi()))
  expect_s3_class(pd, "anivis_check_na_timing_data")
  expect_setequal(attr(pd, "group_levels"), c("head", "tail"))
  expect_true(is.numeric(attr(pd, "interval_size")))
  expect_setequal(levels(pd$status), c("present", "missing"))
})

test_that("as_plot_data.check_na_timing percent values are shares in [0, 1]", {
  pd <- as_plot_data(make_check_na_timing(af_na_single()), measure = "percent")
  expect_true(all(pd$value >= 0 & pd$value <= 1))
})

test_that("as_plot_data.check_na_timing count values are frame counts", {
  pd <- as_plot_data(make_check_na_timing(af_na_single()), measure = "count")
  # Total counted frames equal the recording length (present + missing).
  expect_equal(sum(pd$value), 20)
})


# --- plot.check_na_gapsize ---------------------------------------------------

test_that("plot.check_na_gapsize returns a ggplot with a geom_col layer", {
  p <- plot(make_check_na_gapsize(af_na_single()))
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomCol" %in% geoms)
})

test_that("plot.check_na_gapsize accepts both ranking orders", {
  expect_s3_class(
    plot(make_check_na_gapsize(af_na_single()), ranked_by = "occurrence"),
    "ggplot"
  )
  expect_s3_class(
    plot(make_check_na_gapsize(af_na_single()), ranked_by = "total"),
    "ggplot"
  )
})

test_that("plot.check_na_gapsize include_total = FALSE drops the total series", {
  pd <- as_plot_data(
    make_check_na_gapsize(af_na_single()),
    include_total = FALSE
  )
  expect_false("total" %in% as.character(pd$series))
  pd2 <- as_plot_data(make_check_na_gapsize(af_na_single()))
  expect_true("total" %in% as.character(pd2$series))
})

test_that("plot.check_na_gapsize keeps at most `limit` gap sizes per group", {
  # af_na_single has two distinct gap sizes (1 and 2); limit = 1 keeps the
  # single top-ranked one, exercising the truncation branch.
  full <- as_plot_data(make_check_na_gapsize(af_na_single()))
  expect_gt(length(unique(as.character(full$key))), 1)

  pd <- as_plot_data(make_check_na_gapsize(af_na_single()), limit = 1)
  n_keys <- length(unique(as.character(pd$key[pd$series == "occurrence"])))
  expect_equal(n_keys, 1)
})

test_that("plot.check_na_gapsize facets only with several groups", {
  expect_s3_class(
    plot(make_check_na_gapsize(af_na_single()))$facet,
    "FacetNull"
  )
  expect_s3_class(plot(make_check_na_gapsize(af_na_multi()))$facet, "FacetWrap")
})

test_that("plot.check_na_gapsize handles data with no gaps", {
  expect_s3_class(plot(make_check_na_gapsize(af_na_none())), "ggplot")
})

test_that("as_plot_data.check_na_gapsize returns the staged class", {
  pd <- as_plot_data(make_check_na_gapsize(af_na_single()))
  expect_s3_class(pd, "anivis_check_na_gapsize_data")
  expect_s3_class(pd$key, "factor")
  expect_setequal(levels(pd$series), c("occurrence", "total"))
})


# --- plot.check_confidence ---------------------------------------------------

test_that("plot.check_confidence returns a ggplot with a violin polygon", {
  p <- plot(make_check_confidence(af_conf_single()))
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% geoms)
  expect_true("GeomPoint" %in% geoms) # median marker
})

test_that("plot.check_confidence puts keypoint on the axis, not a facet", {
  p <- plot(make_check_confidence(af_conf_single()))
  expect_s3_class(p$facet, "FacetNull")
  expect_false(isTRUE(attr(
    as_plot_data(make_check_confidence(af_conf_single())),
    "facet"
  )))
})

test_that("plot.check_confidence facets the second varying identity", {
  pd <- as_plot_data(make_check_confidence(af_conf_facet()))
  expect_true(isTRUE(attr(pd, "facet")))
  expect_equal(attr(pd, "axis_var"), "keypoint")
  expect_s3_class(
    plot(make_check_confidence(af_conf_facet()))$facet,
    "FacetWrap"
  )
})

test_that("plot.check_confidence clip = 0 keeps the full density", {
  full <- as_plot_data(make_check_confidence(af_conf_single()), clip = 0)
  clipped <- as_plot_data(make_check_confidence(af_conf_single()), clip = 0.02)
  expect_gte(nrow(full), nrow(clipped))
  expect_s3_class(
    plot(make_check_confidence(af_conf_single()), clip = 0),
    "ggplot"
  )
})

test_that("plot.check_confidence handles a degenerate single-value group", {
  af <- aniframe::as_aniframe(data.frame(time = 1:5, x = 1:5, y = 1:5))
  af$confidence <- rep(0.5, 5)
  ck <- make_check_confidence(af)
  expect_true(is.na(attr(as_plot_data(ck), "axis_var")))
  expect_s3_class(plot(ck), "ggplot")
})

test_that("plot.check_confidence axis falls back to a non-keypoint identity", {
  # An aniframe whose only *varying* what-variable is `individual` (keypoint is
  # just one possible identity, not a requirement): the y axis is `individual`.
  af <- aniframe::as_aniframe(data.frame(
    individual = rep(c("rat1", "rat2"), each = 60),
    time = rep(1:60, 2),
    x = rnorm(120),
    y = rnorm(120)
  ))
  af$confidence <- runif(120, 0.2, 1)
  ck <- make_check_confidence(af)
  pd <- as_plot_data(ck)
  expect_equal(attr(pd, "axis_var"), "individual")
  expect_false(isTRUE(attr(pd, "facet")))
  expect_s3_class(plot(ck), "ggplot")
})

test_that("as_plot_data.check_confidence stages positions and an overlay", {
  pd <- as_plot_data(make_check_confidence(af_conf_single()))
  expect_s3_class(pd, "anivis_check_confidence_data")
  positions <- attr(pd, "positions")
  expect_setequal(names(positions), c("head", "tail"))
  overlay <- attr(pd, "overlay")
  expect_true(all(c("y", "median", "q25", "q75") %in% names(overlay)))
})

test_that("check_confidence object needs a confidence column", {
  af <- aniframe::as_aniframe(data.frame(time = 1:5, x = 1:5, y = 1:5))
  expect_error(make_check_confidence(af), "confidence")
})


# --- dark mode + theme_imputets ----------------------------------------------

test_that("check plots build in dark mode", {
  expect_s3_class(
    plot(make_check_na_timing(af_na_single()), mode = "dark"),
    "ggplot"
  )
  expect_s3_class(
    plot(make_check_na_gapsize(af_na_single()), mode = "dark"),
    "ggplot"
  )
  expect_s3_class(
    plot(make_check_confidence(af_conf_single()), mode = "dark"),
    "ggplot"
  )
})

test_that("theme_imputets returns a theme in both modes", {
  expect_s3_class(theme_imputets(mode = "light"), "theme")
  expect_s3_class(theme_imputets(mode = "dark"), "theme")
  expect_s3_class(theme_imputets(base_size = 16), "theme")
})
