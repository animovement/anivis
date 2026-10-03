# Tests for plot.anivis_check_segment_length() and its as_plot_data() staging
# step. Check objects come from make_check_segment_length() in
# helper-check-objects.R, which vendors anicheck's computation.

# A chain a -> b -> c -> d per individual, each segment's length set directly
# (`a` sits `l_ab` left of `b`, `d` sits `l_cd` right of `c`; b -> c is 2), with
# a little jitter so every violin has a body.
seg_frame <- function(
  individuals = c("A", "B"),
  n = 60,
  stretch = 5:12,
  factor = 1.8,
  outlier = FALSE,
  structure_length = NULL
) {
  set.seed(1)
  one <- function(id) {
    l_ab <- 1 + stats::rnorm(n, sd = 0.02)
    l_cd <- 1.5 + stats::rnorm(n, sd = 0.03)
    l_cd[stretch] <- l_cd[stretch] * factor
    if (outlier) {
      l_cd[n] <- 30
    }
    data.frame(
      individual = id,
      keypoint = rep(c("a", "b", "c", "d"), each = n),
      time = rep(seq_len(n), 4),
      x = c(1 - l_ab, rep(1, n), rep(1, n), 1 + l_cd),
      y = c(rep(0, n), rep(0, n), rep(2, n), rep(2, n))
    )
  }
  af <- anicore::as_anipoint(
    do.call(rbind, lapply(individuals, one)),
    variables_what = c("individual", "keypoint")
  )
  segments <- data.frame(
    segment = c("ab", "bc", "cd"),
    from = c("a", "b", "c"),
    to = c("b", "c", "d")
  )
  if (!is.null(structure_length)) {
    segments$length <- structure_length
  }
  anicore::set_structure(af, anicore::anistructure(segments = segments))
}

layer_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
}

# --- as_plot_data.check_segment_length ----------------------------------------

test_that("as_plot_data.check_segment_length stages violins per segment", {
  chk <- make_check_segment_length(seg_frame())
  pd <- as_plot_data(chk)

  expect_s3_class(pd, "anivis_check_segment_length_data")
  expect_named(pd, c("x", "y", "segment", "group", "poly"))
  # Segments in the structure's order from the top, at integer positions.
  expect_equal(attr(pd, "positions"), c(ab = 3L, bc = 2L, cd = 1L))
  expect_setequal(unique(pd$segment), c("ab", "bc", "cd"))
  expect_setequal(unique(pd$group), c("A", "B"))
  expect_true(isTRUE(attr(pd, "facet")))
  # Each violin stays within its slot, and within the grid's range.
  for (s in c("ab", "bc", "cd")) {
    rows <- pd[pd$segment == s, ]
    expect_true(all(abs(rows$y - attr(pd, "positions")[[s]]) <= 0.4 + 1e-9))
  }
  expect_gte(min(pd$x), min(chk$value))
  expect_lte(max(pd$x), max(chk$value))
})

test_that("as_plot_data.check_segment_length overlays the relative quartiles", {
  chk <- make_check_segment_length(seg_frame())
  overlay <- attr(as_plot_data(chk), "overlay")
  groups <- attr(chk, "groups")

  expect_named(overlay, c("y", "group", "median", "q25", "q75", "share_off"))
  expect_equal(nrow(overlay), 6L)
  expect_equal(overlay$median, groups$relative_median)
  expect_equal(overlay$q25, groups$relative_q25)
  expect_equal(overlay$q75, groups$relative_q75)
  expect_equal(overlay$share_off, groups$share_off)
  expect_equal(overlay$y, rep(3:1, 2))
  expect_equal(overlay$group, rep(c("A", "B"), each = 3))
})

test_that("the guides sit at 1 plus and minus the tolerance", {
  pd <- as_plot_data(make_check_segment_length(seg_frame()))
  expect_equal(attr(pd, "tolerance"), 0.3)
  expect_equal(attr(pd, "guides"), c(0.7, 1.3))

  pd <- as_plot_data(make_check_segment_length(seg_frame(), tolerance = 0.1))
  expect_equal(attr(pd, "guides"), c(0.9, 1.1))

  # Any shortening is within a tolerance of 1, so only the upper line is left.
  pd <- as_plot_data(make_check_segment_length(seg_frame(), tolerance = 1))
  expect_equal(attr(pd, "guides"), 2)
  expect_equal(attr(pd, "clamp"), 3)
})

test_that("one individual is a single panel", {
  chk <- make_check_segment_length(seg_frame(individuals = "A"))
  pd <- as_plot_data(chk)

  expect_false(isTRUE(attr(pd, "facet")))
  expect_equal(unique(pd$group), "all")
  expect_s3_class(plot(chk)$facet, "FacetNull")
})

test_that("relative lengths beyond the clamp are flagged and capped", {
  chk <- make_check_segment_length(seg_frame(outlier = TRUE))
  pd <- as_plot_data(chk)

  expect_true(attr(pd, "clamped"))
  expect_lte(max(pd$x), 2)
  expect_lte(max(attr(pd, "overlay")$q75), 2)
  expect_equal(
    plot(chk)$labels$caption,
    "Relative lengths above 2 are drawn at 2."
  )

  plain <- make_check_segment_length(seg_frame())
  expect_false(attr(as_plot_data(plain), "clamped"))
  expect_null(plot(plain)$labels$caption)
})

test_that("a reference from the structure moves the violin off 1", {
  chk <- make_check_segment_length(
    seg_frame(individuals = "A", structure_length = c(NA, NA, 1))
  )
  overlay <- attr(as_plot_data(chk), "overlay")

  expect_equal(overlay$median[overlay$y == 1], 1.5, tolerance = 0.05)
  expect_equal(overlay$median[overlay$y == 3], 1, tolerance = 0.05)
})

test_that("clip trims the violins, and a clip above 1 leaves none", {
  chk <- make_check_segment_length(seg_frame())
  full <- as_plot_data(chk, clip = 0)
  clipped <- as_plot_data(chk, clip = 0.2)
  expect_gt(nrow(full), nrow(clipped))

  none <- as_plot_data(chk, clip = 2)
  expect_identical(nrow(none), 0L)
  expect_named(none, c("x", "y", "segment", "group", "poly"))
  expect_s3_class(plot(chk, clip = 2), "ggplot")
})

test_that("a segment never measured has nothing to draw", {
  af <- seg_frame(individuals = "A")
  d <- as.data.frame(af)
  d$x[d$keypoint == "d"] <- NA
  af <- anicore::set_structure(
    anicore::as_anipoint(d, variables_what = c("individual", "keypoint")),
    anicore::get_structure(af, "keypoint")
  )
  chk <- make_check_segment_length(af)
  pd <- as_plot_data(chk)

  expect_false("cd" %in% pd$segment)
  expect_equal(attr(pd, "overlay")$y, c(3L, 2L))
  # The segment keeps its place on the axis.
  expect_equal(names(attr(pd, "positions")), c("ab", "bc", "cd"))
  expect_no_warning(ggplot2::ggplot_build(plot(chk)))
})

test_that("an empty check stages and plots", {
  chk <- data.frame(
    individual = character(0),
    segment = character(0),
    value = numeric(0),
    density = numeric(0)
  )
  class(chk) <- c(
    "check_segment_length",
    "anivis_check_segment_length",
    "data.frame"
  )
  attr(chk, "group_cols") <- c("individual", "segment")
  attr(chk, "groups") <- data.frame(
    individual = character(0),
    segment = character(0),
    relative_q25 = numeric(0),
    relative_median = numeric(0),
    relative_q75 = numeric(0),
    relative_max = numeric(0),
    share_off = numeric(0)
  )
  attr(chk, "tolerance") <- 0.3
  attr(chk, "clamp") <- 2

  pd <- as_plot_data(chk)
  expect_identical(nrow(pd), 0L)
  expect_identical(nrow(attr(pd, "overlay")), 0L)
  expect_false(attr(pd, "clamped"))
  expect_s3_class(plot(chk), "ggplot")
})

# --- plot.anivis_check_segment_length -----------------------------------------

test_that("plot draws guides, violins, quartile lines and medians", {
  p <- plot(make_check_segment_length(seg_frame()))

  expect_s3_class(p, "ggplot")
  expect_equal(
    layer_geoms(p),
    c(
      "GeomVline",
      "GeomVline",
      "GeomPolygon",
      "GeomLinerange",
      "GeomPoint",
      "GeomText"
    )
  )
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(p$labels$x, "length relative to reference")
  expect_equal(p$labels$y, "segment")
  expect_match(p$labels$subtitle, "30% either side")

  built <- ggplot2::ggplot_build(p)
  # One panel per individual, stacked.
  expect_equal(nrow(built$layout$layout), 2L)
  expect_equal(built$data[[1]]$xintercept[1], 1)
  expect_equal(sort(unique(built$data[[2]]$xintercept)), c(0.7, 1.3))
  # The axis reads the segment names, the first at the top.
  y_scale <- built$layout$panel_scales_y[[1]]
  expect_equal(y_scale$get_breaks(), c(3, 2, 1))
  expect_equal(y_scale$get_labels(), c("ab", "bc", "cd"))
})

test_that("the share off is written beside each segment that has any", {
  chk <- make_check_segment_length(seg_frame())
  built <- ggplot2::ggplot_build(plot(chk))
  labels <- built$data[[6]]
  groups <- attr(chk, "groups")
  off <- groups[groups$share_off > 0, ]

  expect_equal(nrow(labels), nrow(off))
  expect_equal(
    labels$label,
    sprintf("%.1f%% off", 100 * off$share_off)
  )
  # cd, stretched in frames 5 to 12 of 60, in both panels.
  expect_true(all(labels$y == 1))
  expect_equal(labels$label, rep("13.3% off", 2))
})

test_that("an unstable segment's violin is the wide one", {
  pd <- as_plot_data(make_check_segment_length(seg_frame(factor = 1.6)))
  spread <- tapply(pd$x, pd$segment, function(x) diff(range(x)))

  expect_gt(spread[["cd"]], 3 * spread[["ab"]])
  expect_gt(spread[["cd"]], 3 * spread[["bc"]])
})

test_that("plot.anivis_check_segment_length builds in dark mode", {
  p <- plot(make_check_segment_length(seg_frame()), mode = "dark")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # Light ink for the medians, which grey20 would lose on the dark panel.
  expect_equal(unique(built$data[[5]]$colour), "grey85")
  light <- ggplot2::ggplot_build(plot(make_check_segment_length(seg_frame())))
  expect_equal(unique(light$data[[5]]$colour), "grey20")
})

test_that("plot draws a check from anicheck itself", {
  skip_if_not_installed("anicheck")
  skip_if_not(
    exists("check_segment_length", envir = asNamespace("anicheck")),
    "anicheck has no check_segment_length() yet"
  )
  chk <- anicheck::check_segment_length(seg_frame())
  ours <- make_check_segment_length(seg_frame())

  # The vendored builder in helper-check-objects.R stages identically.
  expect_equal(as_plot_data(chk), as_plot_data(ours))
  expect_s3_class(plot(chk), "ggplot")
})
