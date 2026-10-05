# Tests for plot_circular.

# A frame with an angular column `ang`, given in the frame's unit. `dirs`
# declares the axis directions, which set the drawing sense.
make_circ <- function(
  ang,
  unit = "rad",
  dirs = c(x = "right", y = "up"),
  keypoint = NULL
) {
  n <- length(ang)
  df <- data.frame(time = seq_len(n), x = rnorm(n), y = rnorm(n))
  if (!is.null(keypoint)) {
    df$keypoint <- keypoint
    df$time <- stats::ave(seq_len(n), keypoint, FUN = seq_along)
  }
  af <- anicore::as_anipoint(df)
  af$ang <- ang
  af <- anicore::set_metadata(af, unit_angle = unit)
  if (!is.null(dirs)) {
    af <- anicore::set_axis_directions(af, dirs)
  }
  af
}

# The wedges (layer 1) and mean lines (layer 2) as built.
built <- function(p, i = 1) ggplot2::ggplot_build(p)$data[[i]]

# The angular labels, with plotmath ones deparsed.
x_labels <- function(p) {
  labels <- p$scales$get_scales("x")$labels
  if (is.expression(labels)) vapply(labels, deparse, character(1)) else labels
}

# The bin holding the largest share.
peak <- function(p) {
  d <- built(p)
  d[which.max(d$ymax), ]
}

width10 <- 2 * pi / 36

test_that("plot_circular returns a radial ggplot of wedges and a mean line", {
  p <- plot_circular(make_circ(runif(50, -pi, pi)), variable = "ang")
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$coordinates, "CoordRadial")
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  expect_s3_class(p$layers[[2]]$geom, "GeomSegment")
  expect_length(p$layers, 2)
  expect_equal(nrow(built(p)), 36)
})

test_that("show_mean = FALSE leaves out the mean line", {
  p <- plot_circular(
    make_circ(runif(50, -pi, pi)),
    variable = "ang",
    show_mean = FALSE
  )
  expect_length(p$layers, 1)
})

test_that("shares sum to one per group", {
  p <- plot_circular(make_circ(runif(80, -pi, pi)), variable = "ang")
  wedges <- p$layers[[1]]$data
  expect_equal(sum(wedges$share), 1)
  expect_equal(sum(wedges$total), 80)
})

# --- units -------------------------------------------------------------------

test_that("degrees and radians give the same wedges, labelled in their unit", {
  rad <- c(0.1, 0.2, 1.6, 3.1, -2.0)
  p_rad <- plot_circular(make_circ(rad, "rad"), variable = "ang")
  p_deg <- plot_circular(make_circ(rad * 180 / pi, "deg"), variable = "ang")

  expect_equal(built(p_rad)$ymax, built(p_deg)$ymax)
  expect_equal(built(p_rad)$xmin, built(p_deg)$xmin)

  expect_equal(p_rad$labels$x, "ang (rad)")
  expect_equal(p_deg$labels$x, "ang (deg)")

  deg_labels <- p_deg$scales$get_scales("x")$labels
  rad_labels <- x_labels(p_rad)
  expect_true("90\u00b0" %in% deg_labels)
  expect_true("pi/2" %in% rad_labels)
})

test_that("a frame with no angular unit is read and labelled as radians", {
  af <- make_circ(c(0.1, 1.6), "none")
  p <- plot_circular(af, variable = "ang")
  expect_equal(p$labels$x, "ang")
  expect_true("pi/2" %in% x_labels(p))
})

test_that("labels follow the range the column uses", {
  signed <- plot_circular(make_circ(c(-1, 1), "rad"), variable = "ang")
  unsigned <- plot_circular(make_circ(c(1, 5), "rad"), variable = "ang")
  expect_true("-pi/2" %in% x_labels(signed))
  expect_true("3 * pi/2" %in% x_labels(unsigned))

  deg <- plot_circular(make_circ(c(-10, 10), "deg"), variable = "ang")
  expect_setequal(
    deg$scales$get_scales("x")$labels,
    paste0(c(0, 45, 90, 135, 180, -135, -90, -45), "\u00b0")
  )
})

test_that("pi_eighths_label() writes multiples of pi/4 in lowest terms", {
  expect_equal(
    pi_eighths_label(c(0, 1, 2, 3, 4, 6, -1, -2, -4)),
    c("0", "pi/4", "pi/2", "3*pi/4", "pi", "3*pi/2", "-pi/4", "-pi/2", "-pi")
  )
})

# --- wrapping ----------------------------------------------------------------

test_that("signed, [0, 2pi) and unwrapped angles give the same plot", {
  set.seed(1)
  ang <- runif(60, -pi, pi)
  p_signed <- plot_circular(make_circ(ang), variable = "ang")
  p_pos <- plot_circular(make_circ(ang %% (2 * pi)), variable = "ang")
  p_unwrapped <- plot_circular(make_circ(ang + 4 * pi), variable = "ang")
  expect_equal(built(p_signed)$ymax, built(p_pos)$ymax)
  expect_equal(built(p_signed)$ymax, built(p_unwrapped)$ymax)
  expect_equal(built(p_signed, 2)$x, built(p_pos, 2)$x)
})

test_that("a distribution straddling pi falls in one bin", {
  ang <- c(pi - 0.01, -pi + 0.01, pi, -pi + 0.02)
  p <- plot_circular(make_circ(ang), variable = "ang")
  wedges <- p$layers[[1]]$data
  expect_equal(sum(wedges$share > 0), 1)
  expect_equal(wedges$centre[wedges$share > 0], pi)
})

# --- sense -------------------------------------------------------------------

test_that("a counter-clockwise frame draws angles counter-clockwise", {
  # Position on the theta scale runs clockwise, so 30 degrees counter-clockwise
  # sits 3 bins before the bin centred on 0, which starts at position 0.
  p <- plot_circular(make_circ(rep(pi / 6, 10)), variable = "ang")
  expect_equal(peak(p)$xmin, 2 * pi - 3 * width10)
  expect_equal(built(p, 2)$x, width10 / 2 - pi / 6 + 2 * pi)
})

test_that("a clockwise frame mirrors it", {
  af <- make_circ(rep(pi / 6, 10), dirs = c(x = "right", y = "down"))
  expect_equal(anicore::get_angle_direction(af), "clockwise")
  p <- plot_circular(af, variable = "ang")
  expect_equal(peak(p)$xmin, 3 * width10)
  expect_equal(built(p, 2)$x, width10 / 2 + pi / 6)
})

test_that("0 points along +x whatever the sense", {
  ccw <- plot_circular(make_circ(c(0, 1)), variable = "ang")
  cw <- plot_circular(
    make_circ(c(0, 1), dirs = c(x = "right", y = "down")),
    variable = "ang"
  )
  # The bin centred on 0 starts at position 0, turned to 3 o'clock less half
  # a bin, clockwise from 12.
  expect_equal(ccw$coordinates$arc[1], pi / 2 - width10 / 2)
  expect_equal(cw$coordinates$arc[1], pi / 2 - width10 / 2)
})

test_that("a frame with unknown axis directions is drawn counter-clockwise", {
  af <- make_circ(rep(pi / 6, 10), dirs = NULL)
  expect_equal(anicore::get_angle_direction(af), "unknown")
  p <- plot_circular(af, variable = "ang")
  expect_equal(peak(p)$xmin, 2 * pi - 3 * width10)
})

# --- bins --------------------------------------------------------------------

test_that("bins sets the number of wedges", {
  p <- plot_circular(make_circ(runif(30, -pi, pi)), variable = "ang", bins = 8)
  expect_equal(nrow(built(p)), 8)
  expect_equal(p$coordinates$arc[1], pi / 2 - pi / 8)
})

test_that("binwidth is read in the frame's unit", {
  deg <- plot_circular(
    make_circ(runif(30, -180, 180), "deg"),
    variable = "ang",
    binwidth = 15
  )
  rad <- plot_circular(
    make_circ(runif(30, -pi, pi), "rad"),
    variable = "ang",
    binwidth = pi / 6
  )
  expect_equal(nrow(built(deg)), 24)
  expect_equal(nrow(built(rad)), 12)
})

test_that("bins and binwidth are validated", {
  af <- make_circ(runif(10, -pi, pi))
  expect_error(plot_circular(af, "ang", bins = 0), "whole number")
  expect_error(plot_circular(af, "ang", bins = 2.5), "whole number")
  expect_error(plot_circular(af, "ang", bins = c(4, 8)), "whole number")
  expect_error(plot_circular(af, "ang", bins = "a"), "whole number")
  expect_error(
    plot_circular(af, "ang", bins = 8, binwidth = 0.5),
    "only one of"
  )
  expect_error(plot_circular(af, "ang", binwidth = "a"), "single number")
  expect_error(plot_circular(af, "ang", binwidth = -1), "positive")
  expect_error(plot_circular(af, "ang", binwidth = 1), "whole number of bins")
  expect_error(
    plot_circular(make_circ(c(1, 2), "deg"), "ang", binwidth = 7),
    "whole number of bins"
  )
})

# --- weights -----------------------------------------------------------------

test_that("weight counts each angle by its weight", {
  af <- make_circ(c(0, 0, 0, pi / 2))
  af$w <- c(0, 0, 0, 1)
  p <- plot_circular(af, variable = "ang", weight = "w")
  wedges <- p$layers[[1]]$data
  expect_equal(wedges$share[wedges$centre == pi / 2], 1)
  expect_equal(p$labels$y, "share of angles, weighted by w")
  # The weighted mean points the same way.
  expect_equal(p$layers[[2]]$data$mean, pi / 2)
  expect_equal(p$layers[[2]]$data$resultant, 1)
})

test_that("rows with a missing weight are left out", {
  af <- make_circ(c(0, pi / 2))
  af$w <- c(NA, 2)
  p <- plot_circular(af, variable = "ang", weight = "w")
  expect_equal(sum(p$layers[[1]]$data$total), 2)
})

test_that("weight is validated", {
  af <- make_circ(c(0, 1))
  af$w <- c(-1, 1)
  af$tag <- c("a", "b")
  expect_error(plot_circular(af, "ang", weight = 1), "single column name")
  expect_error(plot_circular(af, "ang", weight = "nope"), "unknown column")
  expect_error(plot_circular(af, "ang", weight = "tag"), "numeric column")
  expect_error(plot_circular(af, "ang", weight = "w"), "not be negative")
})

# --- radial scale ------------------------------------------------------------

test_that("equal_area makes a wedge's area in the ring proportional to its share", {
  af <- make_circ(c(rep(0, 4), rep(pi / 2, 1)))
  for (inner in c(0, 0.25)) {
    p <- plot_circular(af, variable = "ang", inner_radius = inner)
    w <- p$layers[[1]]$data
    outer <- inner + (1 - inner) * w$r
    ring_area <- outer^2 - inner^2
    expect_equal(ring_area / max(ring_area), w$share / max(w$share))
    expect_equal(p$coordinates$inner_radius[1], inner * 0.4)
  }
})

test_that("equal_area = FALSE makes a wedge's length proportional to its share", {
  af <- make_circ(c(rep(0, 4), rep(pi / 2, 1)))
  p <- plot_circular(af, variable = "ang", equal_area = FALSE)
  w <- p$layers[[1]]$data
  expect_equal(w$r, w$share / max(w$share))
})

test_that("the radial axis is labelled in shares", {
  af <- make_circ(c(rep(0, 4), rep(pi / 2, 1)))
  p <- plot_circular(af, variable = "ang", equal_area = FALSE)
  y <- p$scales$get_scales("y")
  expect_equal(y$labels, paste0(c(20, 40, 60, 80), "%"))
  expect_equal(y$breaks, c(0.25, 0.5, 0.75, 1))
})

test_that("the mean line runs from the inner circle to the edge at the mean", {
  af <- make_circ(c(0, pi / 2))
  for (inner in c(0, 0.25)) {
    p <- plot_circular(af, variable = "ang", inner_radius = inner)
    means <- p$layers[[2]]$data
    expect_equal(means$mean, pi / 4)
    expect_equal(means$resultant, sqrt(0.5))
    line <- built(p, 2)
    # Radial position 0 is the inner circle and 1 the edge, whatever the
    # resultant length.
    expect_equal(line$y, 0)
    expect_equal(line$yend, 1)
    expect_equal(line$x, line$xend)
    expect_equal(line$x, width10 / 2 - pi / 4 + 2 * pi)
    expect_null(p$layers[[2]]$geom_params$arrow)
    expect_equal(p$layers[[2]]$aes_params$linewidth, 0.4)
  }
})

test_that("a near-uniform sample still gets a full-length mean line", {
  af <- make_circ(c(0, pi / 2, pi, 3 * pi / 2 + 0.1))
  p <- plot_circular(af, variable = "ang")
  expect_lt(p$layers[[2]]$data$resultant, 0.05)
  expect_equal(built(p, 2)$yend, 1)
})

test_that("equal_area, inner_radius and show_mean are validated", {
  af <- make_circ(c(0, 1))
  expect_error(plot_circular(af, "ang", equal_area = NA), "TRUE")
  expect_error(plot_circular(af, "ang", show_mean = "yes"), "TRUE")
  expect_error(plot_circular(af, "ang", inner_radius = 1), "inner_radius")
  expect_error(plot_circular(af, "ang", inner_radius = -0.1), "inner_radius")
  expect_error(plot_circular(af, "ang", inner_radius = "a"), "inner_radius")
  expect_error(
    plot_circular(af, "ang", inner_radius = c(0, 0.5)),
    "inner_radius"
  )
  expect_no_error(plot_circular(af, "ang", inner_radius = 0L))
})

# --- grouping ----------------------------------------------------------------

make_circ_keypoints <- function() {
  make_circ(
    c(rep(0, 10), rep(pi / 2, 10)),
    keypoint = rep(c("head", "tail"), each = 10)
  )
}

test_that("layout = 'facet' gives each group a panel and hides the legend", {
  p <- plot_circular(make_circ_keypoints(), variable = "ang")
  expect_s3_class(p$facet, "FacetWrap")
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$PANEL)), 2)
  expect_equal(nrow(b$data[[1]]), 2 * 36)
  expect_equal(nrow(b$data[[2]]), 2)
  # Each group's shares sum to one on its own.
  w <- p$layers[[1]]$data
  expect_equal(as.vector(tapply(w$share, w$keypoint, sum)), c(1, 1))
  expect_equal(p$guides$guides$fill, "none")
})

test_that("layout = 'facet' uses facet_grid when identity and trial both vary", {
  df <- data.frame(
    individual = rep(c("a", "b"), each = 20),
    trial = rep(rep(1:2, each = 10), 2),
    time = rep(1:10, 4),
    x = rnorm(40),
    y = rnorm(40)
  )
  af <- anicore::as_anipoint(df, variables_when = c("trial", "time"))
  af$ang <- runif(40, -pi, pi)
  p <- plot_circular(af, variable = "ang")
  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL)), 4)
})

test_that("a single group is not faceted", {
  p <- plot_circular(make_circ(c(0, 1)), variable = "ang")
  expect_s3_class(p$facet, "FacetNull")
})

test_that("layout = 'inline' overlays a coloured outline per group", {
  p <- plot_circular(make_circ_keypoints(), variable = "ang", layout = "inline")
  expect_s3_class(p$facet, "FacetNull")
  expect_s3_class(p$layers[[1]]$geom, "GeomPath")
  d <- built(p)
  expect_equal(length(unique(d$colour)), 2)
  # Along each bin and back to the start: 2 points a bin, plus 1.
  expect_equal(nrow(d), 2 * (2 * 36 + 1))
  expect_equal(length(unique(built(p, 2)$colour)), 2)
  expect_false(identical(p$guides$guides$colour, "none"))
})

# --- inputs ------------------------------------------------------------------

test_that("plot_circular validates data and variable", {
  df <- data.frame(time = 1:3, x = 1:3, y = 1:3, ang = 1:3)
  expect_error(plot_circular(df, variable = "ang"), "must be an aniframe")
  af <- make_circ(c(0, 1))
  af$tag <- c("a", "b")
  af$ang2 <- c(0, 1)
  expect_error(plot_circular(af), "variable.*required")
  expect_error(plot_circular(af, variable = "nope"), "unknown column")
  expect_error(plot_circular(af, variable = "tag"), "must be numeric")
  expect_error(
    plot_circular(af, variable = c("ang", "ang2")),
    "single column"
  )
})

test_that("a frame with nothing to draw still plots", {
  all_na <- make_circ(c(NA_real_, NA_real_))
  p <- plot_circular(all_na, variable = "ang")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(nrow(p$layers[[2]]$data), 0)

  empty <- make_circ(c(0, 1))[0, ]
  p <- plot_circular(empty, variable = "ang")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_true(all(p$layers[[1]]$data$share == 0))
  expect_equal(nrow(p$layers[[2]]$data), 0)
})

test_that("zero total weight gives empty wedges and no mean line", {
  af <- make_circ(c(0, 1))
  af$w <- c(0, 0)
  p <- plot_circular(af, variable = "ang", weight = "w")
  expect_true(all(p$layers[[1]]$data$share == 0))
  expect_equal(nrow(p$layers[[2]]$data), 0)
})

test_that("the mean line is in the theme's text colour, light and dark", {
  light <- plot_circular(make_circ(c(0, 1)), variable = "ang")
  dark <- plot_circular(make_circ(c(0, 1)), variable = "ang", mode = "dark")
  expect_equal(dark$theme$panel.background$fill, "#1A1A2E")
  expect_equal(light$layers[[2]]$aes_params$colour, "#2D3E50")
  expect_equal(dark$layers[[2]]$aes_params$colour, "#E0E0E0")
})

# --- with animetric ----------------------------------------------------------

test_that("plot_circular draws course and a declared heading from animetric", {
  skip_if_not_installed("animetric", "0.5.0.9006")
  kin <- anicore::example_anipoint(n_obs = 50, n_individuals = 1) |>
    anicore::convert_unit_angle("deg") |>
    animetric::add_orientation(
      from = "shoulder_right",
      to = "head",
      level = "keypoint"
    ) |>
    animetric::add_kinematics()

  course <- plot_circular(kin, variable = "course", weight = "speed")
  heading <- plot_circular(kin, variable = "heading")
  expect_equal(course$labels$x, "course (deg)")
  expect_equal(heading$labels$x, "heading (deg)")
  expect_no_error(ggplot2::ggplot_build(course))
  expect_no_error(ggplot2::ggplot_build(heading))
})
