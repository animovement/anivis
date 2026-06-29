# Tests for plot.aniframe, plot_trajectory and palette_animovement.

make_aniframe_single <- function() {
  data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe()
}

make_aniframe_multi_keypoint <- function() {
  data.frame(
    keypoint = rep(c("head", "tail"), each = 10),
    time = rep(1:10, 2),
    x = rnorm(20),
    y = rnorm(20)
  ) |>
    aniframe::as_aniframe()
}

make_aniframe_multi_trial <- function() {
  data.frame(
    trial = rep(c(1L, 2L, 3L), each = 10),
    time = rep(1:10, 3),
    x = rnorm(30),
    y = rnorm(30)
  ) |>
    aniframe::as_aniframe(variables_when = c("trial", "time"))
}

make_aniframe_matrix <- function() {
  data.frame(
    individual = rep(c("a", "b"), each = 30),
    trial = rep(rep(c(1L, 2L, 3L), each = 10), 2),
    time = rep(1:10, 6),
    x = rnorm(60),
    y = rnorm(60)
  ) |>
    aniframe::as_aniframe(variables_when = c("trial", "time"))
}


# --- plot.aniframe / plot_trajectory basics ----------------------------------

test_that("plot.aniframe returns a patchwork object", {
  p <- plot(make_aniframe_single())
  expect_s3_class(p, "patchwork")
})

test_that("plot_trajectory returns a ggplot object", {
  p <- plot_trajectory(make_aniframe_single())
  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory errors when data is not an aniframe", {
  data <- data.frame(time = 1:10, x = rnorm(10), y = rnorm(10))
  expect_error(plot_trajectory(data), "must be an aniframe")
})

test_that("plot_trajectory contains geom_path", {
  p <- plot_trajectory(make_aniframe_single())
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPath" %in% geom_classes)
})

test_that("plot_trajectory uses coord_fixed", {
  p <- plot_trajectory(make_aniframe_single())
  # In ggplot2 4.0 coord_fixed() returns a CoordCartesian with ratio = 1
  # rather than a distinct CoordFixed class.
  expect_s3_class(p$coordinates, "CoordCartesian")
  expect_equal(p$coordinates$ratio, 1)
})

test_that("plot_trajectory honours unit_space in axis labels", {
  data <- make_aniframe_single() |>
    aniframe::set_unit_space("mm")
  p <- plot_trajectory(data)
  expect_match(p$labels$x, "mm")
  expect_match(p$labels$y, "mm")
})

test_that("plot_trajectory uses filled circle (21) and filled triangle (24) markers", {
  p <- plot_trajectory(make_aniframe_multi_keypoint())
  built <- ggplot2::ggplot_build(p)
  shapes <- unlist(lapply(built$data, function(d) {
    if ("shape" %in% names(d)) unique(d$shape) else NULL
  }))
  expect_true(21 %in% shapes)
  expect_true(24 %in% shapes)
})

test_that("plot_trajectory in dark mode applies a theme", {
  p <- plot_trajectory(make_aniframe_single(), mode = "dark")
  expect_s3_class(p, "ggplot")
})


# --- group key mode dispatch -------------------------------------------------

test_that("group keys drop the redundant axis in single-variable modes", {
  expect_equal(
    aniframe_group_keys(make_aniframe_multi_keypoint())$mode,
    "what"
  )
  expect_setequal(
    unique(aniframe_group_keys(make_aniframe_multi_keypoint())$group),
    c("head", "tail")
  )

  expect_equal(
    aniframe_group_keys(make_aniframe_multi_trial())$mode,
    "when"
  )
  expect_setequal(
    unique(aniframe_group_keys(make_aniframe_multi_trial())$group),
    c("1", "2", "3")
  )

  expect_equal(
    aniframe_group_keys(make_aniframe_matrix())$mode,
    "matrix"
  )
  expect_true(all(grepl(
    " :: ",
    unique(aniframe_group_keys(make_aniframe_matrix())$group)
  )))

  expect_equal(
    aniframe_group_keys(make_aniframe_single())$mode,
    "single"
  )
})


# --- palette_animovement dispatch --------------------------------------------

test_that("palette_animovement returns one colour per group in matrix mode", {
  pal <- palette_animovement(make_aniframe_matrix())
  expect_length(pal, 2 * 3)
  expect_true(all(grepl("^#", pal)))
  expect_true(all(grepl(" :: ", names(pal))))
})

test_that("palette_animovement uses qualitative hues across what (what mode)", {
  pal <- palette_animovement(make_aniframe_multi_keypoint())
  expect_length(pal, 2)
  expect_equal(length(unique(pal)), 2)
  expect_setequal(names(pal), c("head", "tail"))
})

test_that("palette_animovement uses qualitative hues across when (when mode)", {
  pal <- palette_animovement(make_aniframe_multi_trial())
  expect_length(pal, 3)
  expect_equal(length(unique(pal)), 3)
  expect_setequal(names(pal), c("1", "2", "3"))
})

test_that("palette_animovement returns a single hue for single-trajectory data", {
  pal <- palette_animovement(make_aniframe_single())
  expect_length(pal, 1)
  expect_equal(names(pal), "all")
})


# --- mode-specific path colour aesthetics ------------------------------------

test_that("plot_trajectory maps line colour to time in single mode", {
  p <- plot_trajectory(make_aniframe_single())
  path_layer <- p$layers[[1]]
  expect_equal(rlang::as_label(path_layer$mapping$colour), "time")
})

test_that("plot_trajectory maps line colour to group in what / when / matrix", {
  for (af in list(
    make_aniframe_multi_keypoint(),
    make_aniframe_multi_trial(),
    make_aniframe_matrix()
  )) {
    p <- plot_trajectory(af)
    path_layer <- p$layers[[1]]
    expect_equal(rlang::as_label(path_layer$mapping$colour), ".group")
  }
})

test_that("plot_trajectory maps time to alpha (with a legend) in what / when", {
  for (af in list(
    make_aniframe_multi_keypoint(),
    make_aniframe_multi_trial()
  )) {
    p <- plot_trajectory(af)
    expect_equal(rlang::as_label(p$layers[[1]]$mapping$alpha), "time")
    expect_equal(p$scales$get_scales("alpha")$name, "time")
  }
})

test_that("plot_trajectory keeps matrix lines solid (no alpha mapping)", {
  p <- plot_trajectory(make_aniframe_matrix())
  expect_null(p$layers[[1]]$mapping$alpha)
})


# --- multi-line plot still draws one path per group --------------------------

test_that("plot_trajectory builds one trajectory group per what x when combo", {
  p <- plot_trajectory(make_aniframe_matrix())
  groups <- unique(as.character(p$data$.group))
  expect_length(groups, 2 * 3)
})


# --- exported scale wrappers and themes --------------------------------------

test_that("exported scale wrappers return ggplot2 scales", {
  # Okabe-Ito (categorical, colour-blind safe)
  expect_s3_class(scale_colour_okabeito(), "ScaleDiscrete")
  expect_s3_class(scale_color_okabeito(), "ScaleDiscrete")
  expect_s3_class(scale_fill_okabeito(), "ScaleDiscrete")
  expect_s3_class(scale_colour_oi(), "ScaleDiscrete")

  # Material (discrete + continuous)
  expect_s3_class(scale_colour_material_d(), "ScaleDiscrete")
  expect_s3_class(scale_fill_material_d(), "ScaleDiscrete")
  expect_s3_class(scale_colour_material_c(), "ScaleContinuous")
  expect_s3_class(scale_fill_material_c(), "ScaleContinuous")
})

test_that("okabeito and material palettes generate colours", {
  expect_length(palette_okabeito()(3), 3)
  expect_true(all(grepl("^#", palette_okabeito()(3))))
  expect_length(palette_material("gradient")(256), 256)
  expect_setequal(
    names(okabeito_colors("orange", "blue")),
    c("orange", "blue")
  )
})

test_that("theme_animovement returns a theme in both modes", {
  expect_s3_class(theme_animovement(mode = "light"), "theme")
  expect_s3_class(theme_animovement(mode = "dark"), "theme")
  expect_s3_class(theme_animovement_light(), "theme")
  expect_s3_class(theme_animovement_dark(), "theme")
})


# --- edge cases --------------------------------------------------------------

test_that("trajectory_endpoints skips groups whose x/y are all NA", {
  data <- data.frame(
    keypoint = rep(c("head", "tail"), each = 5),
    time = rep(1:5, 2),
    x = c(rnorm(5), rep(NA_real_, 5)),
    y = c(rnorm(5), rep(NA_real_, 5))
  ) |>
    aniframe::as_aniframe()

  suppressWarnings({
    p <- plot_trajectory(data)
  })
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  markers <- p$layers[[which(geoms == "GeomPoint")[[1]]]]
  # Only the valid group is marked: start + end = 2 rows.
  expect_equal(nrow(markers$data), 2)
})

test_that("plot_trajectory handles a group whose time is a single value", {
  af <- make_aniframe_multi_keypoint()
  df <- as.data.frame(af)
  # Force the first keypoint's time column to a constant.
  df$time[df$keypoint == unique(df$keypoint)[1]] <- 1
  af <- aniframe::as_aniframe(df)

  expect_s3_class(plot_trajectory(af), "ggplot")
})


# --- missing-data gap bridges across all colour modes ------------------------

with_internal_gap <- function(df) {
  # Blank a run of frames in the middle of each group so trajectory_gaps()
  # emits a dashed connector segment.
  df$x[df$time %in% c(4, 5)] <- NA
  df$y[df$time %in% c(4, 5)] <- NA
  df
}

test_that("plot_trajectory bridges gaps with a dashed segment (single mode)", {
  af <- aniframe::as_aniframe(with_internal_gap(
    data.frame(time = 1:10, x = as.numeric(1:10), y = as.numeric(1:10))
  ))
  p <- plot_trajectory(af)
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  expect_true("GeomSegment" %in% geoms)
})

test_that("plot_trajectory bridges gaps in what mode (multi-keypoint)", {
  df <- with_internal_gap(data.frame(
    keypoint = rep(c("head", "tail"), each = 10),
    time = rep(1:10, 2),
    x = as.numeric(rep(1:10, 2)),
    y = as.numeric(rep(1:10, 2))
  ))
  p <- plot_trajectory(aniframe::as_aniframe(df))
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  expect_true("GeomSegment" %in% geoms)
})

test_that("plot_trajectory bridges gaps in matrix mode (what x when)", {
  df <- with_internal_gap(data.frame(
    keypoint = rep(c("head", "tail"), each = 30),
    trial = rep(rep(c(1L, 2L, 3L), each = 10), 2),
    time = rep(1:10, 6),
    x = as.numeric(rep(1:10, 6)),
    y = as.numeric(rep(1:10, 6))
  ))
  af <- aniframe::as_aniframe(df, variables_when = c("trial", "time"))
  p <- plot_trajectory(af)
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1]], character(1))
  expect_true("GeomSegment" %in% geoms)
})


# --- true-time-unit colour-bar / legend labels -------------------------------

test_that("plot_trajectory formats the time legend as HH:MM:SS for time units", {
  af <- make_aniframe_single() |> aniframe::set_unit_time("s")
  p <- plot_trajectory(af)
  lab_fun <- p$scales$get_scales("colour")$labels
  expect_true(is.function(lab_fun))
  expect_match(lab_fun(60), "00:01:00")
})

test_that("plot_trajectory formats the alpha time legend for time units (what)", {
  af <- make_aniframe_multi_keypoint() |> aniframe::set_unit_time("s")
  p <- plot_trajectory(af)
  lab_fun <- p$scales$get_scales("alpha")$labels
  expect_true(is.function(lab_fun))
  expect_match(lab_fun(60), "00:01:00")
})
