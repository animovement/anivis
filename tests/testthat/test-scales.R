# Tests for the vendored colour palettes / scales (Okabe-Ito, Material), the
# palette-retrieval helper, and the plots() patchwork wrapper — focusing on the
# argument branches and warnings the higher-level plot tests do not reach.

# --- okabeito_colors ---------------------------------------------------------

test_that("okabeito_colors() returns the full ordered palette by default", {
  pal <- okabeito_colors()
  expect_length(pal, 9)
  expect_true(all(grepl("^#", pal)))
  # amber = TRUE by default swaps in the darkened "amber" yellow.
  expect_true("amber" %in% names(pal))
  expect_true("#F5C710" %in% unname(pal))
  expect_false("#F0E442" %in% unname(pal))
})

test_that("okabeito_colors(amber = FALSE) uses the bright original yellow", {
  pal <- okabeito_colors(amber = FALSE)
  expect_equal(unname(pal["yellow"]), "#F0E442")
  expect_false("#F5C710" %in% unname(pal))
})

test_that("okabeito_colors(original_names = TRUE) uses the Okabe & Ito names", {
  pal <- okabeito_colors(original_names = TRUE)
  expect_true("sky blue" %in% names(pal))
  expect_true("vermillion" %in% names(pal))
})

test_that("okabeito_colors(black_first = TRUE) puts black first", {
  expect_equal(names(okabeito_colors(black_first = TRUE))[1], "black")
})

test_that("oi_colors is an alias of okabeito_colors", {
  expect_identical(oi_colors("orange"), okabeito_colors("orange"))
})


# --- palette_okabeito --------------------------------------------------------

test_that("palette_okabeito generates the requested number of colours", {
  expect_length(palette_okabeito()(5), 5)
})

test_that("palette_okabeito reverses the order on request", {
  expect_equal(
    palette_okabeito(reverse = TRUE)(9),
    rev(palette_okabeito()(9))
  )
})

test_that("palette_okabeito warns and falls back on an unknown palette name", {
  expect_warning(
    pal <- palette_okabeito(palette = "nope"),
    "not a valid palette"
  )
  expect_length(pal(3), 3)
})

test_that("palette_okabeito rejects an out-of-range order", {
  expect_error(palette_okabeito(order = 10), "between 1 and 9")
  expect_error(palette_okabeito(order = "a"), "between 1 and 9")
})

test_that("palette_okabeito warns when more colours are requested than exist", {
  pal <- palette_okabeito(order = 1:3)
  expect_warning(out <- pal(5), "3 colours")
  expect_length(out, 3)
})


# --- okabeito scale wrappers / aliases ---------------------------------------

test_that("okabeito scale aliases all return discrete scales", {
  expect_s3_class(scale_colour_oi(), "ScaleDiscrete")
  expect_s3_class(scale_color_oi(), "ScaleDiscrete")
  expect_s3_class(scale_fill_oi(), "ScaleDiscrete")
})


# --- material_colors / palette_material --------------------------------------

test_that("material_colors() returns the full palette, names extract subsets", {
  expect_length(material_colors(), length(material_colors()))
  expect_gt(length(material_colors()), 1)
  expect_equal(unname(material_colors("blue")), "#2196F3")
})

test_that("palette_material warns and falls back on an unknown name", {
  # Exercises retrieve_palette()'s warning branch in palette_utils.R.
  expect_warning(
    pal <- palette_material(palette = "nope"),
    "not a valid palette"
  )
  expect_length(pal(4), 4)
})

test_that("palette_material reverses colours on request", {
  expect_equal(
    palette_material(palette = "gradient", reverse = TRUE)(2),
    rev(palette_material(palette = "gradient")(2))
  )
})


# --- material scale wrappers -------------------------------------------------

test_that("material scale wrappers honour discrete vs continuous", {
  expect_s3_class(scale_colour_material(discrete = TRUE), "ScaleDiscrete")
  expect_s3_class(scale_colour_material(discrete = FALSE), "ScaleContinuous")
  expect_s3_class(scale_fill_material(discrete = TRUE), "ScaleDiscrete")
  expect_s3_class(scale_fill_material(discrete = FALSE), "ScaleContinuous")
  # Aliases resolve to the same constructors.
  expect_s3_class(scale_color_material(), "ScaleDiscrete")
  expect_s3_class(scale_color_material_d(), "ScaleDiscrete")
  expect_s3_class(scale_color_material_c(), "ScaleContinuous")
  expect_s3_class(scale_fill_material_d(), "ScaleDiscrete")
})


# --- plots() wrapper ---------------------------------------------------------

make_two_plots <- function() {
  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
  list(p1, p2)
}

test_that("plots() combines plots into a patchwork", {
  ps <- make_two_plots()
  expect_s3_class(plots(ps[[1]], ps[[2]]), "patchwork")
})

test_that("plots() auto-tags panels with tags = TRUE", {
  ps <- make_two_plots()
  expect_s3_class(plots(ps[[1]], ps[[2]], tags = TRUE), "patchwork")
})

test_that("plots() accepts a custom character-vector tag sequence", {
  ps <- make_two_plots()
  # Exercises the length(tags) > 1 branch (tags wrapped in a list).
  expect_s3_class(
    plots(ps[[1]], ps[[2]], tags = c("i", "ii")),
    "patchwork"
  )
})

test_that("plots() accepts a single list of plots and a title", {
  expect_s3_class(
    plots(make_two_plots(), title = "Overview", guides = "collect"),
    "patchwork"
  )
})
