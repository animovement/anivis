# Tests for plot.aniframe and plot_trajectory
#
# plot.aniframe:
# - returns a patchwork object
# - passes ... to plot_trajectory
#
# plot_trajectory:
# - returns a ggplot object
# - errors when data is not an aniframe
# - plot contains geom_path
# - plot uses coord_fixed

test_that("plot.aniframe returns a patchwork object", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe()

  p <- plot(data)

  expect_s3_class(p, "patchwork")
})

test_that("plot_trajectory returns a ggplot object", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe()

  p <- plot_trajectory(data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory errors when data is not an aniframe", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )

  expect_error(
    plot_trajectory(data),
    "must be an aniframe"
  )
})

test_that("plot_trajectory contains geom_path", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe()

  p <- plot_trajectory(data)
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomPath" %in% geom_classes)
})

test_that("plot_trajectory uses coord_fixed", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe()

  p <- plot_trajectory(data)

  expect_s3_class(p$coordinates, "CoordFixed")
})

test_that("plot_trajectory uses coord_fixed", {
  data <- data.frame(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  ) |>
    aniframe::as_aniframe() |> 
    aniframe::set_unit_space("mm")

  p <- plot_trajectory(data)

  expect_s3_class(p$coordinates, "CoordFixed")
})