# Tests for plotting a frame with nothing to draw (#32)
# -----------------------------------------------------
# Both cases reach the same code: trajectory_endpoints() finds no group with a
# valid point, do.call(rbind, list()) gives NULL, and nrow(NULL) as `each`
# produced a length-2 column against a length-0 one.

base_frame <- function() {
  anicore::example_aniframe(n_individuals = 1, n_keypoints = 1, n_obs = 5)
}

test_that("an aniframe with no rows can be plotted", {
  empty <- dplyr::filter(base_frame(), FALSE)

  expect_no_error(plot(empty))
  expect_no_warning(plot(empty))
})

test_that("a keypoint that was never tracked can be plotted", {
  # More likely than an empty frame: every position is NA, so there is no
  # start or end to mark.
  all_na <- dplyr::mutate(base_frame(), x = NA_real_, y = NA_real_)

  expect_no_error(plot(all_na))
  expect_no_warning(plot(all_na))
})

test_that("trajectory_endpoints() returns a shaped frame when there is nothing to mark", {
  df <- data.frame(
    .group = character(0),
    time = numeric(0),
    x = numeric(0),
    y = numeric(0)
  )
  out <- trajectory_endpoints(df)

  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_named(out, c(".group", "x_start", "y_start", "x_end", "y_end"))
})
