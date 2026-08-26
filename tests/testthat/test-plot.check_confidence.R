# The axis is the finest identity, whatever it is named (#21) ----

confidence_frame <- function(what1, what2, session = FALSE) {
  set.seed(1)
  n <- 48
  d <- data.frame(
    time = rep(seq_len(n / 4), 4),
    .a = rep(c("A", "A", "B", "B"), each = n / 4),
    .b = rep(c("p", "q", "p", "q"), each = n / 4),
    x = rnorm(n),
    y = rnorm(n),
    confidence = runif(n)
  )
  if (session) {
    d$session <- rep(c("s1", "s2"), each = n / 2)
  }
  names(d)[names(d) == ".a"] <- what1
  names(d)[names(d) == ".b"] <- what2
  aniframe::as_aniframe(d, variables_what = c(what1, what2))
}

test_that("the axis is the finest identity for recognised names", {
  pd <- as_plot_data(make_check_confidence(
    confidence_frame("individual", "keypoint")
  ))

  expect_equal(attr(pd, "axis_var"), "keypoint")
})

test_that("the axis is the finest identity for free-form names", {
  # Previously the fallback took `varying[[1]]` — the *coarsest* varying
  # column — so a frame declaring identity any other way put the axis and
  # the facets the wrong way round, silently (#21).
  pd <- as_plot_data(make_check_confidence(
    confidence_frame("animal", "bodypart")
  ))

  expect_equal(attr(pd, "axis_var"), "bodypart")
  expect_true(
    "animal" %in% attr(pd, "facet_vars") || is.null(attr(pd, "facet_vars"))
  )
})

test_that("temporal context never becomes the axis", {
  # `group_cols` is identity followed by temporal context, so simply taking
  # the last varying column would pick `session` here.
  pd <- as_plot_data(make_check_confidence(
    confidence_frame("animal", "bodypart", session = TRUE)
  ))

  expect_equal(attr(pd, "axis_var"), "bodypart")
})

test_that("a check object without the declarations still works", {
  # anicheck carries `variables_what` for this; objects from before it did
  # not, and `keypoint` stays the best guess available for them.
  chk <- make_check_confidence(confidence_frame("individual", "keypoint"))
  attr(chk, "variables_what") <- NULL

  expect_equal(attr(as_plot_data(chk), "axis_var"), "keypoint")
})
