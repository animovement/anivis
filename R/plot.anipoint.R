#' Plot an anipoint Object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Creates a visualization of movement data stored in an
#' [anicore::anipoint()]. Returns a patchwork object that can be combined with
#' additional plots.
#'
#' Today the figure is the trajectory alone, drawn by [plot_trajectory()]. What
#' `plot()` shows may change without a deprecation cycle, for example to add
#' speed and course traces beneath the path. Call [plot_trajectory()] directly
#' when you need exactly that plot.
#'
#' @param x An anipoint object.
#' @param ... Additional arguments passed to underlying plot functions.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [plot_trajectory()].
#'
#' @return A patchwork object.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 1)
#' plot(af)
#'
#' @export
plot.anipoint <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)

  p <- plot_trajectory(x, ..., mode = mode)

  patchwork::wrap_plots(p)
}
