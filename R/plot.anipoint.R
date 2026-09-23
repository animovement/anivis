#' Plot an anipoint Object
#'
#' Creates a visualization of movement data stored in an
#' [anicore::anipoint()]. Returns a patchwork object that can be combined with
#' additional plots.
#'
#' @param x An anipoint object.
#' @param ... Additional arguments passed to underlying plot functions.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [plot_trajectory()].
#'
#' @return A patchwork object.
#'
#' @export
plot.anipoint <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)

  p <- plot_trajectory(x, ..., mode = mode)

  patchwork::wrap_plots(p)
}
