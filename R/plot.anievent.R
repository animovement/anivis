#' Plot an anievent Object
#'
#' Creates a visualization of state and point events from an
#' [aniframe::anievent()] using [geom_event_state()] and
#' [geom_event_point()]. Returns a patchwork object that can be combined
#' with additional plots. The drawing itself is done by [plot_events()].
#'
#' @param x An anievent object.
#' @param ... Additional arguments passed to underlying plot functions.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A patchwork object.
#'
#' @seealso [plot_events()]
#'
#' @export
plot.anievent <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)
  p <- plot_events(x, ..., mode = mode)
  patchwork::wrap_plots(p)
}
