#' Prepare a Check Result for Plotting
#'
#' Coerces a check object — such as the result of `check_na_timing()` (from the
#' anicheck package) — into the
#' plot-ready data frame that its `plot()` method draws from. This is the
#' staging step that sits between the diagnostic computation (the `check_*()`
#' function) and the ggplot: it reshapes the natural, analysis-friendly columns
#' of the check into the exact aesthetic contract a geom needs (a single
#' grouping factor, ordered fill levels, the right level ordering, and so on).
#'
#' It is the anivis analog of `data_plot()` in the \pkg{see} package. Most users
#' never call it directly — `plot()` calls it for you. Reach for it when you
#' want the precise rows a plot is built from, to inspect them or to assemble a
#' custom chart by hand.
#'
#' Methods exist for check objects with a single canonical plot (one object
#' class maps to one figure). Object types that can be plotted several different
#' ways — an aniframe, which feeds both [plot_trajectory()] and
#' [plot_timeseries()] — deliberately do not have a single method, since there
#' would be nothing for it to dispatch on to choose between those shapes.
#'
#' @param x A check object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame classed for the corresponding plot method (for example
#'   `anivis_check_na_timing`).
#'
#' @seealso [plot.check_na_timing()]
#'
#' @export
as_plot_data <- function(x, ...) {
  UseMethod("as_plot_data")
}

#' @rdname as_plot_data
#' @export
as_plot_data.default <- function(x, ...) {
  cli::cli_abort(
    "{.fun as_plot_data} has no method for {.cls {class(x)[1]}} objects."
  )
}
