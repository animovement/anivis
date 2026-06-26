#' Plot the Distribution of Missing-Value Gap Sizes
#'
#' Renders a `check_na_gapsize()` result (from the anicheck package) as a bar
#' chart of gap size against frequency: each bar is a run length, its height the
#' number of such gaps (`measure = "occurrence"`) or the total missing frames
#' they account for (`measure = "total"`). A tall bar at size 1 means scattered
#' single-frame dropouts; bars far out on the x axis mean long blackouts. With
#' more than one group, each gets its own stacked panel.
#'
#' Bars use the animovement single hue. The plot is built from an intermediate
#' frame of class `anivis_check_na_gapsize` produced by [as_plot_data()] — the
#' staging step that mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_na_gapsize` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param measure Either `"occurrence"` (default, count of gaps of each size) or
#'   `"total"` (missing frames = size x count).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_na_gapsize <- function(
  x,
  ...,
  measure = c("occurrence", "total"),
  mode = c("light", "dark")
) {
  measure <- match.arg(measure)
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x, measure = measure)
  group_levels <- attr(plot_df, "group_levels")

  y_lab <- if (measure == "occurrence") "number of gaps" else "missing frames"

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$gap_size, y = .data$value)
  ) +
    ggplot2::geom_col(fill = "#3A6FB0", width = 0.8) +
    ggplot2::labs(
      x = "gap size (frames)",
      y = y_lab,
      title = "Missing-value gap sizes"
    ) +
    theme_animovement(mode = mode) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  if (length(group_levels) > 1L) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$group),
        ncol = 1,
        scales = "free_y"
      )
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @param measure For `as_plot_data.check_na_gapsize()`: which count to put on
#'   the y axis — `"occurrence"` (number of gaps) or `"total"` (missing frames).
#'
#' @details
#' `as_plot_data.check_na_gapsize()` turns the gap-size table from
#' `check_na_gapsize()` into a tidy bar-chart frame: one row per (group, gap
#' size) with the chosen `measure` as `value`, `gap_size` an ascending factor so
#' every bar position lines up across panels, and the varying grouping columns
#' collapsed into a single `group` factor. Returns a frame classed
#' `anivis_check_na_gapsize`.
as_plot_data.check_na_gapsize <- function(
  x,
  ...,
  measure = c("occurrence", "total")
) {
  measure <- match.arg(measure)
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")

  varying <- group_cols[vapply(
    group_cols,
    function(col) length(unique(groups[[col]])) > 1L,
    logical(1)
  )]
  label <- function(tbl) {
    if (length(varying)) {
      do.call(
        paste,
        c(lapply(varying, function(col) as.character(tbl[[col]])), sep = " | ")
      )
    } else {
      rep("all", nrow(tbl))
    }
  }
  group_levels <- unique(label(groups))
  sizes <- sort(unique(x$gap_size))

  out <- data.frame(
    group = factor(label(x), levels = group_levels),
    gap_size = factor(as.character(x$gap_size), levels = as.character(sizes)),
    value = if (measure == "occurrence") x$n_gaps else x$n_na,
    stringsAsFactors = FALSE
  )
  attr(out, "group_levels") <- group_levels
  attr(out, "measure") <- measure
  class(out) <- c("anivis_check_na_gapsize", "data.frame")
  out
}
