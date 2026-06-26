#' Plot the Timing of Missing Values
#'
#' Renders a `check_na_timing()` result (from the anicheck package) as a
#' missingness strip: `time` on the x
#' axis, one row per trajectory group, and each missing run drawn as a coloured
#' span. Empty stretches read as present data, so scanning left to right shows
#' *when* the gaps fall — a long block at the end looks very different from the
#' same frames scattered throughout. Groups with no gaps still appear as an empty
#' row, so a clean keypoint is visibly clean rather than absent.
#'
#' Missing runs use the Okabe-Ito vermillion. The x axis is formatted as
#' `HH:MM:SS` for true time units (as [plot_events()] does) and as raw frame
#' numbers otherwise.
#'
#' The plot is built from an intermediate, plot-ready data frame of class
#' `anivis_check_na_timing` produced by [as_plot_data()] — the staging step that
#' mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_na_timing` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_na_timing <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x)
  group_levels <- attr(plot_df, "group_levels")
  time_range <- attr(plot_df, "time_range")

  # Format the x axis like plot_trajectory(): keep the values numeric and format
  # the labels (HH:MM:SS for true time units), which avoids hms arithmetic on
  # the tile widths.
  unit_chr <- attr(x, "unit_time") %||% NA_character_
  factor <- seconds_per_unit(unit_chr)
  x_labels <- if (!is.na(factor)) {
    function(b) format(hms::as_hms(round(b * factor)))
  } else {
    ggplot2::waiver()
  }
  x_lab <- if (!is.na(factor)) {
    NULL
  } else if (identical(unit_chr, "frame")) {
    "time (frames)"
  } else {
    "time"
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$x, y = .data$group, width = .data$width)
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data$status),
      height = 0.7
    ) +
    ggplot2::scale_fill_manual(
      values = c(missing = "#D55E00"),
      name = NULL,
      drop = FALSE
    ) +
    ggplot2::scale_y_discrete(limits = group_levels) +
    ggplot2::scale_x_continuous(
      labels = x_labels,
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::expand_limits(x = time_range) +
    ggplot2::labs(x = x_lab, y = NULL, title = "Timing of missing values") +
    theme_animovement(mode = mode) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' @rdname as_plot_data
#' @export
#'
#' @details
#' `as_plot_data.check_na_timing()` turns the gap table from `check_na_timing()`
#' into the frame its strip plot is drawn from. Each gap becomes a tile centred
#' on the gap with a width spanning it (plus one frame step, so single-frame gaps
#' stay visible); the grouping columns that actually vary collapse into a single
#' `group` factor, reversed so the first group reads at the top. The full set of
#' groups (including gap-free ones) and the time range ride along as attributes
#' so the plot can show every row across the whole timeline. Returns a frame
#' classed `anivis_check_na_timing`.
as_plot_data.check_na_timing <- function(x, ...) {
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")
  step <- attr(x, "time_step")

  # Label only by the columns that actually vary across groups (else "all").
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

  # Reverse so the first group sits at the top of the strip (geom_tile draws the
  # first discrete level at the bottom).
  group_levels <- rev(unique(label(groups)))

  out <- data.frame(
    group = factor(label(x), levels = group_levels),
    x = (x$start + x$stop) / 2,
    width = (x$stop - x$start) + step,
    status = factor("missing", levels = "missing"),
    stringsAsFactors = FALSE
  )
  attr(out, "group_levels") <- group_levels
  attr(out, "time_range") <- attr(x, "time_range")
  class(out) <- c("anivis_check_na_timing", "data.frame")
  out
}
