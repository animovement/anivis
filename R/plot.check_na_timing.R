#' Plot the Distribution of Missing Values Over Time
#'
#' Renders a `check_na_timing()` result (from the anicheck package) as a stacked
#' bar chart of missing (`NA`) versus present values across successive time
#' intervals — an adaptation of `ggplot_na_distribution2()` from the imputeTS
#' package (Moritz & Gatscha, GPL-3). Each bar is one interval of the recording;
#' its split shows how much of that stretch was missing, so a run of tall
#' \dQuote{NA} bars marks a blackout while an even sprinkle marks scattered
#' dropouts. With more than one group, each gets its own stacked panel.
#'
#' The interval counts are reconstructed from the compact gap table in the check
#' object (no per-frame data needed). Missing uses the imputeTS indianred,
#' present the imputeTS steelblue, with the two named in the coloured subtitle
#' (so no legend is needed). Styling is [theme_imputets()]. The plot is built
#' from an intermediate frame of class `anivis_check_na_timing` produced by
#' [as_plot_data()] — the staging step that mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_na_timing` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param measure Either `"percent"` (default, share of each interval) or
#'   `"count"` (number of frames).
#' @param n_intervals Number of intervals to bin into. Default (`NULL`) uses
#'   Sturges' rule on the largest group's frame count.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_imputets()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_na_timing <- function(
  x,
  ...,
  measure = c("percent", "count"),
  n_intervals = NULL,
  mode = c("light", "dark")
) {
  measure <- match.arg(measure)
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x, measure = measure, n_intervals = n_intervals)
  group_levels <- attr(plot_df, "group_levels")
  interval_size <- attr(plot_df, "interval_size")

  # Format the x axis as HH:MM:SS for true time units (like plot_events), raw
  # otherwise.
  unit_chr <- attr(x, "unit_time") %||% NA_character_
  factor <- seconds_per_unit(unit_chr)
  x_labels <- if (!is.na(factor)) {
    function(b) format(hms::as_hms(round(b * factor)))
  } else {
    ggplot2::waiver()
  }

  cols <- imputets_colours()
  fills <- c(
    present = ggplot2::alpha(cols$nona, 0.45),
    missing = ggplot2::alpha(cols$na, 0.95)
  )

  y_scale <- if (measure == "percent") {
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(round(v * 100), "%"),
      expand = c(0, 0)
    )
  } else {
    ggplot2::scale_y_continuous(expand = c(0, 0))
  }
  y_lab <- if (measure == "percent") "Percent" else "Count"

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$x, y = .data$value, fill = .data$status)
  ) +
    # White borders separate adjacent interval bars (the imputeTS look).
    ggplot2::geom_col(
      ggplot2::aes(width = .data$width),
      position = "stack",
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_x_continuous(expand = c(0, 0), labels = x_labels) +
    y_scale +
    ggplot2::labs(
      x = paste0("Time Lapse (Interval Size: ", interval_size, ")"),
      y = y_lab,
      title = "Missing Values per Interval",
      subtitle = "Amount of {.na NA} and {.nona non-NA} for successive intervals"
    ) +
    theme_imputets(mode = mode) +
    ggplot2::theme(
      # No vertical gridlines; keep only the major horizontal ones.
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # Bars run edge to edge (no x expansion); widen the right margin so the
      # final axis label is not clipped.
      plot.margin = ggplot2::margin(10, 22, 10, 10)
    )

  if (length(group_levels) > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group), ncol = 1)
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @param n_intervals For `as_plot_data.check_na_timing()`: number of time
#'   intervals to bin into (default `NULL` uses Sturges' rule on the largest
#'   group).
#'
#' @details
#' `as_plot_data.check_na_timing()` reconstructs, from the compact gap table, the
#' missing / present frame counts per time interval — one row per
#' (group, interval, status). A single interval width (in frames) is chosen for
#' all groups (Sturges' rule by default) so bars line up across panels, and each
#' gap's overlap with each interval is counted, so no per-frame data is needed.
#' `value` is the share (`measure = "percent"`) or count (`"count"`), `width` the
#' interval's span in time units, and the frame interval size rides along as an
#' attribute. Returns a frame classed `anivis_check_na_timing`.
as_plot_data.check_na_timing <- function(
  x,
  ...,
  measure = c("percent", "count"),
  n_intervals = NULL
) {
  measure <- match.arg(measure)
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")
  step <- attr(x, "time_step")

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
  gaps_group <- if (nrow(x)) label(x) else character(0)

  # One interval width (in frames) for every group, so bars align across panels.
  max_frames <- max(groups$n_frames)
  ni <- if (!is.null(n_intervals)) {
    n_intervals
  } else {
    max(1L, ceiling(log2(max(max_frames, 2)) + 1))
  }
  interval_size <- max(1L, floor(max_frames / ni))

  per_group <- lapply(seq_len(nrow(groups)), function(i) {
    g <- label(groups)[i]
    n_frames <- groups$n_frames[i]
    t_min <- groups$time_min[i]

    sel <- gaps_group == g
    a <- round((x$start[sel] - t_min) / step) + 1 # gap start index
    b <- a + x$length[sel] - 1 # gap stop index

    breaks <- unique(c(seq(0, n_frames, by = interval_size), n_frames))

    do.call(
      rbind,
      lapply(seq_len(length(breaks) - 1L), function(k) {
        lo <- breaks[k]
        hi <- breaks[k + 1]
        size <- hi - lo
        missing <- if (length(a)) {
          min(size, sum(pmax(0, pmin(b, hi) - pmax(a, lo + 1) + 1)))
        } else {
          0
        }
        centre_time <- t_min + ((lo + hi) / 2 - 0.5) * step
        data.frame(
          group = g,
          x = centre_time,
          width = size * step,
          size = size,
          present = size - missing,
          missing = missing
        )
      })
    )
  })
  binned <- do.call(rbind, per_group)

  long <- rbind(
    data.frame(
      group = binned$group,
      x = binned$x,
      width = binned$width,
      status = "present",
      count = binned$present,
      size = binned$size
    ),
    data.frame(
      group = binned$group,
      x = binned$x,
      width = binned$width,
      status = "missing",
      count = binned$missing,
      size = binned$size
    )
  )
  long$value <- if (measure == "percent") long$count / long$size else long$count
  long$status <- factor(long$status, levels = c("present", "missing"))
  long$group <- factor(long$group, levels = group_levels)

  attr(long, "group_levels") <- group_levels
  attr(long, "measure") <- measure
  attr(long, "interval_size") <- interval_size
  class(long) <- c("anivis_check_na_timing", "data.frame")
  long
}
