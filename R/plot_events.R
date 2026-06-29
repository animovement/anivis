#' Plot State and Point Events
#'
#' Creates a ggplot that overlays state events (durative bouts, drawn as
#' horizontal bars by [geom_event_state()]) and point events
#' (instantaneous occurrences, drawn as dots by
#' [geom_event_point()]) on a shared time axis. Each behaviour `label`
#' becomes a row on the y axis.
#'
#' Two dispatches:
#'
#' * **`plot_events.anievent()`** — auto-detects which rows are states
#'   and which are points by reading the `type` column. The `layout`
#'   argument chooses how channels and labels are arranged:
#'
#'     - `"facet"` (default): each `label` is a y-axis row, and multiple
#'       `channel`s become facet rows (with free y-scales). Any
#'       `variables_what` column with more than one level adds a facet
#'       column per identity — `facet_grid()` when both vary, otherwise
#'       `facet_wrap()`.
#'     - `"inline"` (ethogram): each `channel` is a single y-axis row and
#'       `label` moves to a colour legend, so all channels share one panel.
#'
#' * **`plot_events.default()`** — for callers without an anievent. Pass
#'   the state events as the first arg and the point events via `point`;
#'   either may be `NULL`. No automatic faceting.
#'
#' @param data An anievent object, or a data frame of state events for
#'   the default method.
#' @param point For the default method only: an optional data frame of
#'   point events. Ignored by `plot_events.anievent()`.
#' @param ... Additional arguments (currently unused).
#' @param layout For `plot_events.anievent()`: either `"facet"` (default,
#'   one y row per label with channels faceted) or `"inline"` (an
#'   ethogram — one y row per channel with labels in a colour legend).
#' @param point_style How point events are drawn: `"point"` (default,
#'   dots) or `"raster"` (vertical ticks, the classic spike-raster look).
#'   Passed to [geom_event_point()].
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @export
plot_events <- function(data, ...) {
  UseMethod("plot_events")
}

#' @rdname plot_events
#' @export
plot_events.anievent <- function(
  data,
  ...,
  layout = c("facet", "inline"),
  point_style = c("point", "raster"),
  mode = c("light", "dark")
) {
  layout <- match.arg(layout)
  point_style <- match.arg(point_style)
  mode <- match.arg(mode)
  meta <- aniframe::get_metadata(data)

  what_cols <- intersect(meta$variables_what %||% character(), names(data))
  multi_what_cols <- what_cols[vapply(
    what_cols,
    function(col) length(unique(data[[col]])) > 1,
    logical(1)
  )]
  multi_channel <- length(unique(data$channel)) > 1

  is_state <- as.character(data$type) == "state"
  state_df <- if (any(is_state)) data[is_state, , drop = FALSE] else NULL
  point_df <- if (any(!is_state)) data[!is_state, , drop = FALSE] else NULL

  # "inline" (ethogram): one y row per channel, labels in a colour legend.
  # "facet": one y row per label, channels split into facet rows.
  row <- if (layout == "inline") "channel" else "label"

  p <- events_base_plot(
    state = state_df,
    point = point_df,
    meta = meta,
    mode = mode,
    row = row,
    point_style = point_style
  )

  # In "inline" mode the channel is the y axis, so it is not also faceted.
  facet_channel <- multi_channel && layout == "facet"
  facet_layer <- events_facets(facet_channel, multi_what_cols)
  if (!is.null(facet_layer)) {
    p <- p + facet_layer
  }
  p
}

#' @rdname plot_events
#' @export
plot_events.default <- function(
  data = NULL,
  point = NULL,
  ...,
  point_style = c("point", "raster"),
  mode = c("light", "dark")
) {
  point_style <- match.arg(point_style)
  mode <- match.arg(mode)
  if (is.null(data) && is.null(point)) {
    cli::cli_abort(
      "Either {.arg data} (state events) or {.arg point} must be supplied."
    )
  }
  events_base_plot(
    state = data,
    point = point,
    mode = mode,
    point_style = point_style
  )
}

# Internal: shared ggplot construction for both methods.
#
# X-axis dispatch:
#   - true time units (s, m, h, ms, us, ns): values are converted to hms
#     so scale_x_time() renders HH:MM:SS ticks. No axis label, since the
#     format is self-explanatory.
#   - "frame": values are kept raw; the axis is labelled "time (frames)".
#   - "unknown" / NULL / anything else: values are kept raw; the axis is
#     labelled "time".
events_base_plot <- function(
  data = NULL,
  state = data,
  point = data,
  meta = NULL,
  mode,
  row = "label",
  point_style = "point"
) {
  unit <- if (!is.null(meta)) meta$unit_time else NULL
  unit_chr <- if (!is.null(unit)) as.character(unit) else NA_character_
  factor <- seconds_per_unit(unit_chr)
  use_hms <- !is.na(factor)

  if (use_hms) {
    state <- to_hms_columns(state, c("start", "stop"), factor)
    point <- to_hms_columns(point, "start", factor)
  }

  state <- reorder_label_if_numeric(state)
  point <- reorder_label_if_numeric(point)

  x_lab <- if (use_hms) {
    NULL
  } else if (identical(unit_chr, "frame")) {
    "time (frames)"
  } else {
    "time"
  }

  p <- ggplot2::ggplot()

  if (!is.null(state) && nrow(state)) {
    p <- p +
      geom_event_state(
        data = state,
        mapping = ggplot2::aes(
          y = .data[[row]],
          fill = .data$label
        )
      )
  }
  if (!is.null(point) && nrow(point)) {
    p <- p +
      geom_event_point(
        data = point,
        mapping = ggplot2::aes(y = .data[[row]], colour = .data$label),
        style = point_style
      )
  }

  x_scale <- if (use_hms) {
    ggplot2::scale_x_time()
  } else {
    ggplot2::scale_x_continuous()
  }

  # Colour by label with the colour-blind-safe Okabe-Ito palette, but skipping
  # its pale amber (index 4) and black (index 9) so thin point-event dots stay
  # legible. Beyond those 7 hues, fall back to an interpolated Material
  # "rainbow". State and point events share one global colour map so every
  # label is a distinct hue regardless of which scale draws it.
  okabeito_order <- c(1, 2, 3, 5, 6, 7, 8)
  state_labels <- if (!is.null(state)) {
    sort(unique(as.character(state$label)))
  } else {
    character()
  }
  point_labels <- if (!is.null(point)) {
    sort(unique(as.character(point$label)))
  } else {
    character()
  }
  all_labels <- union(state_labels, point_labels)
  n_labels <- length(all_labels)
  pal_fun <- if (n_labels > length(okabeito_order)) {
    palette_material("rainbow")
  } else {
    palette_okabeito(order = okabeito_order)
  }
  colour_map <- stats::setNames(pal_fun(n_labels), all_labels)

  # State events ride the fill aesthetic, point events the colour aesthetic, so
  # ggplot renders them as two separate legend categories. The legends only
  # appear when channels are the y rows (row != "label"); otherwise each label
  # is its own row and the legends are redundant.
  show_legend <- !identical(row, "label")
  fill_scale <- ggplot2::scale_fill_manual(
    values = colour_map,
    na.value = "grey70",
    name = "state events",
    guide = if (show_legend) ggplot2::guide_legend(order = 1) else "none"
  )
  colour_scale <- ggplot2::scale_colour_manual(
    values = colour_map,
    na.value = "grey40",
    name = "point events",
    guide = if (show_legend) ggplot2::guide_legend(order = 2) else "none"
  )

  p +
    fill_scale +
    colour_scale +
    x_scale +
    ggplot2::labs(x = x_lab, y = NULL) +
    theme_animovement(mode = mode) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

# Internal: if every `label` parses as a number, reorder the factor
# levels numerically so the y axis shows "2" between "1" and "10"
# rather than in lexical order ("1", "10", "2", ...). Leaves labels
# alone when even one value is non-numeric, and passes NULL through.
reorder_label_if_numeric <- function(data) {
  if (is.null(data) || !("label" %in% names(data))) {
    return(data)
  }
  lvls <- if (is.factor(data$label)) {
    levels(data$label)
  } else {
    unique(as.character(data$label))
  }
  if (!length(lvls)) {
    return(data)
  }
  numeric_vals <- suppressWarnings(as.numeric(lvls))
  if (any(is.na(numeric_vals))) {
    return(data)
  }
  data$label <- factor(
    as.character(data$label),
    levels = lvls[order(numeric_vals)]
  )
  data
}

# Internal: multiply raw numeric time columns by `factor` (seconds per
# native tick) and wrap them as hms so scale_x_time() can format them.
# `data` may be NULL, in which case it passes through unchanged.
to_hms_columns <- function(data, cols, factor) {
  if (is.null(data)) {
    return(data)
  }
  for (col in cols) {
    if (col %in% names(data)) {
      data[[col]] <- hms::as_hms(as.numeric(data[[col]]) * factor)
    }
  }
  data
}

# Internal: seconds per native tick of a true time unit. Returns NA_real_
# for any unit (including "frame", "unknown", and NA) that should not be
# rendered as HH:MM:SS.
seconds_per_unit <- function(unit) {
  if (is.na(unit %||% NA_character_)) {
    return(NA_real_)
  }
  switch(
    as.character(unit),
    h = 3600,
    m = 60,
    s = 1,
    ms = 1e-3,
    us = 1e-6,
    ns = 1e-9,
    NA_real_
  )
}

# Internal: pick a facet layer based on which axes vary.
# Returns NULL when no faceting is needed.
events_facets <- function(multi_channel, multi_what_cols) {
  if (multi_channel && length(multi_what_cols)) {
    return(ggplot2::facet_grid(
      rows = ggplot2::vars(.data$channel),
      cols = ggplot2::vars(!!!rlang::syms(multi_what_cols)),
      scales = "free_y"
    ))
  }
  if (multi_channel) {
    return(ggplot2::facet_wrap(
      ggplot2::vars(.data$channel),
      scales = "free_y",
      ncol = 1
    ))
  }
  if (length(multi_what_cols)) {
    return(ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(multi_what_cols)),
      ncol = 1
    ))
  }
  NULL
}
