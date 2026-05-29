#' Plot an anievent Object
#'
#' Creates a visualization of state and point events from an
#' [aniframe::anievent()] using [geom_event_state()] and
#' [geom_event_point()]. Returns a patchwork object that can be combined
#' with additional plots.
#'
#' @param x An anievent object.
#' @param ... Additional arguments passed to underlying plot functions.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A patchwork object.
#'
#' @export
plot.anievent <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)
  p <- plot_events(x, ..., mode = mode)
  patchwork::wrap_plots(p)
}

#' Plot State and Point Events
#'
#' Creates a ggplot that overlays state events (durative bouts, drawn as
#' horizontal bars by [geom_event_state()]) and point events
#' (instantaneous occurrences, drawn as vertical ticks by
#' [geom_event_point()]) on a shared time axis. Each behaviour `label`
#' becomes a row on the y axis.
#'
#' Two dispatches:
#'
#' * **`plot_events.anievent()`** — auto-detects which rows are states
#'   and which are points by reading the `type` column. Facet dispatch
#'   uses metadata (`variables_what`, `channel`):
#'
#'     - Multiple `channel`s → one facet row per channel, with free
#'       y-scales.
#'     - Any `variables_what` column with more than one level → one
#'       facet column per identity. When both vary, a `facet_grid()` is
#'       used; otherwise `facet_wrap()`.
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
plot_events.anievent <- function(data, ..., mode = c("light", "dark")) {
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

  p <- events_base_plot(
    state = state_df,
    point = point_df,
    meta = meta,
    mode = mode
  )

  facet_layer <- events_facets(multi_channel, multi_what_cols)
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
  mode = c("light", "dark")
) {
  mode <- match.arg(mode)
  if (is.null(data) && is.null(point)) {
    cli::cli_abort(
      "Either {.arg data} (state events) or {.arg point} must be supplied."
    )
  }
  events_base_plot(state = data, point = point, mode = mode)
}

# Internal: shared ggplot construction for both methods.
events_base_plot <- function(
  data = NULL,
  state = data,
  point = data,
  meta = NULL,
  mode
) {
  unit <- if (!is.null(meta)) meta$unit_time else NULL
  sampling_rate <- if (!is.null(meta)) meta$sampling_rate else NA_real_
  factor <- seconds_per_unit(unit, sampling_rate)
  use_time_scale <- !is.na(factor)

  if (use_time_scale) {
    state <- to_hms_columns(state, c("start", "stop"), factor)
    point <- to_hms_columns(point, "start", factor)
  }

  unit_chr <- if (!is.null(unit)) as.character(unit) else NA_character_
  x_lab <- if (!is.na(unit_chr) && unit_chr != "unknown") {
    paste0("time (", unit_chr, ")")
  } else {
    "time"
  }
  x_scale <- if (use_time_scale) {
    ggplot2::scale_x_time()
  } else {
    ggplot2::scale_x_continuous()
  }

  p <- ggplot2::ggplot()

  if (!is.null(state) && nrow(state)) {
    p <- p +
      geom_event_state(
        data = state,
        mapping = ggplot2::aes(
          y = .data$label,
          fill = .data$label,
          colour = .data$label
        )
      )
  }
  if (!is.null(point) && nrow(point)) {
    p <- p +
      geom_event_point(
        data = point,
        mapping = ggplot2::aes(y = .data$label, colour = .data$label)
      )
  }

  p +
    scale_fill_animovement() +
    scale_colour_animovement() +
    x_scale +
    ggplot2::labs(x = x_lab, y = NULL) +
    ggplot2::guides(fill = "none", colour = "none") +
    theme_animovement(mode = mode) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

# Internal: convert the given numeric time columns to hms by multiplying
# by `factor` (seconds per tick). Caller is responsible for ensuring
# `factor` is non-NA — `events_base_plot` only converts in that case.
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

# Internal: seconds per tick of the given aniframe `unit_time`. Returns
# `NA_real_` when the conversion is undefined: "frame" without a
# `sampling_rate`, or "unknown". Callers fall back to a continuous x
# scale in that case.
seconds_per_unit <- function(unit, sampling_rate = NA_real_) {
  unit_chr <- as.character(unit %||% "s")
  if (unit_chr == "frame") {
    if (!is.na(sampling_rate) && sampling_rate > 0) {
      return(1 / sampling_rate)
    }
    return(NA_real_)
  }
  switch(
    unit_chr,
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
