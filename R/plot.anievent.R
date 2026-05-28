#' Plot an anievent Object
#'
#' Creates a visualization of state bouts stored in an [aniframe::anievent()]
#' using [geom_ethogram()]. Returns a patchwork object that can be combined
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
  p <- plot_ethogram(x, ..., mode = mode)
  patchwork::wrap_plots(p)
}

#' Plot an Ethogram
#'
#' Creates a ggplot ethogram of state bouts from an anievent. Each behaviour
#' `label` becomes a row on the y axis, bouts are coloured by `label`, and
#' the time axis is labelled with `unit_time` from metadata.
#'
#' The plot is faceted automatically:
#'
#' * Multiple `channel`s → one facet row per channel, with free y-scales so
#'   each channel's labels keep their own ranking.
#' * Multiple identities (any `variables_what` column with more than one
#'   level) → one facet column per identity. When both `channel` and
#'   identity vary, a `facet_grid()` is used; otherwise `facet_wrap()`.
#'
#' @param data An anievent object.
#' @param ... Additional arguments (currently unused).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @export
plot_ethogram <- function(data, ...) {
  UseMethod("plot_ethogram")
}

#' @rdname plot_ethogram
#' @export
plot_ethogram.default <- function(data, ..., mode = c("light", "dark")) {
  if (!aniframe::is_anievent(data)) {
    cli::cli_abort("{.arg data} must be an anievent.")
  }
  mode <- match.arg(mode)

  meta <- aniframe::get_metadata(data)
  unit <- meta$unit_time
  has_unit <- !is.null(unit) && !as.character(unit) %in% c("unknown", "frame")
  x_lab <- if (has_unit) paste0("time (", unit, ")") else "time"

  what_cols <- intersect(meta$variables_what %||% character(), names(data))
  multi_what_cols <- what_cols[vapply(
    what_cols,
    function(col) length(unique(data[[col]])) > 1,
    logical(1)
  )]
  multi_channel <- length(unique(data$channel)) > 1

  p <- ggplot2::ggplot(data) +
    geom_ethogram(
      ggplot2::aes(
        y = .data$label,
        fill = .data$label,
        colour = .data$label
      )
    ) +
    ggplot2::labs(x = x_lab, y = NULL) +
    scale_fill_animovement() +
    scale_colour_animovement() +
    theme_animovement(mode = mode) +
    ggplot2::guides(fill = "none", colour = "none")

  facet_layer <- ethogram_facets(multi_channel, multi_what_cols)
  if (!is.null(facet_layer)) {
    p <- p + facet_layer
  }

  p
}

# Internal: pick a facet layer based on which axes vary.
# Returns NULL when no faceting is needed.
ethogram_facets <- function(multi_channel, multi_what_cols) {
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
