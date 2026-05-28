#' Draw state events as horizontal bars
#'
#' A ggplot2 layer that renders state (durative) events as rectangles
#' spanning each event's `start` -> `stop` on the x axis, with one row
#' per `label` on the y axis. When the input has a `type` column (e.g.
#' an [aniframe::anievent()]), rows with `type != "state"` are dropped.
#'
#' The default mapping is only `aes(xmin = start, xmax = stop)` — every
#' other aesthetic is up to the caller. Common patterns:
#'
#' * `aes(fill = label)` — colour each bout's interior by behaviour.
#' * `aes(colour = label)` — outline each bout (keeps sub-pixel-wide
#'   bouts visible at any zoom).
#' * `aes(y = label)` (or `y = channel`, `y = subject`, ...) — spread
#'   bouts across rows. With no `y` mapping all bouts collapse onto a
#'   single line.
#' * `+ ggplot2::facet_wrap(~ channel, scales = "free_y", ncol = 1)` —
#'   split channels into their own panels.
#'
#' Opinionated wrappers (themes, colour scales, sensible defaults) are
#' intended to live one layer up; this geom is deliberately bare.
#'
#' This is a thin wrapper around [ggplot2::GeomRect]: a subclass adds a
#' `setup_data` step that derives `ymin` / `ymax` from `y` + `height`, so
#' a discrete y scale keeps the label rows ranked correctly (including
#' under per-panel `scales = "free_y"`).
#'
#' @param mapping Aesthetic mapping. Merged on top of the defaults, so
#'   you only need to supply the aesthetics you want to override.
#' @param data A data frame (typically an `anievent`). If `NULL`, inherits
#'   from the parent `ggplot()` call. Either way, rows with
#'   `type != "state"` are dropped from this layer.
#' @param height Vertical extent of each bout bar, in y-axis units
#'   (1 = the spacing between adjacent label rows). Default 0.7.
#' @param na.rm,show.legend,inherit.aes,... Passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#' @export
geom_event_state <- function(
  mapping = NULL,
  data = NULL,
  ...,
  height = 0.7,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  rlang::check_installed("ggplot2", reason = "for `geom_event_state()`.")

  GeomEventState <- ggplot2::ggproto(
    "GeomEventState",
    ggplot2::GeomRect,
    required_aes = c("xmin", "xmax"),
    optional_aes = "y",
    default_aes = ggplot2::aes(
      colour = NA,
      fill = "grey35",
      linewidth = 0.3,
      linetype = 1,
      alpha = NA
    ),
    extra_params = c("na.rm", "height"),
    setup_data = function(data, params) {
      h <- if (is.null(params$height)) 0.7 else params$height
      if (!"y" %in% names(data)) {
        data$y <- 1
      }
      data$ymin <- data$y - h / 2
      data$ymax <- data$y + h / 2
      data
    }
  )

  default_mapping <- ggplot2::aes(
    xmin = .data$start,
    xmax = .data$stop
  )
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    default_mapping[names(mapping)] <- mapping
    mapping <- default_mapping
  }

  filter_state <- function(d) {
    if ("type" %in% names(d)) {
      d <- d[as.character(d$type) == "state", , drop = FALSE]
    }
    d
  }
  data_arg <- if (is.null(data)) {
    filter_state
  } else if (is.function(data)) {
    function(d) filter_state(data(d))
  } else {
    filter_state(data)
  }

  ggplot2::layer(
    geom = GeomEventState,
    stat = "identity",
    data = data_arg,
    mapping = mapping,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(height = height, na.rm = na.rm, ...)
  )
}
