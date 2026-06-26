#' Draw point events as dots or raster ticks
#'
#' A ggplot2 layer that renders instantaneous events at each event's
#' `start` time, with one row per `label` (or `channel`, ...) on the y
#' axis. Two styles are available via `style`:
#'
#' * `"point"` (default) — a dot at each event time.
#' * `"raster"` — a short vertical tick at each event time, the classic
#'   spike-raster look (more standard for e.g. neuron spike trains).
#'
#' When the input has a `type` column (e.g. an [aniframe::anievent()]),
#' rows with `type != "point"` are dropped — the point half of the
#' state/point pair complementing [geom_event_state()].
#'
#' The default mapping is only `aes(x = start)` — every other aesthetic
#' is up to the caller. Common patterns:
#'
#' * `aes(colour = label)` — colour each mark by behaviour.
#' * `aes(y = label)` (or `y = channel`, `y = subject`, ...) — spread
#'   marks across rows. With no `y` mapping all marks collapse onto a
#'   single line.
#'
#' Opinionated wrappers (themes, colour scales, sensible defaults) are
#' intended to live one layer up; this geom is deliberately bare.
#'
#' This is a thin wrapper around [ggplot2::GeomPoint] (`"point"`) or
#' [ggplot2::GeomSegment] (`"raster"`), with a `setup_data` step that
#' supplies a default `y` when none is mapped.
#'
#' @param mapping Aesthetic mapping. Merged on top of the defaults, so
#'   you only need to supply the aesthetics you want to override.
#' @param data A data frame (typically an `anievent`). If `NULL`, inherits
#'   from the parent `ggplot()` call. Either way, rows with
#'   `type != "point"` are dropped from this layer.
#' @param style Either `"point"` (default, a dot per event) or `"raster"`
#'   (a vertical tick per event).
#' @param size Dot size, for `style = "point"`. Default 1.8.
#' @param height Tick height in y-axis units (1 = the spacing between
#'   adjacent rows), for `style = "raster"`. Default 0.7.
#' @param na.rm,show.legend,inherit.aes,... Passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#' @export
geom_event_point <- function(
  mapping = NULL,
  data = NULL,
  ...,
  style = c("point", "raster"),
  size = 1.8,
  height = 0.7,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  rlang::check_installed("ggplot2", reason = "for `geom_event_point()`.")
  style <- match.arg(style)

  GeomEventPoint <- if (style == "raster") {
    ggplot2::ggproto(
      "GeomEventPoint",
      ggplot2::GeomSegment,
      required_aes = "x",
      optional_aes = "y",
      default_aes = ggplot2::aes(
        colour = "grey25",
        linewidth = 0.5,
        linetype = 1,
        alpha = NA
      ),
      draw_key = ggplot2::draw_key_vpath,
      extra_params = c("na.rm", "height"),
      setup_data = function(data, params) {
        h <- if (is.null(params$height)) 0.7 else params$height
        if (!"y" %in% names(data)) {
          data$y <- 1
        }
        data$xend <- data$x
        data$yend <- data$y + h / 2
        data$y <- data$y - h / 2
        data
      }
    )
  } else {
    ggplot2::ggproto(
      "GeomEventPoint",
      ggplot2::GeomPoint,
      required_aes = "x",
      optional_aes = "y",
      default_aes = ggplot2::aes(
        shape = 19,
        colour = "grey25",
        size = 1.8,
        fill = NA,
        alpha = NA,
        stroke = 0.5
      ),
      setup_data = function(data, params) {
        if (!"y" %in% names(data)) {
          data$y <- 1
        }
        data
      }
    )
  }

  default_mapping <- ggplot2::aes(x = .data$start)
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    default_mapping[names(mapping)] <- mapping
    mapping <- default_mapping
  }

  filter_point <- function(d) {
    if ("type" %in% names(d)) {
      d <- d[as.character(d$type) == "point", , drop = FALSE]
    }
    d
  }
  data_arg <- if (is.null(data)) {
    filter_point
  } else if (is.function(data)) {
    data_fn <- data
    function(d) filter_point(data_fn(d))
  } else {
    filter_point(data)
  }

  params <- if (style == "raster") {
    list(height = height, na.rm = na.rm, ...)
  } else {
    list(size = size, na.rm = na.rm, ...)
  }

  ggplot2::layer(
    geom = GeomEventPoint,
    stat = "identity",
    data = data_arg,
    mapping = mapping,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}
