# Material design colour palette, vendored from the see package
# (easystats/see, GPL-3). Used by anivis mainly through its continuous
# "gradient" palette (blue -> orange), the warm-to-cool ramp see showcases
# for continuous data. Warnings use cli instead of insight / datawizard.

material_colors_list <- c(
  red = "#f44336",
  pink = "#E91E63",
  purple = "#9C27B0",
  `deep purple` = "#673AB7",
  indigo = "#3F51B5",
  blue = "#2196F3",
  `light blue` = "#03A9F4",
  cyan = "#00BCD4",
  teal = "#009688",
  green = "#4CAF50",
  `light green` = "#8BC34A",
  lime = "#CDDC39",
  yellow = "#FFEB3B",
  amber = "#FFC107",
  orange = "#FF9800",
  `deep orange` = "#FF5722",
  brown = "#795548",
  grey = "#9E9E9E",
  `blue grey` = "#607D8B"
)

#' Material design colours
#'
#' Extract one or more colours from the Material design palette. Vendored from
#' the see package.
#'
#' @param ... Character names of colours to extract. If none are given, the
#'   full palette is returned.
#'
#' @return A named character vector of hex colours.
#' @export
material_colors <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(material_colors_list)
  }
  material_colors_list[cols]
}

material_palettes <- list(
  full = material_colors(),
  ice = material_colors("purple", "deep purple", "indigo", "blue", "light blue"),
  gradient = material_colors("blue", "orange"),
  rainbow = material_colors(
    "purple", "deep purple", "indigo", "blue", "light blue", "green",
    "light green", "lime", "amber", "orange", "deep orange", "red", "pink"
  ),
  contrast = material_colors("blue", "green", "amber", "purple", "red"),
  light = material_colors("light blue", "pink", "yellow", "light green", "orange"),
  complement = material_colors(
    "blue", "blue grey", "teal", "green", "light green", "yellow", "amber", "red"
  )
)

#' Material design colour palette
#'
#' Palette generator for the Material design colours. Vendored from the see
#' package.
#'
#' @param palette One of `"full"`, `"ice"`, `"gradient"`, `"rainbow"`,
#'   `"contrast"`, `"light"` or `"complement"`.
#' @param reverse Reverse the colour order.
#'
#' @return A function that returns `n` hex colours.
#' @export
palette_material <- function(palette = "contrast", reverse = FALSE, ...) {
  retrieve_palette(palette, material_palettes, reverse = reverse, ...)
}

#' Material design colour scales
#'
#' Colour and fill scales using the Material design palette, vendored from the
#' see package. With `discrete = FALSE` the palette is interpolated into a
#' smooth gradient — the default `"gradient"` palette runs blue to orange, the
#' warm-to-cool ramp suited to continuous data.
#'
#' @param palette Palette name (see [palette_material()]). Defaults to
#'   `"contrast"` when discrete and `"gradient"` when continuous.
#' @param discrete Treat the scale as discrete (`TRUE`) or continuous
#'   (`FALSE`).
#' @param reverse Reverse the colour order.
#' @param aesthetics Aesthetics this scale applies to.
#' @param ... Passed to the underlying ggplot2 scale.
#'
#' @return A ggplot2 scale.
#' @name scale_material
NULL

#' @rdname scale_material
#' @export
scale_colour_material <- function(
  palette = NULL,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "colour",
  ...
) {
  if (is.null(palette)) {
    palette <- if (discrete) "contrast" else "gradient"
  }
  pal <- palette_material(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}

#' @rdname scale_material
#' @export
scale_color_material <- scale_colour_material

#' @rdname scale_material
#' @export
scale_fill_material <- function(
  palette = NULL,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "fill",
  ...
) {
  if (is.null(palette)) {
    palette <- if (discrete) "contrast" else "gradient"
  }
  pal <- palette_material(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}

#' @rdname scale_material
#' @export
scale_colour_material_d <- function(palette = NULL, ...) {
  scale_colour_material(palette = palette, discrete = TRUE, ...)
}

#' @rdname scale_material
#' @export
scale_color_material_d <- scale_colour_material_d

#' @rdname scale_material
#' @export
scale_colour_material_c <- function(palette = NULL, ...) {
  scale_colour_material(palette = palette, discrete = FALSE, ...)
}

#' @rdname scale_material
#' @export
scale_color_material_c <- scale_colour_material_c

#' @rdname scale_material
#' @export
scale_fill_material_d <- function(palette = NULL, ...) {
  scale_fill_material(palette = palette, discrete = TRUE, ...)
}

#' @rdname scale_material
#' @export
scale_fill_material_c <- function(palette = NULL, ...) {
  scale_fill_material(palette = palette, discrete = FALSE, ...)
}
