# Okabe-Ito qualitative colour palette, vendored from the see package
# (easystats/see, GPL-3) so anivis gains the palette without depending on the
# whole easystats stack. Behaviour and colour values mirror see; warnings use
# cli instead of insight / datawizard.

# Full named colour list, including both the plain-language names and the
# original Okabe & Ito names, plus the darkened "amber" yellow see prefers.
okabeito_colors_list <- c(
  orange = "#E69F00",
  `light blue` = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  red = "#D55E00",
  purple = "#CC79A7",
  grey = "#999999",
  black = "#000000",
  `sky blue` = "#56B4E9",
  `bluish green` = "#009E73",
  vermillion = "#D55E00",
  `reddish purple` = "#CC79A7",
  `dark yellow` = "#F5C710",
  amber = "#F5C710"
)

#' Okabe-Ito colours
#'
#' Extract one or more colours from the colour-blind-safe Okabe-Ito palette.
#' `oi_colors()` is an alias. Vendored from the see package.
#'
#' @param ... Character names of colours to extract (e.g. `"orange"`). If none
#'   are given, the full ordered palette is returned.
#' @param original_names Use the original Okabe & Ito colour names (e.g.
#'   `"sky blue"`, `"vermillion"`) rather than the plain-language ones.
#' @param black_first Place black first in the returned palette.
#' @param amber Use the slightly darkened "amber" yellow (`#F5C710`) instead of
#'   the original bright yellow (`#F0E442`), for better legibility.
#'
#' @return A named character vector of hex colours.
#' @export
okabeito_colors <- function(
  ...,
  original_names = FALSE,
  black_first = FALSE,
  amber = TRUE
) {
  cols <- c(...)
  if (length(cols)) {
    return(okabeito_colors_list[cols])
  }

  yellow_col <- if (isTRUE(amber)) "amber" else "yellow"

  if (isTRUE(original_names)) {
    cols <- c(
      "orange",
      "sky blue",
      "bluish green",
      yellow_col,
      "blue",
      "vermillion",
      "reddish purple",
      "grey",
      "black"
    )
  } else {
    cols <- c(
      "orange",
      "light blue",
      "green",
      yellow_col,
      "blue",
      "red",
      "purple",
      "grey",
      "black"
    )
  }

  if (isTRUE(black_first)) {
    cols <- union("black", cols)
  }

  okabeito_colors_list[cols]
}

#' @rdname okabeito_colors
#' @export
oi_colors <- okabeito_colors

# Named palettes mirroring see: amber vs original yellow, black first vs last.
okabeito_palettes <- list(
  full = okabeito_colors(black_first = FALSE, amber = TRUE),
  black_first = okabeito_colors(black_first = TRUE, amber = TRUE),
  full_original = okabeito_colors(black_first = FALSE, amber = FALSE),
  black_original = okabeito_colors(black_first = TRUE, amber = FALSE)
)

#' Okabe-Ito colour palette
#'
#' Palette generator for the Okabe-Ito colours, for use with ggplot2 scales.
#' Vendored from the see package.
#'
#' @param palette One of `"full"`, `"black_first"`, `"full_original"` or
#'   `"black_original"`.
#' @param reverse Reverse the colour order.
#' @param order Integer vector (within `1:9`) selecting and reordering colours.
#'
#' @return A function that returns `n` hex colours.
#' @export
palette_okabeito <- function(palette = "full", reverse = FALSE, order = 1:9) {
  if (!palette %in% names(okabeito_palettes)) {
    cli::cli_warn(c(
      "{.val {palette}} is not a valid palette name.",
      "i" = "It must be one of {.or {.val {names(okabeito_palettes)}}}.",
      "i" = "Using {.val full} instead."
    ))
    palette <- "full"
  }

  if (!is.numeric(order) || any(order < 1 | order > 9)) {
    cli::cli_abort("{.arg order} must be integers between 1 and 9.")
  }

  pal <- okabeito_palettes[[palette]][order]
  if (isTRUE(reverse)) {
    pal <- rev(pal)
  }

  function(n) {
    if (n > length(pal)) {
      cli::cli_warn(c(
        "This palette has {length(pal)} colours but {n} were requested.",
        "i" = "Returning {length(pal)} colours; consider a continuous scale."
      ))
      n <- length(pal)
    }
    unname(pal[seq_len(n)])
  }
}

#' Okabe-Ito colour scales
#'
#' Colour-blind-safe qualitative colour and fill scales using the Okabe-Ito
#' palette, vendored from the see package. `*_oi()` are shorter aliases.
#'
#' @inheritParams palette_okabeito
#' @param aesthetics Aesthetics this scale applies to.
#' @param ... Passed to [ggplot2::discrete_scale()].
#'
#' @return A ggplot2 scale.
#' @name scale_okabeito
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_color_okabeito()
NULL

#' @rdname scale_okabeito
#' @export
scale_colour_okabeito <- function(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "colour",
  ...
) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = palette_okabeito(
      palette = palette,
      reverse = reverse,
      order = order
    ),
    ...
  )
}

#' @rdname scale_okabeito
#' @export
scale_color_okabeito <- scale_colour_okabeito

#' @rdname scale_okabeito
#' @export
scale_fill_okabeito <- function(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "fill",
  ...
) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = palette_okabeito(
      palette = palette,
      reverse = reverse,
      order = order
    ),
    ...
  )
}

#' @rdname scale_okabeito
#' @export
scale_colour_oi <- scale_colour_okabeito

#' @rdname scale_okabeito
#' @export
scale_color_oi <- scale_colour_okabeito

#' @rdname scale_okabeito
#' @export
scale_fill_oi <- scale_fill_okabeito
