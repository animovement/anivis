#' Draw an Arena
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns the layers that draw an arena, to add to any ggplot before the
#' layers drawn on top of it. The layers ignore the plot's own aesthetics, so
#' they work whatever the plot maps.
#'
#' The colours are muted, after sports broadcast graphics: surfaces recede,
#' markings are low in contrast, and the data drawn over them keeps the strong
#' colours. Each arena has a default palette:
#'
#' * `"lab"`: an off-white floor inside slate walls, or navy in dark mode, to
#'   match [theme_animovement()].
#' * `"sport"`: muted grass, track, sand, concrete and landing mats, with white
#'   lines, and faint mowing stripes on pitches; deep tones in dark mode.
#'
#' `palette = "plain"` draws outlines only, for figures where colour is kept for
#' the data.
#'
#' @param arena An arena, such as from [arena_ymaze()] or [arena_football()].
#' @param palette The colours (default `NULL`, the arena's own style):
#'   `"lab"`, `"sport"` or `"plain"`, or a named list replacing single entries
#'   of the arena's own. The entries are the materials (`grass`, `track`,
#'   `sand`, `concrete`, `mat`), `stripe`, `stripe_alpha`, `floor`, `wall`,
#'   `zone`, `zone_alpha`, `line`, `line_alpha` and `label`.
#' @param mode Either `"light"` (default) or `"dark"`, matching
#'   [theme_animovement()].
#' @param linewidth Width of the markings (default `0.4`); walls are drawn
#'   2.5 times as wide.
#' @param labels A logical value (default `TRUE`) determining whether arm
#'   labels are drawn.
#'
#' @return A list of ggplot2 layers.
#'
#' @family arenas
#' @examples
#' library(ggplot2)
#' path <- data.frame(x = c(0, 0, 20, 30), y = c(0, 30, -10, -16))
#' ggplot(path, aes(x, y)) +
#'   geom_arena(arena_ymaze(arm_length = 40, arm_width = 8)) +
#'   geom_path() +
#'   coord_fixed()
#' @export
geom_arena <- function(
  arena,
  palette = NULL,
  mode = c("light", "dark"),
  linewidth = 0.4,
  labels = TRUE
) {
  ensure_is_arena(arena)
  mode <- match.arg(mode)
  pal <- arena_palette(arena, palette, mode)
  s <- arena$shapes
  of <- function(kind) s[s$kind == kind, , drop = FALSE]

  # `.colour` comes after `...` so that `colour = ` is never matched to it.
  layer <- function(geom, data, ..., .colour) {
    if (nrow(data) == 0 || is.na(.colour)) {
      return(NULL)
    }
    geom(
      data = data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$path),
      inherit.aes = FALSE,
      ...
    )
  }

  # Surfaces are drawn one path at a time, in order, so a later one covers an
  # earlier one. A fill scale is avoided, since the plot may have its own.
  surfaces <- of("surface")
  surface_layers <- lapply(unique(surfaces$path), function(p) {
    shape <- surfaces[surfaces$path == p, , drop = FALSE]
    fill <- pal[[shape$material[1]]]
    layer(
      ggplot2::geom_polygon,
      shape,
      .colour = fill,
      fill = fill,
      colour = NA
    )
  })

  layers <- c(
    surface_layers,
    list(
      layer(
        ggplot2::geom_polygon,
        of("stripe"),
        .colour = pal$stripe,
        fill = pal$stripe,
        alpha = pal$stripe_alpha,
        colour = NA
      ),
      layer(
        ggplot2::geom_polygon,
        of("wall"),
        .colour = pal$floor,
        fill = pal$floor,
        colour = NA
      ),
      layer(
        ggplot2::geom_polygon,
        of("zone"),
        .colour = pal$zone,
        fill = pal$zone,
        alpha = pal$zone_alpha,
        colour = NA
      ),
      layer(
        ggplot2::geom_path,
        of("line"),
        .colour = pal$line,
        colour = pal$line,
        alpha = pal$line_alpha,
        linewidth = linewidth
      ),
      # Walls are outlined as polygons, which join every corner; a closed path
      # leaves a seam where it starts.
      layer(
        ggplot2::geom_polygon,
        drop_closing_vertex(of("wall")),
        .colour = pal$wall,
        fill = NA,
        colour = pal$wall,
        linewidth = linewidth * 2.5,
        linejoin = "mitre"
      ),
      layer(
        ggplot2::geom_point,
        of("spot"),
        .colour = pal$line,
        colour = pal$line,
        alpha = pal$line_alpha,
        size = linewidth * 3
      )
    )
  )
  if (isTRUE(labels) && any(s$kind == "label")) {
    layers <- c(
      layers,
      list(ggplot2::geom_text(
        data = of("label"),
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        inherit.aes = FALSE,
        colour = pal$label,
        size = 3.5,
        fontface = "bold"
      ))
    )
  }
  Filter(Negate(is.null), layers)
}

# Internal ---------------------------------------------------------------

# Removes the last vertex of each path where it repeats the first.
drop_closing_vertex <- function(shapes) {
  if (nrow(shapes) == 0) {
    return(shapes)
  }
  last <- !duplicated(shapes$path, fromLast = TRUE)
  first <- shapes[!duplicated(shapes$path), , drop = FALSE]
  repeats <- last &
    shapes$x == first$x[match(shapes$path, first$path)] &
    shapes$y == first$y[match(shapes$path, first$path)]
  shapes[!repeats, , drop = FALSE]
}

# Every palette has every entry; NA means "not drawn".
arena_palettes <- function() {
  entries <- function(materials, ...) c(as.list(materials), list(...))
  none <- c(grass = NA, track = NA, sand = NA, concrete = NA, mat = NA)
  list(
    lab = list(
      # Tinted to theme_animovement(): slate text on white, navy #1A1A2E.
      light = entries(
        none,
        stripe = NA,
        stripe_alpha = 0,
        floor = "#F4F4F1",
        wall = "#738091",
        zone = "#8E99A6",
        zone_alpha = 0.22,
        line = "#A5AEB9",
        line_alpha = 1,
        label = "#738091"
      ),
      dark = entries(
        none,
        stripe = NA,
        stripe_alpha = 0,
        floor = "#252740",
        wall = "#8D91AB",
        zone = "#8D91AB",
        zone_alpha = 0.3,
        line = "#4B4F6B",
        line_alpha = 1,
        label = "#8D91AB"
      )
    ),
    # Muted after broadcast graphics: the ground recedes and the lines stay
    # quiet, so the data in front keeps the saturated colours.
    sport = list(
      light = entries(
        c(
          grass = "#AFC0A2",
          track = "#C99C8C",
          sand = "#E2D6BC",
          concrete = "#CBCBC6",
          mat = "#8FA3BD"
        ),
        stripe = "#FFFFFF",
        stripe_alpha = 0.07,
        floor = NA,
        wall = "#FFFFFF",
        zone = "#FFFFFF",
        zone_alpha = 0.15,
        line = "#FFFFFF",
        line_alpha = 0.85,
        label = "#FFFFFF"
      ),
      dark = entries(
        c(
          grass = "#27352C",
          track = "#3F2D2C",
          sand = "#3D3829",
          concrete = "#2F3034",
          mat = "#283244"
        ),
        stripe = "#FFFFFF",
        stripe_alpha = 0.03,
        floor = NA,
        wall = "#FFFFFF",
        zone = "#FFFFFF",
        zone_alpha = 0.08,
        line = "#FFFFFF",
        line_alpha = 0.35,
        label = "#FFFFFF"
      )
    ),
    plain = list(
      light = entries(
        none,
        stripe = NA,
        stripe_alpha = 0,
        floor = NA,
        wall = "grey45",
        zone = "grey50",
        zone_alpha = 0.15,
        line = "grey65",
        line_alpha = 1,
        label = "grey45"
      ),
      dark = entries(
        none,
        stripe = NA,
        stripe_alpha = 0,
        floor = NA,
        wall = "grey70",
        zone = "grey70",
        zone_alpha = 0.12,
        line = "grey45",
        line_alpha = 1,
        label = "grey70"
      )
    )
  )
}

arena_palette <- function(arena, palette, mode, call = rlang::caller_env()) {
  palettes <- arena_palettes()
  if (is.list(palette)) {
    unknown <- setdiff(names(palette), names(palettes$lab$light))
    if (length(unknown) > 0) {
      cli::cli_abort(
        "{.arg palette} sets unknown {cli::qty(unknown)}entr{?y/ies} {.val {unknown}}.",
        call = call
      )
    }
    base <- palettes[[arena$style]][[mode]]
    base[names(palette)] <- palette
    return(base)
  }
  name <- palette %||% arena$style
  if (!is.character(name) || length(name) != 1 || !name %in% names(palettes)) {
    cli::cli_abort(
      "{.arg palette} must be one of {.val {names(palettes)}}, or a named list.",
      call = call
    )
  }
  palettes[[name]][[mode]]
}
