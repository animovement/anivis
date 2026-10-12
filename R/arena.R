#' Create an Arena From Its Shapes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An arena is the space a recording was made in, such as a maze, an open field
#' or a pitch, drawn under trajectories by [plot_trajectory()] or added to any
#' plot with [geom_arena()]. The presets ([arena_ymaze()], [arena_football()],
#' ...) cover common arenas; `as_arena()` builds one from shapes of your own.
#'
#' Each shape is a run of vertices, one row each. Its `kind` sets how it is
#' drawn:
#'
#' * `"wall"`: a closed outline, filled with the floor colour and drawn heavier
#'   than markings.
#' * `"line"`: a marking, drawn as a path. Repeat the first vertex at the end
#'   to close it.
#' * `"zone"`: a region inside the arena, such as a closed arm, filled with a
#'   light tint.
#' * `"surface"`: ground beneath and around the arena, coloured by its
#'   `material`: `"grass"`, `"track"` (a synthetic running track), `"sand"`,
#'   `"concrete"` or `"mat"` (a landing mat). Surfaces are drawn in order, so
#'   a later one covers an earlier one, as an infield covers the track around
#'   it.
#' * `"stripe"`: a faint band on the surface, such as mowing stripes.
#' * `"spot"`: a single point.
#' * `"label"`: text at a point, from the `label` column.
#'
#' @param shapes A data frame with columns `element` (the name of each part),
#'   `kind` (one of the kinds above), `x` and `y`. An optional `path` column
#'   separates several pieces of one element, an optional `label` column holds
#'   the text of `"label"` shapes, and an optional `material` column the
#'   material of `"surface"` shapes (default grass).
#' @param unit The unit of `x` and `y`, such as `"cm"` or `"m"`.
#' @param name A short description, used when printing.
#' @param style The palette [geom_arena()] colours the arena with by default:
#'   `"lab"` (default) for setups in the lab, or `"sport"` for playing
#'   surfaces.
#'
#' @return An `anivis_arena`: a list of `shapes`, `unit`, `name` and `style`.
#'
#' @family arenas
#' @examples
#' box <- data.frame(
#'   element = "wall",
#'   kind = "wall",
#'   x = c(0, 30, 30, 0),
#'   y = c(0, 0, 20, 20)
#' )
#' as_arena(box, unit = "cm", name = "home cage")
#' @export
as_arena <- function(
  shapes,
  unit,
  name = "arena",
  style = c("lab", "sport")
) {
  style <- match.arg(style)
  if (!is.data.frame(shapes)) {
    cli::cli_abort("{.arg shapes} must be a data frame.")
  }
  missing <- setdiff(c("element", "kind", "x", "y"), names(shapes))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg shapes} is missing {.val {missing}}.")
  }
  unknown <- setdiff(unique(shapes$kind), arena_kinds())
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown shape {cli::qty(unknown)}kind{?s} {.val {unknown}}.",
      "i" = "A kind is one of {.val {arena_kinds()}}."
    ))
  }
  if (!is.character(unit) || length(unit) != 1) {
    cli::cli_abort("{.arg unit} must be a single string, such as {.val cm}.")
  }
  if (is.null(shapes$path)) {
    shapes$path <- 1L
  }
  if (is.null(shapes$label)) {
    shapes$label <- NA_character_
  }
  if (is.null(shapes$material)) {
    shapes$material <- NA_character_
  }
  is_surface <- shapes$kind == "surface"
  shapes$material[is_surface & is.na(shapes$material)] <- "grass"
  unknown <- setdiff(
    stats::na.omit(shapes$material[is_surface]),
    arena_materials()
  )
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown {cli::qty(unknown)}material{?s} {.val {unknown}}.",
      "i" = "A material is one of {.val {arena_materials()}}."
    ))
  }
  shapes <- data.frame(
    element = as.character(shapes$element),
    kind = as.character(shapes$kind),
    path = paste(shapes$element, shapes$path, sep = "/"),
    x = as.numeric(shapes$x),
    y = as.numeric(shapes$y),
    label = as.character(shapes$label),
    material = as.character(shapes$material)
  )
  structure(
    list(shapes = shapes, unit = unit, name = name, style = style),
    class = "anivis_arena"
  )
}

#' @export
print.anivis_arena <- function(x, ...) {
  s <- x$shapes
  first <- s[!duplicated(s$path), , drop = FALSE]
  kinds <- table(factor(first$kind, levels = arena_kinds()))
  kinds <- kinds[kinds > 0]
  cat(sprintf(
    "<anivis_arena> %s, in %s, %s style\n",
    x$name,
    x$unit,
    x$style
  ))
  cat(sprintf(
    "x %s to %s, y %s to %s\n",
    signif(min(s$x), 4),
    signif(max(s$x), 4),
    signif(min(s$y), 4),
    signif(max(s$y), 4)
  ))
  cat(paste(kinds, names(kinds), collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Plot an Arena
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draws an arena on its own, in its own unit, to check its shape and
#' placement before drawing data on it.
#'
#' @param x An arena.
#' @param ... Passed to [geom_arena()].
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [geom_arena()] and [theme_animovement()].
#' @param axes A logical value (default `TRUE`) determining whether the axes
#'   are drawn. Without them, the arena stands on the background alone.
#'
#' @return A ggplot object.
#'
#' @family arenas
#' @examples
#' plot(arena_ymaze(arm_length = 40, arm_width = 8))
#' plot(arena_football(), mode = "dark", axes = FALSE)
#' @export
plot.anivis_arena <- function(x, ..., mode = c("light", "dark"), axes = TRUE) {
  mode <- match.arg(mode)
  ggplot2::ggplot() +
    geom_arena(x, ..., mode = mode) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = paste0("x (", x$unit, ")"),
      y = paste0("y (", x$unit, ")")
    ) +
    theme_animovement(mode = mode) +
    theme_arena() +
    list(if (!isTRUE(axes)) theme_no_axes())
}

#' Place an Arena Over Data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Moves, turns and rescales an arena. Calibrating the data is usually the
#' better route: bring it into real units with a sensible origin
#' ([anicore::convert_unit_space()]), and an arena drops onto it unchanged.
#' `arena_transform()` is for drawing an arena over data that is not
#' calibrated yet, such as positions in pixels.
#'
#' The steps are applied in order: the unit conversion, `scale`, `rotate`
#' about the arena's origin, `flip_y`, and the shift to `origin`.
#'
#' @param arena An arena.
#' @param unit The unit to convert to (default `NULL`, no conversion). Between
#'   units of length the arena is rescaled. `"px"` and `"none"` only relabel
#'   the arena, so give the scale yourself, in `unit` per unit of the arena.
#' @param scale A multiplier, or one per axis as `c(x, y)` (default `1`).
#' @param rotate Rotation in degrees, counter-clockwise (default `0`).
#' @param flip_y A logical value (default `FALSE`) determining whether y is
#'   reversed, for data whose y axis points down, as in images.
#' @param origin Where the arena's origin lands, as `c(x, y)` (default
#'   `c(0, 0)`).
#'
#' @return An arena with its shapes moved, and its unit updated if `unit` is
#'   given.
#'
#' @family arenas
#' @examples
#' # A Y-maze filmed at 12 px per cm, centred at pixel (640, 360), with y down
#' ymaze <- arena_ymaze(arm_length = 40, arm_width = 8)
#' arena_transform(ymaze, unit = "px", scale = 12, flip_y = TRUE, origin = c(640, 360))
#' @export
arena_transform <- function(
  arena,
  unit = NULL,
  scale = 1,
  rotate = 0,
  flip_y = FALSE,
  origin = c(0, 0)
) {
  ensure_is_arena(arena)
  s <- arena$shapes
  if (!is.null(unit) && !identical(unit, arena$unit)) {
    f <- arena_unit_factor(arena$unit, unit)
    s$x <- s$x * f
    s$y <- s$y * f
    arena$unit <- unit
  }
  scale <- rep_len(scale, 2)
  x <- s$x * scale[1]
  y <- s$y * scale[2]
  a <- rotate * pi / 180
  rx <- x * cos(a) - y * sin(a)
  ry <- x * sin(a) + y * cos(a)
  if (isTRUE(flip_y)) {
    ry <- -ry
  }
  s$x <- rx + origin[1]
  s$y <- ry + origin[2]
  arena$shapes <- s
  arena
}

# Internal ---------------------------------------------------------------

arena_materials <- function() {
  c("grass", "track", "sand", "concrete", "mat")
}

arena_kinds <- function() {
  c("surface", "stripe", "wall", "zone", "line", "spot", "label")
}

ensure_is_arena <- function(
  arena,
  arg = rlang::caller_arg(arena),
  call = rlang::caller_env()
) {
  if (!inherits(arena, "anivis_arena")) {
    cli::cli_abort(
      "{.arg {arg}} must be an arena, such as from {.fn arena_rect}.",
      call = call
    )
  }
  invisible(arena)
}

# Metres per unit. "px" and "none" have no length, so converting to them only
# relabels the arena.
arena_unit_metres <- function() {
  c(
    nm = 1e-9,
    um = 1e-6,
    mm = 1e-3,
    cm = 1e-2,
    m = 1,
    km = 1e3,
    `in` = 0.0254,
    ft = 0.3048,
    yd = 0.9144,
    mi = 1609.344
  )
}

arena_unit_factor <- function(from, to, call = rlang::caller_env()) {
  if (to %in% c("px", "none")) {
    return(1)
  }
  metres <- arena_unit_metres()
  if (!from %in% names(metres) || !to %in% names(metres)) {
    cli::cli_abort(
      "Cannot convert an arena in {.val {from}} to {.val {to}}.",
      call = call
    )
  }
  metres[[from]] / metres[[to]]
}

# Drops the axes and the panel border, keeping the background. The x and y
# elements are blanked too, since theme_animovement() styles them separately.
theme_no_axes <- function() {
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
  )
}

# An arena is the reference, so the panel grid behind it goes.
theme_arena <- function() {
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )
}

# Shape builders. Every closed shape repeats its first vertex, so it is drawn
# whole as a path as well as a polygon.

arena_shape <- function(
  element,
  kind,
  x,
  y,
  path = 1L,
  label = NA_character_,
  material = NA_character_
) {
  data.frame(
    element = element,
    kind = kind,
    path = path,
    x = x,
    y = y,
    label = label,
    material = material
  )
}

arena_close <- function(xy) {
  list(x = c(xy$x, xy$x[1]), y = c(xy$y, xy$y[1]))
}

arena_rect_xy <- function(xmin, xmax, ymin, ymax) {
  arena_close(list(
    x = c(xmin, xmax, xmax, xmin),
    y = c(ymin, ymin, ymax, ymax)
  ))
}

arena_arc_xy <- function(x0, y0, r, from = 0, to = 2 * pi, n = 120) {
  t <- seq(from, to, length.out = n)
  list(x = x0 + r * cos(t), y = y0 + r * sin(t))
}

arena_xy <- function(element, kind, xy, ...) {
  arena_shape(element, kind, xy$x, xy$y, ...)
}
