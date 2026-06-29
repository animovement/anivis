# Metadata-driven per-group palette for aniframes. This is anivis' own palette
# (not from see): it reads an aniframe's `variables_what` / `variables_when`
# metadata and assigns one hue per `what` level and, where both axes vary, a
# light-to-dark shade per `when` level. Used by plot_trajectory().

# Internal: derive per-row group keys from an aniframe's metadata.
# Returns a list with the per-row `group`, `what` / `when` keys, the unique
# levels in order of first appearance, and a `mode` describing the grouping:
#   - "matrix": both what and when have >1 level
#   - "what":   only what has >1 level (drop redundant when from group key)
#   - "when":   only when has >1 level (drop redundant what from group key)
#   - "single": neither has >1 level
aniframe_group_keys <- function(data) {
  meta <- aniframe::get_metadata(data)
  what_cols <- intersect(meta$variables_what %||% character(), names(data))
  when_cols <- setdiff(
    intersect(meta$variables_when %||% character(), names(data)),
    "time"
  )

  paste_cols <- function(cols) {
    if (!length(cols)) {
      return(rep("", nrow(data)))
    }
    parts <- lapply(cols, function(col) as.character(data[[col]]))
    do.call(paste, c(parts, sep = " | "))
  }
  what_key <- paste_cols(what_cols)
  when_key <- paste_cols(when_cols)

  what_levels <- if (length(what_cols)) unique(what_key) else character()
  when_levels <- if (length(when_cols)) unique(when_key) else character()

  mode <- if (length(what_levels) > 1 && length(when_levels) > 1) {
    "matrix"
  } else if (length(what_levels) > 1) {
    "what"
  } else if (length(when_levels) > 1) {
    "when"
  } else {
    "single"
  }

  group <- switch(
    mode,
    matrix = paste(what_key, when_key, sep = " :: "),
    what = what_key,
    when = when_key,
    single = rep("all", nrow(data))
  )

  list(
    group = group,
    what = what_key,
    when = when_key,
    what_levels = what_levels,
    when_levels = when_levels,
    what_cols = what_cols,
    when_cols = when_cols,
    mode = mode
  )
}

# Internal: blend a colour toward white by `amount` in [0, 1].
lighten_colour <- function(col, amount = 0.5) {
  rgb_mat <- grDevices::col2rgb(col) / 255
  mixed <- rgb_mat + (1 - rgb_mat) * amount
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

# Internal: blend a colour toward black by `amount` in [0, 1].
darken_colour <- function(col, amount = 0.5) {
  rgb_mat <- grDevices::col2rgb(col) / 255
  mixed <- rgb_mat * (1 - amount)
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

#' Per-group colour palette for an aniframe
#'
#' Builds a named character vector mapping trajectory group keys to colours.
#' The colour scheme adapts to how many `what` and (non-time) `when` levels
#' the aniframe has:
#'
#' * **single** (one of each, or none): a single hue.
#' * **what**  (multiple `what`, one `when`): one qualitative hue per `what`
#'   level.
#' * **when**  (one `what`, multiple `when`): one qualitative hue per `when`
#'   level.
#' * **matrix** (multiple of both): one hue per `what` level, then a
#'   lighter-to-darker shade of that hue per `when` level.
#'
#' Names of the returned vector match the group keys returned by the internal
#' grouping logic — the redundant axis is dropped when only one side varies.
#'
#' @param data An aniframe.
#' @param palette Name of a qualitative palette accepted by
#'   [grDevices::hcl.colors()]. Default `"Dark 3"`.
#' @param single_hue Colour used when neither `what` nor `when` has more than
#'   one level.
#'
#' @return A named character vector of hex colours.
#'
#' @export
palette_animovement <- function(
  data,
  palette = "Dark 3",
  single_hue = "#3A6FB0"
) {
  keys <- aniframe_group_keys(data)

  switch(
    keys$mode,
    single = stats::setNames(single_hue, "all"),
    what = stats::setNames(
      grDevices::hcl.colors(length(keys$what_levels), palette = palette),
      keys$what_levels
    ),
    when = stats::setNames(
      grDevices::hcl.colors(length(keys$when_levels), palette = palette),
      keys$when_levels
    ),
    matrix = matrix_palette(keys, palette)
  )
}

# Internal: hue-per-`what` x shade-per-`when` palette for the matrix case.
matrix_palette <- function(keys, palette) {
  base_hues <- stats::setNames(
    grDevices::hcl.colors(length(keys$what_levels), palette = palette),
    keys$what_levels
  )

  pal <- character(0)
  for (w in keys$what_levels) {
    base_col <- base_hues[[w]]
    # Spread the `when` shades across a visible band of the same hue — a darker
    # to a moderately lighter version — instead of fading toward white, so even
    # the lightest shade stays easy to see (important for thin time-series
    # lines, where a near-white shade would disappear).
    shade_cols <- stats::setNames(
      grDevices::colorRampPalette(c(
        darken_colour(base_col, amount = 0.3),
        lighten_colour(base_col, amount = 0.4)
      ))(length(keys$when_levels)),
      keys$when_levels
    )
    for (wn in keys$when_levels) {
      pal[paste(w, wn, sep = " :: ")] <- shade_cols[[wn]]
    }
  }
  pal
}
