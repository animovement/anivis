# Internal null-coalescing helper.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Internal: build a palette function from a named list of palettes.
# Mirrors see::`.retrieve_palette()` but warns via cli instead of insight.
# Returns a `function(n)` that interpolates the chosen colours with
# grDevices::colorRampPalette() — used by the Material / gradient-style scales.
retrieve_palette <- function(palette, palettes, reverse = FALSE, ...) {
  if (!palette %in% names(palettes)) {
    cli::cli_warn(c(
      "{.val {palette}} is not a valid palette name.",
      "i" = "It must be one of {.or {.val {names(palettes)}}}.",
      "i" = "Using {.val {names(palettes)[1]}} instead."
    ))
    palette <- names(palettes)[1]
  }
  pal <- palettes[[palette]]
  if (isTRUE(reverse)) {
    pal <- rev(pal)
  }
  grDevices::colorRampPalette(pal, ...)
}
