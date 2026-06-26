#' Arrange multiple plots side by side
#'
#' A thin convenience wrapper around \pkg{patchwork} for composing several
#' plots into a single figure with optional sub-figure tags, a shared legend,
#' and an overall title / subtitle / caption. Vendored from the see package so
#' anivis plots can be combined without reaching for patchwork directly.
#'
#' @param ... Plots to combine, or a single `list` of plots.
#' @param n_rows,n_columns Number of rows / columns in the layout. By default
#'   patchwork chooses a roughly square grid.
#' @param guides How to treat legends: `"collect"` to pool identical legends
#'   into one, `"keep"` to leave them in place. Passed to
#'   [patchwork::wrap_plots()].
#' @param tags Sub-figure tags. `TRUE` labels panels `A`, `B`, `C`, ...; a
#'   character vector such as `c("1", "2")` or `"i"` chooses another sequence;
#'   `FALSE` (default) adds none.
#' @param tag_prefix,tag_suffix,tag_sep Strings placed before / after each tag,
#'   and the separator between nested tag levels.
#' @param title,subtitle,caption Overall annotations for the combined figure.
#' @param theme A ggplot2 theme applied to the annotations (e.g. the title).
#'
#' @return A patchwork object.
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p2 <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
#' plots(p1, p2, tags = TRUE)
#'
#' @export
plots <- function(
  ...,
  n_rows = NULL,
  n_columns = NULL,
  guides = NULL,
  tags = FALSE,
  tag_prefix = NULL,
  tag_suffix = NULL,
  tag_sep = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  theme = NULL
) {
  # Normalise the tag specification into patchwork's tag_levels.
  if (length(tags) == 1L && is.logical(tags)) {
    tags <- if (isTRUE(tags)) "A" else NULL
  } else if (length(tags) > 1L) {
    tags <- list(tags)
  }

  patchwork::wrap_plots(
    ...,
    nrow = n_rows,
    ncol = n_columns,
    guides = guides
  ) +
    patchwork::plot_annotation(
      tag_levels = tags,
      tag_prefix = tag_prefix,
      tag_suffix = tag_suffix,
      tag_sep = tag_sep,
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme
    )
}
