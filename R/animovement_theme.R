#' Animovement ggplot2 Theme
#'
#' A custom ggplot2 theme for animovement plots with light and dark variants.
#'
#' @param mode Either "light" (default) or "dark".
#' @param base_size Base font size.
#'
#' @return A ggplot2 theme object.
#' @export
theme_animovement <- function(mode = c("light", "dark"), base_size = 11) {
  mode <- match.arg(mode)

 if (mode == "light") {
    theme_animovement_light(base_size = base_size)
  } else {
    theme_animovement_dark(base_size = base_size)
  }
}

#' @rdname theme_animovement
#' @export
theme_animovement_light <- function(base_size = 11) {
 # Colours
 col_bg <- "#FFFFFF"
 col_panel <- "#FAFAFA"
 col_grid <- "#E5E5E5"
 col_text <- "#2D3E50"
 col_text_light <- "#5A6B7D"
 col_accent <- "#E74C3C"

 ggplot2::theme(
   # Base
   text = ggplot2::element_text(
     colour = col_text,
     size = base_size
   ),
   line = ggplot2::element_line(colour = col_grid),

   # Plot
   plot.background = ggplot2::element_rect(fill = col_bg, colour = NA),
   plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2),
   plot.subtitle = ggplot2::element_text(colour = col_text_light),
   plot.margin = ggplot2::margin(10, 10, 10, 10),

   # Panel
   panel.background = ggplot2::element_rect(fill = col_panel, colour = NA),
   panel.border = ggplot2::element_rect(fill = NA, colour = col_text, linewidth = 0.5),
   panel.grid.major = ggplot2::element_line(colour = col_grid, linewidth = 0.4),
   panel.grid.minor = ggplot2::element_blank(),

   # Axes
   axis.text = ggplot2::element_text(colour = col_text_light),
   axis.title = ggplot2::element_text(colour = col_text),
   axis.ticks = ggplot2::element_line(colour = col_text_light, linewidth = 0.4),

   # Legend
   legend.background = ggplot2::element_rect(fill = col_bg, colour = NA),
   legend.key = ggplot2::element_rect(fill = NA, colour = NA),

   # Facets
   strip.background = ggplot2::element_rect(fill = col_accent, colour = NA),
   strip.text = ggplot2::element_text(colour = "#FFFFFF", face = "bold")
 )
}

#' @rdname theme_animovement
#' @export
theme_animovement_dark <- function(base_size = 11) {
  # Colours
  col_bg <- "#1A1A2E"
  col_panel <- "#16213E"
  col_grid <- "#2A3A5E"
  col_text <- "#E0E0E0"
  col_text_light <- "#A0A0B0"
  col_accent <- "#F39C12"

  ggplot2::theme(
    # Base
    text = ggplot2::element_text(
      colour = col_text,
      size = base_size
    ),
    line = ggplot2::element_line(colour = col_grid),

    # Plot
    plot.background = ggplot2::element_rect(fill = col_bg, colour = NA),
    plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2),
    plot.subtitle = ggplot2::element_text(colour = col_text_light),
    plot.margin = ggplot2::margin(10, 10, 10, 10),

    # Panel
    panel.background = ggplot2::element_rect(fill = col_panel, colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = col_text_light, linewidth = 0.5),
    panel.grid.major = ggplot2::element_line(colour = col_grid, linewidth = 0.4),
    panel.grid.minor = ggplot2::element_blank(),

    # Axes
    axis.text = ggplot2::element_text(colour = col_text_light),
    axis.title = ggplot2::element_text(colour = col_text),
    axis.ticks = ggplot2::element_line(colour = col_grid, linewidth = 0.4),

    # Legend
    legend.background = ggplot2::element_rect(fill = col_bg, colour = NA),
    legend.key = ggplot2::element_rect(fill = col_panel, colour = NA),

    # Facets
    strip.background = ggplot2::element_rect(fill = col_accent, colour = NA),
    strip.text = ggplot2::element_text(colour = "#1A1A2E", face = "bold")
  )
}


