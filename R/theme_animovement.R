#' Animovement ggplot2 Theme
#'
#' The default animovement theme, with light and dark variants. It builds on
#' [ggplot2::theme_linedraw()] — fine black panel border, thin gridlines — but
#' swaps linedraw's black facet strips for the usual light-grey strips with
#' dark text, and adds the plain enlarged title, generous axis-title margins
#' and title positioning of `see::theme_modern()`. Further themes with their
#' own names may be added over time.
#'
#' @param mode Either "light" (default) or "dark".
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
#' @export
theme_animovement <- function(
  mode = c("light", "dark"),
  base_size = 11,
  base_family = ""
) {
  mode <- match.arg(mode)

  if (mode == "light") {
    theme_animovement_light(base_size = base_size, base_family = base_family)
  } else {
    theme_animovement_dark(base_size = base_size, base_family = base_family)
  }
}

# Internal: theme_linedraw() with see-style typography (plain enlarged title,
# title aligned to the plot, roomy axis-title margins) and normal light-grey
# facet strips instead of linedraw's black ones. `cols` carries the
# mode-specific colours; `dark` recolours the linedraw lines / backgrounds for
# the dark variant.
theme_animovement_base <- function(base_size, base_family, cols, dark = FALSE) {
  t <- ggplot2::theme_linedraw(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = cols$text, size = base_size),

      # Typography borrowed from see::theme_modern()
      plot.title = ggplot2::element_text(
        size = base_size * 1.35,
        face = "plain",
        colour = cols$text,
        margin = ggplot2::margin(b = base_size * 1.8)
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = cols$text_light),
      plot.margin = ggplot2::margin(10, 10, 10, 10),

      axis.text = ggplot2::element_text(colour = cols$text_light),
      axis.title = ggplot2::element_text(colour = cols$text),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = base_size)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = base_size)
      ),

      legend.position = "right",
      legend.key = ggplot2::element_blank(),

      # Normal light-grey facet strips with dark text, not linedraw's black,
      # boxed to match the panel border. (The strip/panel junction reads a
      # touch heavier where the strip and panel strokes meet.)
      strip.background = ggplot2::element_rect(
        fill = cols$strip,
        colour = cols$strip_border,
        linewidth = 0.5
      ),
      strip.text = ggplot2::element_text(
        colour = cols$strip_text,
        face = "bold"
      )
    )

  if (dark) {
    t <- t +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
        panel.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
        panel.border = ggplot2::element_rect(
          fill = NA,
          colour = cols$border,
          linewidth = 0.5
        ),
        panel.grid.major = ggplot2::element_line(
          colour = cols$grid,
          linewidth = 0.3
        ),
        panel.grid.minor = ggplot2::element_line(
          colour = cols$grid,
          linewidth = 0.2
        ),
        axis.ticks = ggplot2::element_line(
          colour = cols$border,
          linewidth = 0.4
        ),
        legend.background = ggplot2::element_rect(fill = cols$bg, colour = NA)
      )
  }

  t
}

#' @rdname theme_animovement
#' @export
theme_animovement_light <- function(base_size = 11, base_family = "") {
  theme_animovement_base(
    base_size = base_size,
    base_family = base_family,
    cols = list(
      text = "#2D3E50",
      text_light = "#5A6B7D",
      strip = "#D9D9D9",
      strip_text = "#1A1A1A",
      strip_border = "black"
    )
  )
}

#' @rdname theme_animovement
#' @export
theme_animovement_dark <- function(base_size = 11, base_family = "") {
  theme_animovement_base(
    base_size = base_size,
    base_family = base_family,
    dark = TRUE,
    cols = list(
      bg = "#1A1A2E",
      text = "#E0E0E0",
      text_light = "#A0A0B0",
      grid = "#2A3A5E",
      border = "#A0A0B0",
      strip = "#2E3B5A",
      strip_text = "#E0E0E0",
      strip_border = "#A0A0B0"
    )
  )
}
