#' imputeTS-style Theme for Check Plots
#'
#' A larger-font variant of [theme_animovement()] tuned for the diagnostic check
#' plots ([plot.anivis_check_na_timing()], [plot.anivis_check_na_gapsize()],
#' [plot.anivis_check_confidence()]), styled after the imputeTS package figures they are
#' adapted from. Compared with [theme_animovement()] it uses a larger base size,
#' places the title directly above the panel (not over the axis), renders the
#' subtitle as markdown via \pkg{marquee} — so words like NA / non-NA can be
#' coloured inline with the `.na` / `.nona` classes — and puts the legend at the
#' bottom.
#'
#' @param mode Either `"light"` (default) or `"dark"`.
#' @param base_size Base font size. Default `13`.
#'
#' @return A ggplot2 theme object.
#'
#' @seealso [theme_animovement()]
#' @export
theme_imputets <- function(mode = c("light", "dark"), base_size = 13) {
  mode <- match.arg(mode)
  theme_animovement(mode = mode, base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size * 1.45,
        face = "plain",
        margin = ggplot2::margin(b = base_size * 0.2)
      ),
      # Title / subtitle sit above the panel, aligned with it (not the axis).
      plot.title.position = "panel",
      plot.subtitle = marquee::element_marquee(
        size = base_size,
        style = imputets_subtitle_style(),
        margin = ggplot2::margin(b = base_size * 0.8)
      ),
      axis.title = ggplot2::element_text(size = base_size * 1.05),
      axis.text = ggplot2::element_text(size = base_size * 0.9),
      strip.text = ggplot2::element_text(size = base_size, face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size * 0.95)
    )
}

# Internal: the check-plot NA / non-NA colours, shared by the plot fills and the
# coloured subtitle so they always match.
imputets_colours <- function() {
  list(na = "indianred2", nona = "steelblue")
}

# Internal: a marquee style set giving the inline `.na` / `.nona` classes the
# check colours, so a subtitle like "{.na NA}" renders coloured.
imputets_subtitle_style <- function() {
  cols <- imputets_colours()
  sty <- marquee::classic_style()
  sty <- marquee::modify_style(sty, "na", color = cols$na)
  sty <- marquee::modify_style(sty, "nona", color = cols$nona)
  sty
}
