#' Animovement Colour Scales
#'
#' Colour and fill scales using the viridis palette, suitable for both
#' light and dark themes.
#'
#' @param ... Arguments passed to the underlying viridis scale.
#' @param option Viridis palette option. Default is "viridis" (D).
#'
#' @name scale_animovement
NULL

#' @rdname scale_animovement
#' @export
scale_colour_animovement <- function(..., option = "viridis") {
  ggplot2::scale_colour_viridis_d(..., option = option)
}

#' @rdname scale_animovement
#' @export
scale_color_animovement <- scale_colour_animovement

#' @rdname scale_animovement
#' @export
scale_fill_animovement <- function(..., option = "viridis") {
  ggplot2::scale_fill_viridis_d(..., option = option)
}

#' @rdname scale_animovement
#' @export
scale_colour_animovement_c <- function(..., option = "viridis") {
  ggplot2::scale_colour_viridis_c(..., option = option)
}

#' @rdname scale_animovement
#' @export
scale_color_animovement_c <- scale_colour_animovement_c

#' @rdname scale_animovement
#' @export
scale_fill_animovement_c <- function(..., option = "viridis") {
  ggplot2::scale_fill_viridis_c(..., option = option)
}