#' Plot Movement Trajectory
#'
#' Creates a ggplot of the x-y trajectory from an aniframe.
#'
#' @param data An aniframe object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object.
#'
#' @export
plot_trajectory <- function(data, ...) {
 UseMethod("plot_trajectory")
}


#' @rdname plot_trajectory
#' @export
plot_trajectory.default <- function(data, ..., mode = c("light", "dark")) {
 if (!aniframe::is_aniframe(data)) {
   cli::cli_abort("{.arg data} must be an aniframe.")
 }
  
  mode <- match.arg(mode)
  
  x_lab <- if (!aniframe::get_metadata(data)$unit_space == "none") {
    paste0("x (", aniframe::get_metadata(data)$unit_space, ")")
  } else {
    "x"
  }

  y_lab <- if (!aniframe::get_metadata(data)$unit_space == "none") {
    paste0("y (", aniframe::get_metadata(data)$unit_space, ")")
  } else {
    "y"
  }
  

 ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$time)) +
   ggplot2::geom_path() +
   ggplot2::coord_fixed() +
   ggplot2::labs(x = x_lab, y = y_lab) +
   scale_colour_animovement_c() +
   theme_animovement(mode = mode)
   
}