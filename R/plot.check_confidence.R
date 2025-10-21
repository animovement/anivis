#' Visualize the distribution of confidence values for each keypoint
#'
#' This function generates histograms showing the distribution of confidence
#' values for each keypoint in the dataset.
#'
#' @param x A data frame containing at least the columns `keypoint` and `confidence`.
#' @param ... Additional arguments for ggplot2
#'
#' @return A ggplot boxplot of the confidence per keypoint.
#'
#'
#' @export
plot.plot_check_confidence <- function(
    x,
    ...
    ) {
  # Parameters
  keypoints <- unique(x$keypoint)

  # Base plot
  # p <- ggplot()

  # Add data
  # Create the boxplot
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$keypoint, fill = .data$keypoint)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = .data$conf_min,
        lower = .data$conf_q1,
        middle = .data$conf_median,
        upper = .data$conf_q3,
        ymax = .data$conf_max
      ),
      stat = "identity"
    ) +
    ggplot2::labs(
      title = "Confidence",
      subtitle = "Confidence of observations by keypoint.",
      x = "Keypoint",
      y = "Confidence"
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(limits = c(0,1)) +
    theme_animovement()

  p
}
