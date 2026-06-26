#' Plot the Distribution of Tracking Confidence
#'
#' Renders a `check_confidence()` result (from the anicheck package) as a
#' boxplot per keypoint of its tracking confidence. A box sitting high on the y
#' axis is a reliably tracked keypoint; a low box or a long low whisker flags a
#' keypoint the tracker was often unsure about. With several individuals, each
#' gets its own panel.
#'
#' The boxes are drawn straight from the five-number summary in the check object
#' (`stat = "identity"`), so no raw values are needed. The plot is built from an
#' intermediate frame of class `anivis_check_confidence` produced by
#' [as_plot_data()] — the staging step that mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_confidence` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_confidence <- function(x, ..., mode = c("light", "dark")) {
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x)
  has_keypoint <- "keypoint" %in% attr(x, "group_cols")

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$keypoint)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = .data$ymin,
        lower = .data$lower,
        middle = .data$middle,
        upper = .data$upper,
        ymax = .data$ymax
      ),
      stat = "identity",
      fill = "#3A6FB0",
      width = 0.6
    ) +
    ggplot2::labs(
      x = if (has_keypoint) "keypoint" else NULL,
      y = "confidence",
      title = "Tracking confidence"
    ) +
    theme_imputets(mode = mode)

  if (isTRUE(attr(plot_df, "facet"))) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group), scales = "free_y")
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @details
#' `as_plot_data.check_confidence()` maps the per-keypoint five-number summary
#' from `check_confidence()` onto the boxplot aesthetics
#' (`ymin`/`lower`/`middle`/`upper`/`ymax`). `keypoint` is the x-axis factor when
#' present; any other identity that varies (e.g. `individual`) collapses into a
#' `group` factor for faceting (flagged by a `facet` attribute). Returns a frame
#' classed `anivis_check_confidence`.
as_plot_data.check_confidence <- function(x, ...) {
  group_cols <- attr(x, "group_cols")
  has_keypoint <- "keypoint" %in% group_cols

  if (has_keypoint) {
    xvals <- as.character(x$keypoint)
    facet_cols <- setdiff(group_cols, "keypoint")
    varying <- facet_cols[vapply(
      facet_cols,
      function(col) length(unique(x[[col]])) > 1L,
      logical(1)
    )]
  } else {
    # No keypoint: collapse whatever identity columns exist onto the x axis.
    xvals <- if (length(group_cols)) {
      do.call(
        paste,
        c(lapply(group_cols, function(col) as.character(x[[col]])), sep = " | ")
      )
    } else {
      rep("all", nrow(x))
    }
    varying <- character(0)
  }

  group <- if (length(varying)) {
    do.call(
      paste,
      c(lapply(varying, function(col) as.character(x[[col]])), sep = " | ")
    )
  } else {
    rep("all", nrow(x))
  }

  out <- data.frame(
    keypoint = factor(xvals, levels = unique(xvals)),
    group = factor(group, levels = unique(group)),
    ymin = x$min,
    lower = x$q25,
    middle = x$median,
    upper = x$q75,
    ymax = x$max,
    stringsAsFactors = FALSE
  )
  attr(out, "facet") <- length(varying) > 0L
  class(out) <- c("anivis_check_confidence", "data.frame")
  out
}
