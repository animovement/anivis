#' Plot the Distribution of Tracking Confidence
#'
#' Renders a `check_confidence()` result (from the anicheck package) as a
#' horizontal violin per keypoint of its tracking confidence (keypoints on the y
#' axis, confidence on the x), with a point at the median and a line spanning the
#' inter-quartile range. A violin sitting far to the right and tight is a reliably
#' tracked keypoint; a left-shifted body or a long low tail flags one the tracker
#' was often unsure about. With several individuals, each gets its own panel.
#'
#' The violins are drawn from the kernel-density grid stored in the check object
#' (via `geom_polygon`), so no raw values are needed. Styling matches the other
#' check plots ([theme_imputets()], horizontal-only gridlines). The plot is built
#' from an intermediate frame of class `anivis_check_confidence` produced by
#' [as_plot_data()] — the staging step that mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_confidence` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param clip Cut each violin where its density falls below this fraction of its
#'   peak, so thin tails and the neck bridging a bimodal distribution are removed
#'   and the violin shows only where data actually is. Default `0.02`; set `0` to
#'   keep the full density.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_imputets()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_confidence <- function(x, ..., clip = 0.02, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x, clip = clip)
  positions <- attr(plot_df, "positions")
  overlay <- attr(plot_df, "overlay")
  axis_var <- attr(plot_df, "axis_var")

  cols <- imputets_colours()

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = plot_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$poly),
      fill = ggplot2::alpha(cols$nona, 0.6),
      colour = cols$nona,
      linewidth = 0.4
    ) +
    ggplot2::geom_linerange(
      data = overlay,
      ggplot2::aes(y = .data$y, xmin = .data$q25, xmax = .data$q75),
      linewidth = 0.9,
      colour = "grey20"
    ) +
    ggplot2::geom_point(
      data = overlay,
      ggplot2::aes(x = .data$median, y = .data$y),
      size = 1.8,
      colour = "grey20"
    ) +
    ggplot2::scale_y_continuous(
      breaks = unname(positions),
      labels = names(positions),
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::labs(
      x = "confidence",
      y = if (!is.na(axis_var)) axis_var else NULL,
      title = "Tracking Confidence",
      subtitle = "Per-keypoint distribution of tracking confidence"
    ) +
    theme_imputets(mode = mode) +
    # Confidence on x: keep only the major vertical gridlines.
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(attr(plot_df, "facet"))) {
    # Stack facets as rows (one per varying-group combination).
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group), ncol = 1)
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @param clip For `as_plot_data.check_confidence()`: density floor (fraction of
#'   each keypoint's peak) below which the violin is cut. Default `0.02`.
#'
#' @details
#' `as_plot_data.check_confidence()` turns the per-keypoint density grid into
#' closed violin polygons: each keypoint sits at an integer y position, and its
#' density is mirrored either side along the confidence (x) axis (width-normalised
#' so every violin has the same maximum width). Grid points below `clip` x the
#' peak are dropped, so thin tails
#' and bimodal-bridging necks disappear and a split distribution becomes separate
#' polygons. `keypoint` is the x-axis category; any other identity that varies
#' (e.g. `individual`) collapses into a `group` factor for faceting. The x
#' positions, a per-keypoint median / quartile `overlay`, and `facet` ride along
#' as attributes. Returns a frame classed `anivis_check_confidence`.
as_plot_data.check_confidence <- function(x, ..., clip = 0.02) {
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")

  # The y-axis variable is the grouping column that varies (preferring keypoint
  # when several do); anything else that varies becomes a facet. With nothing
  # varying it is a single violin.
  varying <- group_cols[vapply(
    group_cols,
    function(col) length(unique(groups[[col]])) > 1L,
    logical(1)
  )]
  axis_var <- if ("keypoint" %in% varying) {
    "keypoint"
  } else if (length(varying)) {
    varying[[1]]
  } else {
    NA_character_
  }
  facet_vars <- setdiff(varying, axis_var)

  y_cat <- function(tbl) {
    if (is.na(axis_var)) rep("all", nrow(tbl)) else as.character(tbl[[axis_var]])
  }
  facet_group <- function(tbl) {
    if (length(facet_vars)) {
      do.call(
        paste,
        c(lapply(facet_vars, function(col) as.character(tbl[[col]])), sep = " | ")
      )
    } else {
      rep("all", nrow(tbl))
    }
  }
  y_levels <- if (is.na(axis_var)) "all" else unique(y_cat(groups))
  positions <- stats::setNames(seq_along(y_levels), y_levels)

  # One or more closed mirrored polygons per density group (a keypoint within a
  # facet): the density is cut where it drops below `clip` x its peak, so a
  # bimodal distribution splits into separate blobs rather than being bridged by
  # a misleading thin neck.
  gkey <- group_key(x, group_cols)
  parts <- split(x, factor(gkey, levels = unique(gkey)))
  violins <- do.call(rbind, lapply(parts, function(d) {
    d <- d[order(d$value), , drop = FALSE]
    peak <- max(d$density)
    cat <- y_cat(d)[1]
    fg <- facet_group(d)[1]
    centre <- positions[[cat]]

    keep <- which(d$density >= clip * peak)
    if (!length(keep)) {
      return(NULL)
    }
    # Split the kept grid points into contiguous runs (the separate blobs).
    gaps <- which(diff(keep) > 1L)
    seg_start <- c(1L, gaps + 1L)
    seg_end <- c(gaps, length(keep))

    do.call(rbind, lapply(seq_along(seg_start), function(si) {
      seg <- keep[seg_start[si]:seg_end[si]]
      dd <- d[seg, , drop = FALSE]
      half <- dd$density / peak * 0.4 # width-normalised half-width
      # Horizontal violin: confidence on x, the keypoint position (+/- the
      # density half-width) on y.
      data.frame(
        x = c(dd$value, rev(dd$value)),
        y = c(centre + half, centre - rev(half)),
        keypoint = cat,
        group = fg,
        poly = paste(cat, fg, si, sep = "\r"),
        stringsAsFactors = FALSE
      )
    }))
  }))

  overlay <- groups
  overlay$y <- positions[y_cat(overlay)]
  overlay$group <- facet_group(overlay)
  overlay <- overlay[, c("y", "group", "median", "q25", "q75")]

  attr(violins, "positions") <- positions
  attr(violins, "overlay") <- overlay
  attr(violins, "facet") <- length(facet_vars) > 0L
  attr(violins, "axis_var") <- axis_var
  class(violins) <- c("anivis_check_confidence", "data.frame")
  violins
}
