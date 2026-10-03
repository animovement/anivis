#' Plot how much segments vary from their usual length
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Renders a `check_segment_length()` result (from the anicheck package) as a
#' horizontal violin per segment of its length relative to its reference
#' (segments on the y axis, relative length on the x), with a point at the
#' median and a line spanning the inter-quartile range. A solid line marks the
#' reference (1) and dashed lines the check's tolerance either side of it, and
#' the share of frames beyond them is written at the right of each segment
#' that has any. A segment that keeps its length is a narrow violin on the
#' solid line; a wide one, or one reaching past the dashed lines, is a segment
#' the tracking gets wrong. Segments run top to bottom in the structure's
#' order, and with several individuals, each gets its own panel.
#'
#' The violins are drawn from the kernel-density grid stored in the check
#' object (via `geom_polygon`), so no raw lengths are needed. The check enters
#' relative lengths above its `clamp` (2, unless the tolerance is large) into
#' that grid at the clamp, so a segment with a few wild frames shows a bump at
#' the right edge rather than stretching the axis; the plot's caption says so
#' when it happens. Styling matches the other check plots ([theme_imputets()],
#' vertical gridlines only). The plot is built from an intermediate frame of
#' class `anivis_check_segment_length_data` produced by [as_plot_data()] — the
#' staging step that mirrors `data_plot()` in \pkg{see}.
#'
#' @param x A `check_segment_length` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param clip Cut each violin where its density falls below this fraction of
#'   its peak (default `0.02`), so thin tails and the neck bridging a bimodal
#'   distribution are removed. Set `0` to keep the full density.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_imputets()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()], [plot.anivis_check_confidence()]
#'
#' @examplesIf requireNamespace("anicheck", quietly = TRUE) && utils::packageVersion("anicheck") >= "0.3.0.9004"
#' af <- anicore::example_anipoint(n_obs = 100, n_individuals = 2) |>
#'   anicore::set_structure(anicore::example_structure())
#' plot(anicheck::check_segment_length(af))
#'
#' @export
plot.anivis_check_segment_length <- function(
  x,
  ...,
  clip = 0.02,
  mode = c("light", "dark")
) {
  mode <- match.arg(mode)
  plot_df <- as_plot_data(x, clip = clip)
  positions <- attr(plot_df, "positions")
  overlay <- attr(plot_df, "overlay")
  tolerance <- attr(plot_df, "tolerance")
  clamp <- attr(plot_df, "clamp")

  cols <- imputets_colours()
  # The median and quartiles, and the reference line, in ink that reads on
  # the theme's background.
  ink <- if (mode == "dark") "grey85" else "grey20"

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 1, colour = ink, linewidth = 0.4) +
    ggplot2::geom_vline(
      xintercept = attr(plot_df, "guides"),
      colour = cols$na,
      linetype = "dashed",
      linewidth = 0.5
    ) +
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
      colour = ink
    ) +
    ggplot2::geom_point(
      data = overlay,
      ggplot2::aes(x = .data$median, y = .data$y),
      size = 1.8,
      colour = ink
    ) +
    ggplot2::geom_text(
      data = overlay[overlay$share_off > 0, , drop = FALSE],
      ggplot2::aes(
        x = Inf,
        y = .data$y,
        label = sprintf("%.1f%% off", 100 * .data$share_off)
      ),
      hjust = 1.1,
      size = 2.8,
      colour = cols$na
    ) +
    # Room on the right for the share-off labels.
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_y_continuous(
      breaks = unname(positions),
      labels = names(positions),
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::labs(
      x = "length relative to reference",
      y = "segment",
      title = "Segment Lengths",
      subtitle = sprintf(
        "Per-segment length relative to its reference; dashed lines at %s%% either side",
        format(100 * tolerance)
      ),
      caption = if (isTRUE(attr(plot_df, "clamped"))) {
        sprintf("Relative lengths above %s are drawn at %s.", clamp, clamp)
      }
    ) +
    theme_imputets(mode = mode) +
    # Relative length on x: keep only the major vertical gridlines.
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(attr(plot_df, "facet"))) {
    # Stack facets as rows (one per individual, or other varying key).
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group), ncol = 1)
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @details
#' `as_plot_data.check_segment_length()` turns the per-track density grid of
#' relative lengths into closed violin polygons, as for `check_confidence()`:
#' each segment sits at an integer y position, the structure's first segment
#' at the top, with
#' its density mirrored either side along the relative-length (x) axis and cut
#' below `clip` x its peak. `segment` is always the y axis; any other key that
#' varies (an individual, a session) collapses into a `group` factor for
#' faceting. A track that was never measured has nothing to draw and is left
#' out. The y positions, a per-track `overlay` of the median and quartiles
#' (capped at the check's `clamp`, like the grid) and the share off, the
#' tolerance and the x positions of its
#' `guides`, `clamp` and whether any track exceeded it (`clamped`), and `facet`
#' ride along as attributes. Returns a frame classed
#' `anivis_check_segment_length_data`.
as_plot_data.check_segment_length <- function(x, ..., clip = 0.02) {
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")
  tolerance <- attr(x, "tolerance")
  clamp <- attr(x, "clamp")

  # The segment is the axis; anything else that varies is a facet.
  varying <- group_cols[vapply(
    group_cols,
    function(col) length(unique(groups[[col]])) > 1L,
    logical(1)
  )]
  facet_vars <- setdiff(varying, "segment")
  facet_group <- function(tbl) {
    if (length(facet_vars)) {
      do.call(
        paste,
        c(
          lapply(facet_vars, function(col) as.character(tbl[[col]])),
          sep = " | "
        )
      )
    } else {
      rep("all", nrow(tbl))
    }
  }
  # The structure's first segment at the top, so a skeleton reads head down.
  y_levels <- unique(as.character(groups$segment))
  positions <- stats::setNames(rev(seq_along(y_levels)), y_levels)

  # Build the per-track key inline (group_key lives in anicheck) to keep
  # anivis self-contained.
  gkey <- do.call(
    paste,
    c(lapply(group_cols, function(col) as.character(x[[col]])), sep = "\t")
  )
  parts <- split(x, factor(gkey, levels = unique(gkey)))
  violins <- do.call(
    rbind,
    lapply(parts, segment_length_violin, positions, facet_group, clip)
  )
  if (is.null(violins)) {
    violins <- data.frame(
      x = numeric(0),
      y = numeric(0),
      segment = character(0),
      group = character(0),
      poly = character(0)
    )
  }
  rownames(violins) <- NULL

  overlay <- groups[!is.na(groups$relative_median), , drop = FALSE]
  overlay <- data.frame(
    y = unname(positions[as.character(overlay$segment)]),
    group = facet_group(overlay),
    median = pmin(overlay$relative_median, clamp),
    q25 = pmin(overlay$relative_q25, clamp),
    q75 = pmin(overlay$relative_q75, clamp),
    share_off = overlay$share_off
  )

  guides <- c(1 - tolerance, 1 + tolerance)
  attr(violins, "positions") <- positions
  attr(violins, "overlay") <- overlay
  attr(violins, "facet") <- length(facet_vars) > 0L
  attr(violins, "tolerance") <- tolerance
  # A tolerance of 1 or more allows any shortening, so there is no lower line.
  attr(violins, "guides") <- guides[guides > 0]
  attr(violins, "clamp") <- clamp
  attr(violins, "clamped") <- any(groups$relative_max > clamp, na.rm = TRUE)
  class(violins) <- c("anivis_check_segment_length_data", "data.frame")
  violins
}

# Internal: one track's violin, as one closed mirrored polygon per contiguous
# run of grid points above `clip` x the peak (the same shape as the confidence
# violins). NULL for a track with nothing to draw.
segment_length_violin <- function(d, positions, facet_group, clip) {
  d <- d[!is.na(d$value), , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  d <- d[order(d$value), , drop = FALSE]
  peak <- max(d$density)
  cat <- as.character(d$segment[1])
  fg <- facet_group(d)[1]
  centre <- positions[[cat]]

  keep <- which(d$density >= clip * peak)
  if (!length(keep)) {
    return(NULL)
  }
  gaps <- which(diff(keep) > 1L)
  seg_start <- c(1L, gaps + 1L)
  seg_end <- c(gaps, length(keep))

  do.call(
    rbind,
    lapply(seq_along(seg_start), function(si) {
      rows <- keep[seg_start[si]:seg_end[si]]
      dd <- d[rows, , drop = FALSE]
      half <- dd$density / peak * 0.4 # width-normalised half-width
      data.frame(
        x = c(dd$value, rev(dd$value)),
        y = c(centre + half, centre - rev(half)),
        segment = cat,
        group = fg,
        poly = paste(cat, fg, si, sep = "\r"),
        stringsAsFactors = FALSE
      )
    })
  )
}
