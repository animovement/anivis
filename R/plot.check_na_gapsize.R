#' Plot the Occurrence of Missing-Value Gap Sizes
#'
#' Renders a `check_na_gapsize()` result (from the anicheck package) as a
#' horizontal bar chart of how often each gap size occurs — an adaptation of
#' `ggplot_na_gapsize()` from the imputeTS package (Moritz & Gatscha, GPL-3).
#' For every gap length it draws the number of such gaps (indianred) and,
#' optionally, the total missing frames they account for (steelblue, the gap size
#' times its count). Bars are ranked so the most common (or most costly) gaps sit
#' on top; only the top `limit` are shown. With more than one group, each gets
#' its own panel.
#'
#' The plot is built from an intermediate frame of class `anivis_check_na_gapsize`
#' produced by [as_plot_data()] — the staging step that mirrors `data_plot()` in
#' \pkg{see}.
#'
#' @param x A `check_na_gapsize` object (from the anicheck package).
#' @param ... Additional arguments (currently unused).
#' @param ranked_by Order bars by `"occurrence"` (default, gap frequency) or
#'   `"total"` (resulting missing frames).
#' @param limit Maximum number of gap sizes (bars) to show per group, keeping the
#'   top-ranked. Default `10`.
#' @param include_total Whether to add the steelblue \dQuote{resulting NAs} bar
#'   alongside the occurrence bar. Default `TRUE`.
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#'
#' @return A ggplot object.
#'
#' @seealso [as_plot_data()]
#'
#' @export
plot.check_na_gapsize <- function(
  x,
  ...,
  ranked_by = c("occurrence", "total"),
  limit = 10,
  include_total = TRUE,
  mode = c("light", "dark")
) {
  ranked_by <- match.arg(ranked_by)
  mode <- match.arg(mode)
  plot_df <- as_plot_data(
    x,
    ranked_by = ranked_by,
    limit = limit,
    include_total = include_total
  )
  group_levels <- attr(plot_df, "group_levels")

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(y = .data$key, x = .data$value, fill = .data$series)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(reverse = TRUE),
      width = 0.7,
      colour = "black"
    ) +
    ggplot2::scale_y_discrete(labels = function(k) sub("___.*$", "", k)) +
    ggplot2::scale_fill_manual(
      values = c(occurrence = "indianred", total = "steelblue"),
      labels = c(
        occurrence = "Number occurrence gapsize",
        total = "Resulting NAs for gapsize"
      ),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Number occurrence",
      y = NULL,
      title = "Occurrence of gap sizes",
      subtitle = "Gap sizes (NAs in a row) ordered by most common"
    ) +
    theme_imputets(mode = mode)

  if (length(group_levels) > 1L) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$group), ncol = 1, scales = "free_y")
  }
  p
}

#' @rdname as_plot_data
#' @export
#'
#' @param ranked_by,limit,include_total For `as_plot_data.check_na_gapsize()`:
#'   order bars by `"occurrence"` or `"total"`, keep the top `limit` per group,
#'   and whether to include the total-NAs series.
#'
#' @details
#' `as_plot_data.check_na_gapsize()` reshapes the gap-size table into the long,
#' ranked form the imputeTS-style bar chart needs: one row per (group, gap size,
#' series), where `series` is `occurrence` (and `total` when `include_total`),
#' `value` the count, and `key` a `reorder_within`-style factor (`"<n> NA-gap___
#' <group>"`) ordered by `ranked_by` so each facet sorts independently. Returns a
#' frame classed `anivis_check_na_gapsize`.
as_plot_data.check_na_gapsize <- function(
  x,
  ...,
  ranked_by = c("occurrence", "total"),
  limit = 10,
  include_total = TRUE
) {
  ranked_by <- match.arg(ranked_by)
  group_cols <- attr(x, "group_cols")
  groups <- attr(x, "groups")

  varying <- group_cols[vapply(
    group_cols,
    function(col) length(unique(groups[[col]])) > 1L,
    logical(1)
  )]
  label <- function(tbl) {
    if (length(varying)) {
      do.call(
        paste,
        c(lapply(varying, function(col) as.character(tbl[[col]])), sep = " | ")
      )
    } else {
      rep("all", nrow(tbl))
    }
  }
  group_levels <- unique(label(groups))
  xg <- if (nrow(x)) label(x) else character(0)

  levels_acc <- character(0)
  rows <- list()
  for (g in group_levels) {
    d <- x[xg == g, , drop = FALSE]
    if (!nrow(d)) {
      next
    }
    metric <- if (ranked_by == "occurrence") d$n_gaps else d$n_na
    d <- d[order(metric), , drop = FALSE] # ascending -> largest last (top)
    if (nrow(d) > limit) {
      d <- d[(nrow(d) - limit + 1L):nrow(d), , drop = FALSE]
    }
    key <- paste0(d$gap_size, " NA-gap___", g)
    levels_acc <- c(levels_acc, key)

    block <- data.frame(
      group = g,
      key = key,
      series = "occurrence",
      value = d$n_gaps,
      stringsAsFactors = FALSE
    )
    if (include_total) {
      block <- rbind(
        block,
        data.frame(
          group = g,
          key = key,
          series = "total",
          value = d$n_na,
          stringsAsFactors = FALSE
        )
      )
    }
    rows[[g]] <- block
  }

  out <- if (length(rows)) do.call(rbind, rows) else {
    data.frame(
      group = character(0),
      key = character(0),
      series = character(0),
      value = integer(0),
      stringsAsFactors = FALSE
    )
  }
  out$key <- factor(out$key, levels = levels_acc)
  out$series <- factor(out$series, levels = c("occurrence", "total"))
  out$group <- factor(out$group, levels = group_levels)

  attr(out, "group_levels") <- group_levels
  class(out) <- c("anivis_check_na_gapsize", "data.frame")
  out
}
