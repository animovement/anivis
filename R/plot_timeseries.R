#' Plot a Variable as a Time Series
#'
#' Plots one or more numeric variables from an aniframe against `time`, with one
#' line per trajectory group (every `variables_what` column and every non-time
#' `variables_when` column in the metadata). Use it for any per-frame measure —
#' `speed`, `acceleration`, a confidence score, a temperature channel, or any
#' other derived column.
#'
#' Pass several names to `variable` to draw a panel per variable, **vertically
#' stacked** with a shared x axis and a single shared legend (via [plots()]);
#' stacking makes it easy to compare their shape over time. A single variable
#' returns a plain ggplot.
#'
#' The `layout` argument chooses how groups are arranged, mirroring
#' [plot_events()]:
#'
#' * `"inline"` (default): all groups share one panel as coloured lines, with a
#'   legend when there is more than one group.
#' * `"facet"`: each group gets its own panel — `facet_grid()` when both an
#'   identity and a (non-time) condition vary, otherwise `facet_wrap()` with the
#'   panels stacked in rows so the shared x axis lines up.
#'
#' Colours come from [palette_animovement()], matching [plot_trajectory()]. The
#' x axis uses the same time-unit handling as [plot_events()] — `HH:MM:SS` for
#' true time units, raw frames otherwise.
#'
#' @param data An aniframe object.
#' @param variable Name(s) of the column(s) to plot on the y axis — a character
#'   vector. Required (no auto-detection). Several names draw a stacked panel
#'   per variable.
#' @param ... Additional arguments (currently unused).
#' @param layout Either `"inline"` (default, all groups in one panel) or
#'   `"facet"` (one panel per group).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#' @param palette Name of a qualitative palette accepted by
#'   [grDevices::hcl.colors()]; controls the hue family across groups.
#'
#' @return A ggplot object for a single variable, or a stacked patchwork for
#'   several.
#'
#' @export
plot_timeseries <- function(data, ...) {
  UseMethod("plot_timeseries")
}

#' @rdname plot_timeseries
#' @export
plot_timeseries.default <- function(
  data,
  variable = NULL,
  ...,
  layout = c("inline", "facet"),
  mode = c("light", "dark"),
  palette = "Dark 3"
) {
  if (!aniframe::is_aniframe(data)) {
    cli::cli_abort("{.arg data} must be an aniframe.")
  }
  layout <- match.arg(layout)
  mode <- match.arg(mode)

  meta <- aniframe::get_metadata(data)
  variables <- check_timeseries_variables(data, variable)

  keys <- aniframe_group_keys(data)
  pal <- palette_animovement(data, palette = palette)

  plot_df <- as.data.frame(data)
  plot_df[[".group"]] <- factor(keys$group, levels = names(pal))
  plot_df <- plot_df[order(plot_df$.group, plot_df$time), , drop = FALSE]

  # X axis: convert to hms for true time units so scale_x_time() formats it.
  unit <- meta$unit_time
  unit_chr <- if (!is.null(unit)) as.character(unit) else NA_character_
  factor <- seconds_per_unit(unit_chr)
  use_hms <- !is.na(factor)
  if (use_hms) {
    plot_df <- to_hms_columns(plot_df, "time", factor)
  }
  x_scale <- if (use_hms) {
    ggplot2::scale_x_time()
  } else {
    ggplot2::scale_x_continuous()
  }
  x_lab <- if (use_hms) {
    NULL
  } else if (identical(unit_chr, "frame")) {
    "time (frames)"
  } else {
    "time"
  }

  facet_layer <- if (layout == "facet") timeseries_facets(data, keys) else NULL
  # Faceting names the groups, and a single group needs no legend either way.
  hide_legend <- layout == "facet" || keys$mode == "single"

  build_panel <- function(var) {
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x = .data$time,
        y = .data[[var]],
        colour = .data$.group,
        group = .data$.group
      )
    ) +
      ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = pal) +
      x_scale +
      ggplot2::labs(x = x_lab, y = var, colour = NULL) +
      theme_animovement(mode = mode)
    if (!is.null(facet_layer)) {
      p <- p + facet_layer
    }
    if (hide_legend) {
      p <- p + ggplot2::guides(colour = "none")
    }
    p
  }

  if (length(variables) == 1L) {
    return(build_panel(variables))
  }

  # Several variables: one panel each, stacked vertically with a shared x axis
  # (and x title) and a single shared legend.
  plots(
    lapply(variables, build_panel),
    n_columns = 1,
    axes = "collect_x",
    axis_titles = "collect_x",
    guides = "collect"
  )
}

# Internal: validate the requested variable(s). Each must name a numeric
# column; there is deliberately no auto-detection. Returns the character vector.
check_timeseries_variables <- function(data, variable) {
  if (is.null(variable) || !length(variable)) {
    cli::cli_abort(c(
      "{.arg variable} is required.",
      "i" = "Name one or more columns, e.g. {.code variable = \"speed\"} or
             {.code variable = c(\"speed\", \"acceleration\")}."
    ))
  }
  if (!is.character(variable)) {
    cli::cli_abort(
      "{.arg variable} must be a character vector of column names."
    )
  }
  unknown <- setdiff(variable, names(data))
  if (length(unknown)) {
    cli::cli_abort(
      "{.arg variable} names unknown column{?s}: {.val {unknown}}."
    )
  }
  non_numeric <- variable[
    !vapply(
      variable,
      function(v) is.numeric(data[[v]]),
      logical(1)
    )
  ]
  if (length(non_numeric)) {
    cli::cli_abort(
      "{.arg variable} must be numeric; these are not: {.val {non_numeric}}."
    )
  }
  variable
}

# Internal: facet by whichever identity (`what`) and condition (`when`) columns
# actually vary — facet_grid when both do, otherwise facet_wrap. Returns NULL
# when nothing varies (a single group).
timeseries_facets <- function(data, keys) {
  varying <- function(cols) {
    cols[vapply(
      cols,
      function(col) length(unique(data[[col]])) > 1L,
      logical(1)
    )]
  }
  what_v <- varying(keys$what_cols)
  when_v <- varying(keys$when_cols)

  if (length(what_v) && length(when_v)) {
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!!rlang::syms(when_v)),
      cols = ggplot2::vars(!!!rlang::syms(what_v))
    )
  } else if (length(what_v)) {
    # One axis: stack panels in rows so the shared x axis lines up vertically.
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(what_v)), ncol = 1)
  } else if (length(when_v)) {
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(when_v)), ncol = 1)
  } else {
    NULL
  }
}
