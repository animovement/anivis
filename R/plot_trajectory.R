#' Plot Movement Trajectory
#'
#' Creates a ggplot of the x-y trajectory from an anipoint. One path is drawn
#' per trajectory group, where a group is the combination of the frame's
#' grouping columns ([anicore::get_keys()]): its identity columns and its
#' temporal context, such as session or trial.
#'
#' The positions are read from the columns the frame declares for its `x` and
#' `y` axes ([anicore::get_axes()]), and time from its index column
#' ([anicore::get_index()]), so neither has to be named `x`, `y` or `time`. The
#' axes are labelled by role. A three-dimensional frame is drawn in its x-y
#' plane. A frame without `x` and `y` axes, such as a polar one, is an error.
#'
#' Colours adapt to the dataset shape:
#'
#' * **single** trajectory (no grouping): the line is coloured continuously
#'   by time using the Material gradient scale ([scale_colour_material_c()]),
#'   shown as a `time` colour bar.
#' * **what-only** or **when-only** grouping (one varying axis): each line
#'   gets its own hue from a qualitative palette, with `time` mapped to alpha
#'   so the line fades in from start to end — shown in a `time` legend.
#' * **matrix** grouping (both axes vary): each line is solid, coloured by
#'   the hue × shade matrix from [palette_animovement()] — hue per `what`,
#'   shade per `when`; time reads from the start/end markers.
#'
#' Every trajectory is annotated with a filled circle at its first point and
#' a filled triangle at its last point, identified in a start/end legend. Gaps
#' from missing data are bridged with a dashed line so the path stays traceable.
#'
#' @param data An anipoint object.
#' @param ... Additional arguments (currently unused).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#' @param palette Name of a qualitative palette accepted by
#'   [grDevices::hcl.colors()]; controls the hue family across grouping
#'   levels.
#'
#' @return A ggplot object.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 1)
#' plot_trajectory(af)
#'
#' @export
plot_trajectory <- function(data, ...) {
  UseMethod("plot_trajectory")
}

#' @rdname plot_trajectory
#' @export
plot_trajectory.default <- function(
  data,
  ...,
  mode = c("light", "dark"),
  palette = "Dark 3"
) {
  if (!anicore::is_anipoint(data)) {
    cli::cli_abort("{.arg data} must be an anipoint.")
  }
  mode <- match.arg(mode)

  axes <- trajectory_axes(data)
  index <- anicore::get_index(data)
  keys <- aniframe_group_keys(data)
  pal <- palette_animovement(data, palette = palette)

  plot_df <- as.data.frame(data)
  plot_df[[".group"]] <- factor(keys$group, levels = names(pal))
  plot_df <- plot_df[order(plot_df$.group, plot_df[[index]]), , drop = FALSE]

  endpoints <- trajectory_endpoints(plot_df, axes, index)

  unit <- anicore::get_metadata(data, "unit_space")
  has_unit <- !is.null(unit) && as.character(unit) != "none"
  x_lab <- if (has_unit) paste0("x (", unit, ")") else "x"
  y_lab <- if (has_unit) paste0("y (", unit, ")") else "y"

  # Time legend: format as HH:MM:SS for true time units (as plot_events does),
  # raw numbers otherwise. Three breaks keep the legend compact.
  unit_time <- anicore::get_metadata(data, "unit_time")
  t_unit <- if (!is.null(unit_time)) as.character(unit_time) else NA_character_
  t_factor <- seconds_per_unit(t_unit)
  time_labels <- if (!is.na(t_factor)) {
    function(b) format(hms::as_hms(round(b * t_factor)))
  } else {
    ggplot2::waiver()
  }
  three_breaks <- function(limits) {
    round(seq(limits[1], limits[2], length.out = 3))
  }

  # Dashed connectors across missing-data gaps, and start/end markers reshaped
  # so a single shape scale can label which symbol is which.
  bridges <- trajectory_gaps(plot_df, axes, index)
  ends_long <- data.frame(
    .group = rep(endpoints$.group, 2),
    x = c(endpoints$x_start, endpoints$x_end),
    y = c(endpoints$y_start, endpoints$y_end),
    point = factor(
      rep(c("start", "end"), each = nrow(endpoints)),
      levels = c("start", "end")
    )
  )

  # The column names are injected (`!!`) so the mappings name the declared
  # columns themselves.
  base <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data[[!!axes[["x"]]]], y = .data[[!!axes[["y"]]]])
  )

  if (keys$mode == "single") {
    # Time -> a continuous colour bar, titled "time" in the legend.
    path_layers <- Filter(
      Negate(is.null),
      list(
        ggplot2::geom_path(
          ggplot2::aes(colour = .data[[!!index]], group = .data$.group),
          na.rm = TRUE
        ),
        if (!is.null(bridges)) {
          ggplot2::geom_segment(
            data = bridges,
            ggplot2::aes(
              x = .data$x,
              y = .data$y,
              xend = .data$xend,
              yend = .data$yend,
              colour = .data$time
            ),
            linetype = "dashed",
            linewidth = 0.4,
            inherit.aes = FALSE,
            na.rm = TRUE
          )
        },
        scale_colour_material_c(
          name = "time",
          labels = time_labels,
          guide = ggplot2::guide_colourbar(order = 3)
        )
      )
    )
  } else if (keys$mode == "matrix") {
    # Hue x shade per group; solid lines (time reads from the start/end markers).
    path_layers <- Filter(
      Negate(is.null),
      list(
        ggplot2::geom_path(
          ggplot2::aes(colour = .data$.group, group = .data$.group),
          na.rm = TRUE
        ),
        if (!is.null(bridges)) {
          ggplot2::geom_segment(
            data = bridges,
            ggplot2::aes(
              x = .data$x,
              y = .data$y,
              xend = .data$xend,
              yend = .data$yend,
              colour = .data$.group
            ),
            linetype = "dashed",
            linewidth = 0.4,
            inherit.aes = FALSE,
            na.rm = TRUE
          )
        },
        ggplot2::scale_colour_manual(values = pal, guide = "none")
      )
    )
  } else {
    # what / when: one hue per group, with time mapped to alpha so the line
    # fades in from start to end and earns a real "time" legend.
    path_layers <- Filter(
      Negate(is.null),
      list(
        ggplot2::geom_path(
          ggplot2::aes(
            colour = .data$.group,
            alpha = .data[[!!index]],
            group = .data$.group
          ),
          na.rm = TRUE
        ),
        if (!is.null(bridges)) {
          ggplot2::geom_segment(
            data = bridges,
            ggplot2::aes(
              x = .data$x,
              y = .data$y,
              xend = .data$xend,
              yend = .data$yend,
              colour = .data$.group
            ),
            linetype = "dashed",
            linewidth = 0.4,
            inherit.aes = FALSE,
            na.rm = TRUE
          )
        },
        ggplot2::scale_colour_manual(values = pal, guide = "none"),
        ggplot2::scale_alpha(
          range = c(0.45, 1),
          name = "time",
          breaks = three_breaks,
          labels = time_labels,
          guide = ggplot2::guide_legend(
            order = 3,
            override.aes = list(colour = "grey25")
          )
        )
      )
    )
  }

  base +
    path_layers +
    ggplot2::geom_point(
      data = ends_long,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        fill = .data$.group,
        shape = .data$point
      ),
      size = 2.5,
      colour = "grey20",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = c(start = 21, end = 24),
      name = NULL,
      guide = ggplot2::guide_legend(
        order = 2,
        override.aes = list(fill = "grey45")
      )
    ) +
    ggplot2::scale_fill_manual(
      values = pal,
      guide = if (keys$mode == "single") {
        "none"
      } else {
        # Filled circle keys so the group colours actually show.
        ggplot2::guide_legend(order = 1, override.aes = list(shape = 21))
      }
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = x_lab, y = y_lab, fill = NULL) +
    theme_animovement(mode = mode)
}

# Internal: the columns carrying the x and y axes, as `c(x = , y = )`. A
# trajectory is drawn in the x-y plane, so a frame without both roles -- polar,
# one-dimensional, or with undeclared axes -- has nothing to draw it from.
trajectory_axes <- function(data, call = rlang::caller_env()) {
  axes <- anicore::get_axes(data)
  if (!all(c("x", "y") %in% names(axes))) {
    system <- anicore::get_coordinate_system(data)
    hint <- if (identical(system, "unknown")) {
      c(
        "i" = "Say which column carries each axis with
               {.code anicore::set_variables(data, where = c(x = , y = ))}."
      )
    } else if (system %in% c("polar", "cylindrical", "spherical")) {
      c(
        "i" = "Convert it to Cartesian coordinates first, for example with
               {.fn anispace::map_to_cartesian}."
      )
    }
    cli::cli_abort(
      c(
        "{.arg data} must have {.field x} and {.field y} axes to plot a
         trajectory.",
        "x" = "Its coordinate system is {.val {system}}.",
        hint
      ),
      call = call
    )
  }
  axes[c("x", "y")]
}

# Internal: per-group first/last (x, y) ordered by time. `axes` names the x and
# y columns (`c(x = , y = )`), and `index` the time column.
trajectory_endpoints <- function(df, axes, index) {
  ord <- order(df$.group, df[[index]])
  df <- df[ord, , drop = FALSE]
  parts <- split(df, df$.group, drop = TRUE)
  rows <- lapply(parts, function(d) {
    x <- d[[axes[["x"]]]]
    y <- d[[axes[["y"]]]]
    non_na <- !is.na(x) & !is.na(y)
    if (!any(non_na)) {
      return(NULL)
    }
    x <- x[non_na]
    y <- y[non_na]
    data.frame(
      .group = d$.group[non_na][1],
      x_start = x[1],
      y_start = y[1],
      x_end = x[length(x)],
      y_end = y[length(y)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # No group has two valid points to mark -- an empty frame, or one that is all
  # NA. do.call(rbind, list()) is NULL, and NULL flows on into rep(each = ) as a
  # length-2 result against a length-0 column.
  if (is.null(out)) {
    out <- data.frame(
      .group = character(0),
      x_start = numeric(0),
      y_start = numeric(0),
      x_end = numeric(0),
      y_end = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  out
}

# Internal: per-group connector segments spanning missing-data gaps. For each
# run of missing (x, y) between two valid points, returns a segment from the
# last valid point to the next valid one (drawn dashed). Returns NULL when no
# group has a gap. `axes` and `index` are as for trajectory_endpoints(); the
# segments come back in plain `x`, `y`, `xend`, `yend` and `time` columns.
trajectory_gaps <- function(df, axes, index) {
  parts <- split(df, df$.group, drop = TRUE)
  rows <- lapply(parts, function(d) {
    d <- d[order(d[[index]]), , drop = FALSE]
    x <- d[[axes[["x"]]]]
    y <- d[[axes[["y"]]]]
    valid <- which(!is.na(x) & !is.na(y))
    if (length(valid) < 2) {
      return(NULL)
    }
    gap <- which(diff(valid) > 1)
    if (!length(gap)) {
      return(NULL)
    }
    starts <- valid[gap]
    ends <- valid[gap + 1]
    data.frame(
      .group = d$.group[starts],
      x = x[starts],
      y = y[starts],
      xend = x[ends],
      yend = y[ends],
      time = d[[index]][starts],
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || !nrow(rows)) NULL else rows
}
