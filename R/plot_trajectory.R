#' Plot Movement Trajectory
#'
#' Creates a ggplot of the x-y trajectory from an aniframe. One path is drawn
#' per trajectory group, where a group is the combination of every
#' `variables_what` column and every non-time `variables_when` column in the
#' aniframe's metadata.
#'
#' Colours adapt to the dataset shape:
#'
#' * **single** trajectory (no grouping): the line is coloured continuously
#'   by `time` using the Material gradient scale ([scale_colour_material_c()]),
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
#' @param data An aniframe object.
#' @param ... Additional arguments (currently unused).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#' @param palette Name of a qualitative palette accepted by
#'   [grDevices::hcl.colors()]; controls the hue family across grouping
#'   levels.
#'
#' @return A ggplot object.
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
  if (!aniframe::is_aniframe(data)) {
    cli::cli_abort("{.arg data} must be an aniframe.")
  }
  mode <- match.arg(mode)

  meta <- aniframe::get_metadata(data)
  keys <- aniframe_group_keys(data)
  pal <- palette_animovement(data, palette = palette)

  plot_df <- as.data.frame(data)
  plot_df[[".group"]] <- factor(keys$group, levels = names(pal))
  plot_df <- plot_df[order(plot_df$.group, plot_df$time), , drop = FALSE]

  endpoints <- trajectory_endpoints(plot_df)

  unit <- meta$unit_space
  has_unit <- !is.null(unit) && as.character(unit) != "none"
  x_lab <- if (has_unit) paste0("x (", unit, ")") else "x"
  y_lab <- if (has_unit) paste0("y (", unit, ")") else "y"

  # Time legend: format as HH:MM:SS for true time units (as plot_events does),
  # raw numbers otherwise. Three breaks keep the legend compact.
  t_unit <- if (!is.null(meta$unit_time)) as.character(meta$unit_time) else NA_character_
  t_factor <- seconds_per_unit(t_unit)
  time_labels <- if (!is.na(t_factor)) {
    function(b) format(hms::as_hms(round(b * t_factor)))
  } else {
    ggplot2::waiver()
  }
  three_breaks <- function(limits) round(seq(limits[1], limits[2], length.out = 3))

  # Dashed connectors across missing-data gaps, and start/end markers reshaped
  # so a single shape scale can label which symbol is which.
  bridges <- trajectory_gaps(plot_df)
  ends_long <- data.frame(
    .group = rep(endpoints$.group, 2),
    x = c(endpoints$x_start, endpoints$x_end),
    y = c(endpoints$y_start, endpoints$y_end),
    point = factor(
      rep(c("start", "end"), each = nrow(endpoints)),
      levels = c("start", "end")
    )
  )

  base <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y))

  if (keys$mode == "single") {
    # Time -> a continuous colour bar, titled "time" in the legend.
    path_layers <- Filter(Negate(is.null), list(
      ggplot2::geom_path(
        ggplot2::aes(colour = .data$time, group = .data$.group),
        na.rm = TRUE
      ),
      if (!is.null(bridges)) {
        ggplot2::geom_segment(
          data = bridges,
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            colour = .data$time
          ),
          linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE, na.rm = TRUE
        )
      },
      scale_colour_material_c(
        name = "time",
        labels = time_labels,
        guide = ggplot2::guide_colourbar(order = 3)
      )
    ))
  } else if (keys$mode == "matrix") {
    # Hue x shade per group; solid lines (time reads from the start/end markers).
    path_layers <- Filter(Negate(is.null), list(
      ggplot2::geom_path(
        ggplot2::aes(colour = .data$.group, group = .data$.group),
        na.rm = TRUE
      ),
      if (!is.null(bridges)) {
        ggplot2::geom_segment(
          data = bridges,
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            colour = .data$.group
          ),
          linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE, na.rm = TRUE
        )
      },
      ggplot2::scale_colour_manual(values = pal, guide = "none")
    ))
  } else {
    # what / when: one hue per group, with time mapped to alpha so the line
    # fades in from start to end and earns a real "time" legend.
    path_layers <- Filter(Negate(is.null), list(
      ggplot2::geom_path(
        ggplot2::aes(
          colour = .data$.group,
          alpha = .data$time,
          group = .data$.group
        ),
        na.rm = TRUE
      ),
      if (!is.null(bridges)) {
        ggplot2::geom_segment(
          data = bridges,
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            colour = .data$.group
          ),
          linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE, na.rm = TRUE
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
    ))
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
      guide = ggplot2::guide_legend(order = 2, override.aes = list(fill = "grey45"))
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

# Internal: per-group first/last (x, y) ordered by time.
trajectory_endpoints <- function(df) {
  ord <- order(df$.group, df$time)
  df <- df[ord, , drop = FALSE]
  parts <- split(df, df$.group, drop = TRUE)
  rows <- lapply(parts, function(d) {
    non_na <- !is.na(d$x) & !is.na(d$y)
    if (!any(non_na)) {
      return(NULL)
    }
    d <- d[non_na, , drop = FALSE]
    data.frame(
      .group = d$.group[1],
      x_start = d$x[1],
      y_start = d$y[1],
      x_end = d$x[nrow(d)],
      y_end = d$y[nrow(d)],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Internal: per-group connector segments spanning missing-data gaps. For each
# run of missing (x, y) between two valid points, returns a segment from the
# last valid point to the next valid one (drawn dashed). Returns NULL when no
# group has a gap.
trajectory_gaps <- function(df) {
  parts <- split(df, df$.group, drop = TRUE)
  rows <- lapply(parts, function(d) {
    d <- d[order(d$time), , drop = FALSE]
    valid <- which(!is.na(d$x) & !is.na(d$y))
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
      x = d$x[starts],
      y = d$y[starts],
      xend = d$x[ends],
      yend = d$y[ends],
      time = d$time[starts],
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || !nrow(rows)) NULL else rows
}
