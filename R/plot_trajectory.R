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
#'   by `time` using the Material gradient scale ([scale_colour_material_c()]).
#' * **what-only** or **when-only** grouping (one varying axis): each line
#'   gets its own hue from a qualitative palette and shades from light to
#'   that hue along `time`, so direction reads from both the start/end
#'   markers and the within-line gradient.
#' * **matrix** grouping (both axes vary): each line is solid, coloured by
#'   the hue × shade matrix from [palette_animovement()] — hue per `what`,
#'   shade per `when`.
#'
#' Every trajectory is annotated with a filled circle at its first point and
#' a filled triangle at its last point.
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

  base <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y))

  if (keys$mode == "single") {
    path_layers <- list(
      ggplot2::geom_path(
        ggplot2::aes(colour = .data$time, group = .data$.group)
      ),
      scale_colour_material_c()
    )
  } else {
    plot_df[[".row_colour"]] <- row_colours(plot_df, pal, keys$mode)
    path_layers <- list(
      ggplot2::geom_path(
        data = plot_df,
        ggplot2::aes(colour = .data$.row_colour, group = .data$.group)
      ),
      ggplot2::scale_colour_identity()
    )
  }

  p <- base +
    path_layers +
    ggplot2::geom_point(
      data = endpoints,
      ggplot2::aes(x = .data$x_start, y = .data$y_start, fill = .data$.group),
      shape = 21,
      size = 2.5,
      colour = "grey20",
      show.legend = keys$mode != "single"
    ) +
    ggplot2::geom_point(
      data = endpoints,
      ggplot2::aes(x = .data$x_end, y = .data$y_end, fill = .data$.group),
      shape = 24,
      size = 2.5,
      colour = "grey20",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = x_lab, y = y_lab, colour = NULL, fill = NULL) +
    theme_animovement(mode = mode)

  if (keys$mode == "single") {
    p <- p + ggplot2::guides(fill = "none")
  }

  p
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

# Internal: per-row hex colours used with scale_colour_identity().
# For matrix mode each row is the group's solid colour. For what/when modes
# each line ramps from a light tint of its hue to the hue itself as time
# progresses within the group.
row_colours <- function(df, pal, mode) {
  if (mode == "matrix") {
    return(unname(pal[as.character(df$.group)]))
  }

  out <- character(nrow(df))
  for (g in levels(df$.group)) {
    idx <- which(df$.group == g)
    base_col <- pal[[g]]
    light <- lighten_colour(base_col, amount = 0.7)
    ramp <- grDevices::colorRampPalette(c(light, base_col))(100)

    t_vals <- df$time[idx]
    t_range <- range(t_vals, na.rm = TRUE)
    norm <- if (diff(t_range) == 0) {
      rep(0.5, length(t_vals))
    } else {
      (t_vals - t_range[1]) / diff(t_range)
    }
    out[idx] <- ramp[pmin(pmax(round(norm * 99) + 1, 1), 100)]
  }
  out
}
