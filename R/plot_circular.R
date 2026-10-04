#' Plot the Distribution of an Angle
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draws an angular column of an aniframe as a circular histogram, or rose
#' diagram: one wedge per bin of directions, its size the share of the group's
#' angles that fall in the bin. Use it for any angle the frame stores in its
#' angular unit, such as `course` from `animetric::calculate_kinematics()` or a
#' 2D orientation (`heading`) declared with `animetric::add_orientation()`.
#'
#' The plot takes its conventions from the frame rather than assuming them:
#'
#' * **Unit:** the column is read in the frame's `unit_angle`
#'   ([anicore::get_metadata()]) and binned in radians
#'   ([anicore::angle_to_rad()]). The angular axis is labelled in degrees or
#'   radians to match, and `binwidth` is given in the same unit. A frame
#'   declaring no angular unit is read as radians.
#' * **Zero and sense:** `0` points along `+x`, drawn to the right, and angles
#'   count from `x` toward `y`. Whether that is counter-clockwise or clockwise
#'   on the page comes from [anicore::get_angle_direction()]: a frame with `y`
#'   pointing up is drawn counter-clockwise, and one with `y` pointing down,
#'   such as image coordinates, clockwise. A frame whose axis directions are
#'   unknown is drawn counter-clockwise, the sense of `atan2()`.
#' * **Range:** angles are wrapped onto the circle before binning, so signed
#'   angles in `(-pi, pi]`, angles in `[0, 2*pi)` and unwrapped angles all
#'   give the same plot, and a distribution straddling `pi` is not split. The
#'   axis is labelled in the range the column uses: signed when it holds
#'   negative angles, otherwise from `0` to a full turn.
#'
#' The bins are centred on multiples of the bin width, starting at `0`, so
#' with the default 36 bins of 10 degrees the four directions along the axes
#' each fall in the middle of a bin.
#'
#' Wedges show the share of each group's angles, or with `weight`, the share
#' of its total weight. They start from an empty inner circle, `inner_radius`
#' of the way out, which keeps the narrow tips of the wedges from crowding the
#' centre; set it to `0` for a classic rose. By default a wedge's area, rather
#' than its length, is proportional to its share, as in `circular::rose.diag()`
#' and `tectonicr::rose()`: a wedge of share `n` reaches from the inner radius
#' `r0` out to `sqrt(r0^2 + k * n)`, with `k` set so that the largest reaches
#' the edge. With `equal_area = FALSE` its length is proportional to its share
#' instead, which makes peaks stand out more but exaggerates them. The radial
#' axis is labelled in shares either way. Missing angles and weights are left
#' out.
#'
#' Weighting is useful for directions of travel, such as `course`: when an
#' animal is nearly still, its direction of travel is set by tracking jitter,
#' and those frames can swamp the ones where it is going somewhere. Weighting
#' by `speed` counts each frame by how far it moved.
#'
#' With `show_mean = TRUE`, a thin line marks each group's circular mean
#' direction ([anicore::circ_mean()], weighted when `weight` is given), running
#' from the inner circle to the edge. It shows the direction only, not how
#' concentrated the angles are around it. When the angles are close to
#' uniform, the mean resultant length is small and the mean direction is
#' poorly defined, so read the line together with the wedges.
#'
#' Groups are the frame's trajectory groups ([anicore::get_keys()]), coloured
#' with [palette_animovement()] as in [plot_timeseries()]. The `layout`
#' argument chooses how they are arranged:
#'
#' * `"facet"` (default): each group gets its own panel, filled in its colour
#'   -- `facet_grid()` when both an identity and a temporal context (such as
#'   trial) vary, otherwise `facet_wrap()`.
#' * `"inline"`: all groups share one panel, each drawn as an outline in its
#'   colour, with a legend when there is more than one group.
#'
#' @param data An aniframe object.
#' @param variable Name of the angular column to plot, a single string.
#'   Required (no auto-detection).
#' @param ... Additional arguments (currently unused).
#' @param bins Number of bins round the full circle, a single whole number.
#'   Defaults to `36`, bins of 10 degrees, unless `binwidth` is given.
#' @param binwidth Width of each bin, in the frame's angular unit, as an
#'   alternative to `bins`. It must divide a full turn into a whole number of
#'   bins, for example `15` in a frame in degrees or `pi / 6` in one in
#'   radians.
#' @param weight Name of a numeric column to weight each angle by, such as
#'   `"speed"`, or `NULL` (default) to count each angle once. Weights must not
#'   be negative.
#' @param equal_area Whether a wedge's area (`TRUE`, default) or its length
#'   (`FALSE`) is proportional to its share.
#' @param inner_radius Radius of the empty circle in the middle, as a fraction
#'   of the plot's radius, from `0` (a classic rose) up to but not including
#'   `1`. Default `0.25`.
#' @param show_mean Whether to draw a line at each group's circular mean
#'   direction. Default `TRUE`.
#' @param layout Either `"facet"` (default, one panel per group) or `"inline"`
#'   (all groups in one panel, as outlines).
#' @param mode Either `"light"` (default) or `"dark"`; passed to
#'   [theme_animovement()].
#' @param palette Name of a qualitative palette accepted by
#'   [grDevices::hcl.colors()]; controls the hue family across groups.
#'
#' @return A ggplot object.
#'
#' @seealso [plot_timeseries()] for the same column against time.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 200, n_individuals = 2, n_keypoints = 1)
#' af$course <- atan2(c(0, diff(af$y)), c(0, diff(af$x)))
#' plot_circular(af, variable = "course")
#'
#' # Fewer, wider bins, with the groups overlaid
#' plot_circular(af, variable = "course", binwidth = pi / 6, layout = "inline")
#'
#' @examplesIf requireNamespace("animetric", quietly = TRUE)
#' # Course from animetric, and a heading declared from two keypoints, in a
#' # frame storing angles in degrees
#' kin <- anicore::example_anipoint(n_obs = 200, n_individuals = 1) |>
#'   anicore::convert_unit_angle("deg") |>
#'   animetric::add_orientation(
#'     from = "shoulder_right",
#'     to = "head",
#'     level = "keypoint"
#'   ) |>
#'   animetric::calculate_kinematics()
#'
#' # Each frame counted by how far it moved
#' plot_circular(kin, variable = "course", weight = "speed")
#' plot_circular(kin, variable = "heading", show_mean = FALSE)
#'
#' @export
plot_circular <- function(data, ...) {
  UseMethod("plot_circular")
}

#' @rdname plot_circular
#' @export
plot_circular.default <- function(
  data,
  variable = NULL,
  ...,
  bins = NULL,
  binwidth = NULL,
  weight = NULL,
  equal_area = TRUE,
  inner_radius = 0.25,
  show_mean = TRUE,
  layout = c("facet", "inline"),
  mode = c("light", "dark"),
  palette = "Dark 3"
) {
  if (!anicore::is_aniframe(data) || anicore::is_anievent(data)) {
    cli::cli_abort("{.arg data} must be an aniframe.")
  }
  variable <- check_circular_variable(data, variable)
  unit <- circular_unit(data)
  bins <- circular_bin_count(bins, binwidth, unit)
  weights <- circular_weights(data, weight)
  if (!rlang::is_bool(equal_area)) {
    cli::cli_abort("{.arg equal_area} must be {.code TRUE} or {.code FALSE}.")
  }
  if (
    !is.numeric(inner_radius) ||
      length(inner_radius) != 1L ||
      !is.finite(inner_radius) ||
      inner_radius < 0 ||
      inner_radius >= 1
  ) {
    cli::cli_abort(
      "{.arg inner_radius} must be a single number from 0 up to, but not
       including, 1."
    )
  }
  if (!rlang::is_bool(show_mean)) {
    cli::cli_abort("{.arg show_mean} must be {.code TRUE} or {.code FALSE}.")
  }
  layout <- match.arg(layout)
  mode <- match.arg(mode)

  sense <- circular_sense(data)
  values <- data[[variable]]
  signed <- any(values[is.finite(values)] < 0)
  theta <- anicore::angle_to_rad(values, unit)

  keys <- aniframe_group_keys(data)
  pal <- palette_animovement(data, palette = palette)
  group <- factor(keys$group, levels = names(pal))
  key_df <- data[c(keys$what_cols, keys$when_cols)]

  width <- 2 * pi / bins
  wedges <- circular_bins(theta, weights, group, key_df, bins, sense)
  # The largest share across panels reaches the edge, so panels compare. With
  # nothing to draw, any positive value will do.
  share_max <- max(c(wedges$share, 0))
  if (share_max <= 0) {
    share_max <- 1
  }
  radius <- function(share) {
    circular_radius(share, share_max, inner_radius, equal_area)
  }
  wedges$r <- radius(wedges$share)

  # The theme's text colour, for the mean line.
  ink <- theme_animovement(mode = mode)$text$colour
  hide_legend <- layout == "facet" || keys$mode == "single"

  p <- ggplot2::ggplot()
  if (layout == "facet") {
    p <- p +
      ggplot2::geom_rect(
        data = wedges,
        ggplot2::aes(
          xmin = .data$pos_min,
          xmax = .data$pos_max,
          ymin = 0,
          ymax = .data$r,
          fill = .data$.group
        ),
        colour = NA
      )
  } else {
    p <- p +
      ggplot2::geom_path(
        data = circular_outline(wedges),
        ggplot2::aes(
          x = .data$pos,
          y = .data$r,
          group = .data$.group,
          colour = .data$.group
        ),
        linewidth = 0.6
      )
  }

  if (show_mean) {
    means <- circular_means(theta, weights, group, key_df, sense, bins)
    # A radial line from the inner circle (0) to the edge (1), whatever the
    # resultant length, so the direction reads even when it is small.
    line_args <- list(
      data = means,
      mapping = ggplot2::aes(
        x = .data$pos,
        xend = .data$pos,
        y = 0,
        yend = 1,
        colour = .data$.group
      ),
      linewidth = 0.4,
      show.legend = FALSE
    )
    # In its own panel, the line is drawn in the theme's text colour; overlaid,
    # it takes its group's colour, so each line can be told apart.
    if (layout == "facet") {
      line_args$mapping$colour <- NULL
      line_args$colour <- ink
    }
    p <- p + do.call(ggplot2::geom_segment, line_args)
  }

  breaks <- circular_breaks(unit, signed, sense, bins)
  share_breaks <- pretty(c(0, share_max))
  share_breaks <- share_breaks[share_breaks > 0 & share_breaks <= share_max]
  group_scale <- if (layout == "facet") {
    ggplot2::scale_fill_manual(values = pal, drop = FALSE)
  } else {
    ggplot2::scale_colour_manual(values = pal, drop = FALSE)
  }

  p <- p +
    ggplot2::scale_x_continuous(
      limits = c(0, 2 * pi),
      breaks = breaks$pos,
      labels = breaks$label
    ) +
    # The radial position is drawn on [0, 1] from the inner circle to the
    # edge, and labelled with the shares it stands for.
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = radius(share_breaks),
      labels = paste0(signif(100 * share_breaks, 3), "%")
    ) +
    group_scale +
    # Position 0 is the edge of the first bin; `start` turns it to that edge's
    # direction, measured clockwise from 12 o'clock.
    ggplot2::coord_radial(
      start = pi / 2 - width / 2,
      expand = FALSE,
      inner.radius = inner_radius
    ) +
    ggplot2::labs(
      x = circular_axis_title(variable, unit),
      y = if (is.null(weight)) {
        "share of angles"
      } else {
        paste("share of angles, weighted by", weight)
      },
      colour = NULL,
      fill = NULL
    ) +
    theme_animovement(mode = mode) +
    # The circle is the panel; a box round it only crowds the angle labels.
    ggplot2::theme(panel.border = ggplot2::element_blank())

  if (layout == "facet") {
    facet_layer <- timeseries_facets(data, keys, ncol = NULL)
    if (!is.null(facet_layer)) {
      p <- p + facet_layer
    }
  }
  if (hide_legend) {
    p <- p + ggplot2::guides(colour = "none", fill = "none")
  }
  p
}

# Internal: validate `variable` -- a single numeric column, named explicitly.
check_circular_variable <- function(data, variable) {
  variable <- check_timeseries_variables(data, variable)
  if (length(variable) != 1L) {
    cli::cli_abort(
      "{.arg variable} must name a single column, not {length(variable)}."
    )
  }
  variable
}

# Internal: the number of bins, from `bins` or from `binwidth` (in the frame's
# angular unit), 36 when neither is given.
circular_bin_count <- function(bins, binwidth, unit) {
  if (!is.null(bins) && !is.null(binwidth)) {
    cli::cli_abort("Supply only one of {.arg bins} and {.arg binwidth}.")
  }
  if (!is.null(binwidth)) {
    if (
      !rlang::is_scalar_double(binwidth) && !rlang::is_scalar_integer(binwidth)
    ) {
      cli::cli_abort("{.arg binwidth} must be a single number.")
    }
    if (!is.finite(binwidth) || binwidth <= 0) {
      cli::cli_abort("{.arg binwidth} must be a positive number.")
    }
    n <- 2 * pi / anicore::angle_to_rad(binwidth, unit)
    if (abs(n - round(n)) > 1e-6 || round(n) < 1) {
      turn <- if (unit == "deg") "360" else "2 * pi"
      cli::cli_abort(c(
        "{.arg binwidth} must divide a full turn into a whole number of bins.",
        "x" = "A full turn, {turn}, holds {format(n, digits = 4)} bins of
               {binwidth}.",
        "i" = "Angles in this frame are in {.val {unit}}."
      ))
    }
    return(as.integer(round(n)))
  }
  if (is.null(bins)) {
    return(36L)
  }
  if (!rlang::is_scalar_integerish(bins, finite = TRUE) || bins < 1) {
    cli::cli_abort("{.arg bins} must be a single whole number of at least 1.")
  }
  as.integer(bins)
}

# Internal: the weight of each row -- 1 throughout when `weight` is NULL, or
# else the values of the column it names, which must be numeric and not
# negative.
circular_weights <- function(data, weight) {
  if (is.null(weight)) {
    return(rep(1, nrow(data)))
  }
  if (!rlang::is_string(weight)) {
    cli::cli_abort("{.arg weight} must be a single column name.")
  }
  if (!weight %in% names(data)) {
    cli::cli_abort("{.arg weight} names an unknown column: {.val {weight}}.")
  }
  w <- data[[weight]]
  if (!is.numeric(w)) {
    cli::cli_abort("{.arg weight} must name a numeric column.")
  }
  if (any(w < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg weight} must not be negative.")
  }
  w
}

# Internal: where a share is drawn on the radial axis, which runs from 0 at
# the inner circle (radius `inner`, as a fraction of the plot's) to 1 at the
# edge, where `share_max` reaches. With `equal_area`, a wedge's area in the
# ring is proportional to its share: its outer radius is
# sqrt(inner^2 + k * share), k chosen so share_max reaches radius 1.
# Otherwise its length is proportional to its share.
circular_radius <- function(share, share_max, inner, equal_area) {
  if (!equal_area) {
    return(share / share_max)
  }
  outer <- sqrt(inner^2 + (1 - inner^2) * share / share_max)
  (outer - inner) / (1 - inner)
}

# Internal: the frame's angular unit as a string, `"none"` when undeclared.
circular_unit <- function(data) {
  as.character(anicore::get_metadata(data, "unit_angle") %||% "none")
}

# Internal: the drawing sense, +1 when angles run clockwise on the page and
# -1 when they run counter-clockwise. An unknown sense is drawn
# counter-clockwise, as atan2() counts.
circular_sense <- function(data) {
  if (identical(anicore::get_angle_direction(data), "clockwise")) 1 else -1
}

# Internal: where a frame angle (radians) sits on the plot's theta scale.
# Position runs clockwise from 0 to 2*pi, starting at the edge of the bin
# centred on angle 0 that comes first in the clockwise direction. With
# `sense` +1 (clockwise) that is its lower edge, -width/2; with -1, its upper
# edge, +width/2. Every bin then spans [k * width, (k + 1) * width] for some
# k, and none straddles the ends of the scale.
circular_position <- function(theta, sense, bins) {
  width <- 2 * pi / bins
  (sense * theta + width / 2) %% (2 * pi)
}

# Internal: split row indices by group, keeping the rows whose angle and
# weight are both usable. Returns a list named by group level.
circular_groups <- function(theta, weights, group) {
  ok <- is.finite(theta) & is.finite(weights)
  lapply(split(seq_along(theta), group), function(idx) {
    list(all = idx, valid = idx[ok[idx]])
  })
}

# Internal: share of each group's angles (or weight) per bin, one row per
# group and bin, with the group's key columns carried along for faceting.
# `theta` is in radians, in any range.
circular_bins <- function(theta, weights, group, key_df, bins, sense) {
  width <- 2 * pi / bins
  centre <- (seq_len(bins) - 1) * width
  # Bin k is centred on k * width; positions put it at [p, p + width].
  pos_min <- round(circular_position(centre, sense, bins) / width - 0.5) *
    width
  parts <- circular_groups(theta, weights, group)
  rows <- lapply(names(parts), function(g) {
    idx <- parts[[g]]$valid
    k <- floor(anicore::wrap_angle(theta[idx] + width / 2) / width) %% bins
    total <- tapply(weights[idx], factor(k, levels = seq_len(bins) - 1), sum)
    total[is.na(total)] <- 0
    total <- as.vector(total)
    out <- data.frame(
      .group = factor(rep(g, bins), levels = levels(group)),
      centre = centre,
      pos_min = pos_min,
      pos_max = pos_min + width,
      total = total,
      share = if (sum(total) > 0) total / sum(total) else rep(0, bins)
    )
    circular_attach_keys(out, key_df, parts[[g]]$all)
  })
  # Every group has a row per bin, even one with no angles, and there is
  # always at least one group, so `rows` is never empty.
  do.call(rbind, rows)
}

# Internal: add a group's key columns (from its first row, `idx[1]`) to `out`,
# so facet_wrap() / facet_grid() can find them.
circular_attach_keys <- function(out, key_df, idx) {
  for (col in names(key_df)) {
    out[[col]] <- rep(key_df[[col]][idx[1]], nrow(out))
  }
  out
}

# Internal: the outline of each group's wedges, as one path running clockwise
# round the circle from position 0 -- along the top of each bin, and radially
# between neighbouring bins -- and back down to where it started. A polygon
# would close with a segment from 2*pi back to 0, drawn as a full circle.
circular_outline <- function(wedges) {
  parts <- split(wedges, wedges$.group, drop = TRUE)
  rows <- lapply(parts, function(w) {
    w <- w[order(w$pos_min), , drop = FALSE]
    data.frame(
      .group = rep(w$.group[1], 2 * nrow(w) + 1),
      pos = c(as.vector(rbind(w$pos_min, w$pos_max)), 2 * pi),
      r = c(rep(w$r, each = 2), w$r[1])
    )
  })
  do.call(rbind, rows)
}

# Internal: each group's circular mean direction and mean resultant length,
# both weighted by `weights`, with its key columns. Groups with no angles, or
# no weight, are left out.
circular_means <- function(theta, weights, group, key_df, sense, bins) {
  parts <- circular_groups(theta, weights, group)
  rows <- lapply(names(parts), function(g) {
    idx <- parts[[g]]$valid
    th <- theta[idx]
    w <- weights[idx]
    if (!length(th) || sum(w) <= 0) {
      return(NULL)
    }
    # circ_mean() generalised to weights: the direction of the weighted mean
    # of the unit vectors, and that mean's length.
    c_bar <- sum(w * cos(th)) / sum(w)
    s_bar <- sum(w * sin(th)) / sum(w)
    direction <- if (all(w == w[1])) {
      anicore::circ_mean(th)
    } else {
      anicore::wrap_angle(atan2(s_bar, c_bar))
    }
    out <- data.frame(
      .group = factor(g, levels = levels(group)),
      mean = direction,
      resultant = min(sqrt(c_bar^2 + s_bar^2), 1)
    )
    out$pos <- circular_position(out$mean, sense, bins)
    circular_attach_keys(out, key_df, parts[[g]]$all)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- circular_attach_keys(
      data.frame(
        .group = factor(character(0), levels = levels(group)),
        mean = numeric(0),
        resultant = numeric(0),
        pos = numeric(0)
      ),
      key_df,
      integer(0)
    )
  }
  out
}

# Internal: angular breaks every eighth of a turn, with their positions on
# the theta scale and labels in the frame's unit. `signed` labels the lower
# half of the circle with negative angles, as in (-pi, pi].
circular_breaks <- function(unit, signed, sense, bins) {
  eighths <- 0:7
  if (signed) {
    eighths[eighths > 4] <- eighths[eighths > 4] - 8
  }
  theta <- eighths * pi / 4
  # Radians as plotmath, which draws pi on every device: the pdf device has
  # no glyph for the character itself.
  label <- if (unit == "deg") {
    paste0(eighths * 45, "\u00b0")
  } else {
    parse(text = pi_eighths_label(eighths))
  }
  list(pos = circular_position(theta, sense, bins), label = label)
}

# Internal: label multiples of pi / 4 as fractions of pi in lowest terms, as
# plotmath text, e.g. "3*pi/4".
pi_eighths_label <- function(eighths) {
  vapply(
    eighths,
    function(k) {
      if (k == 0) {
        return("0")
      }
      # k / 4 in lowest terms
      d <- 4L
      n <- as.integer(k)
      while (n %% 2L == 0L && d > 1L) {
        n <- n %/% 2L
        d <- d %/% 2L
      }
      num <- if (n == 1L) {
        "pi"
      } else if (n == -1L) {
        "-pi"
      } else {
        paste0(n, "*pi")
      }
      if (d == 1L) num else paste0(num, "/", d)
    },
    character(1)
  )
}

# Internal: the angular axis title, the column named with its unit.
circular_axis_title <- function(variable, unit) {
  if (unit %in% c("deg", "rad")) paste0(variable, " (", unit, ")") else variable
}
