#' Lab Arenas
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Arenas for common behavioural setups, to draw under trajectories with
#' [plot_trajectory()] or [geom_arena()]. Each is drawn in its own unit around
#' a natural origin: open fields from their lower-left corner, everything else
#' from its centre. Calibrate the data to match, or move the arena with
#' [arena_transform()].
#'
#' * `arena_rect()`: a rectangular open field, optionally with a centre zone.
#' * `arena_circle()`: a circular open field or water maze, optionally with a
#'   platform and quadrant lines.
#' * `arena_ymaze()`: three arms 120 degrees apart, the first pointing up.
#' * `arena_tmaze()`: a start arm pointing up into two side arms.
#' * `arena_plusmaze()`: an elevated plus maze, four arms with two closed.
#' * `arena_radial()`: a radial arm maze, optionally with a central hub.
#'
#' Arm lengths run from the centre of the maze to the end of the arm.
#'
#' @param width,height Size of a rectangular field.
#' @param radius Radius of a circular arena.
#' @param arm_length Length of each arm, from the maze's centre.
#' @param arm_width Width of each arm.
#' @param stem_length Length of the T-maze's start arm, from its end to the
#'   side arms.
#' @param n_arms Number of arms of a radial maze.
#' @param hub_radius Radius of a radial maze's central platform, to the corners
#'   between arms (default `NULL`, no platform: the arms meet directly).
#' @param unit Unit of the dimensions (default `"cm"`).
#' @param centre_zone Size of a centre zone as a fraction of the field (default
#'   `NULL`, none), such as `0.5` for the middle half of each side.
#' @param platform A platform as `c(x, y, radius)` (default `NULL`, none).
#' @param quadrants A logical value (default `FALSE`) determining whether lines
#'   divide a circular arena into quadrants.
#' @param closed Labels of the plus maze's closed arms (default `c("N", "S")`).
#' @param labels Names of the arms, drawn at their ends, or `NULL` for none.
#'
#' @return An arena; see [as_arena()].
#'
#' @family arenas
#' @examples
#' plot(arena_rect(60, 45, centre_zone = 0.5))
#' plot(arena_circle(75, platform = c(25, 25, 6), quadrants = TRUE))
#' plot(arena_ymaze(arm_length = 40, arm_width = 8))
#' plot(arena_tmaze(stem_length = 50, arm_length = 40, arm_width = 10))
#' plot(arena_plusmaze(arm_length = 35, arm_width = 5))
#' plot(arena_radial(n_arms = 8, arm_length = 60, arm_width = 10, hub_radius = 20))
#' @name lab_arenas
NULL

#' @rdname lab_arenas
#' @export
arena_rect <- function(width, height, unit = "cm", centre_zone = NULL) {
  check_dimensions(width = width, height = height)
  parts <- list(arena_xy("wall", "wall", arena_rect_xy(0, width, 0, height)))
  if (!is.null(centre_zone)) {
    if (!is.numeric(centre_zone) || centre_zone <= 0 || centre_zone >= 1) {
      cli::cli_abort("{.arg centre_zone} must be a fraction between 0 and 1.")
    }
    mx <- width * (1 - centre_zone) / 2
    my <- height * (1 - centre_zone) / 2
    parts <- c(
      parts,
      list(arena_xy(
        "centre",
        "zone",
        arena_rect_xy(mx, width - mx, my, height - my)
      ))
    )
  }
  as_arena(do.call(rbind, parts), unit, "rectangular open field")
}

#' @rdname lab_arenas
#' @export
arena_circle <- function(
  radius,
  unit = "cm",
  platform = NULL,
  quadrants = FALSE
) {
  check_dimensions(radius = radius)
  parts <- list(arena_xy("wall", "wall", arena_arc_xy(0, 0, radius)))
  if (isTRUE(quadrants)) {
    parts <- c(
      parts,
      list(
        arena_shape(
          "quadrants",
          "line",
          c(-radius, radius),
          c(0, 0),
          path = 1L
        ),
        arena_shape("quadrants", "line", c(0, 0), c(-radius, radius), path = 2L)
      )
    )
  }
  if (!is.null(platform)) {
    if (!is.numeric(platform) || length(platform) != 3) {
      cli::cli_abort("{.arg platform} must be {.code c(x, y, radius)}.")
    }
    parts <- c(
      parts,
      list(arena_xy(
        "platform",
        "zone",
        arena_arc_xy(platform[1], platform[2], platform[3])
      ))
    )
  }
  as_arena(do.call(rbind, parts), unit, "circular arena")
}

#' @rdname lab_arenas
#' @export
arena_ymaze <- function(
  arm_length,
  arm_width,
  unit = "cm",
  labels = c("A", "B", "C")
) {
  check_dimensions(arm_length = arm_length, arm_width = arm_width)
  angles <- pi / 2 + c(0, 2, 4) * pi / 3
  as_arena(arena_arms(angles, arm_length, arm_width, labels), unit, "Y-maze")
}

#' @rdname lab_arenas
#' @export
arena_tmaze <- function(
  stem_length,
  arm_length,
  arm_width,
  unit = "cm",
  labels = c("start", "left", "right")
) {
  check_dimensions(
    stem_length = stem_length,
    arm_length = arm_length,
    arm_width = arm_width
  )
  h <- arm_width / 2
  s <- stem_length
  a <- arm_length
  outline <- arena_xy(
    "wall",
    "wall",
    arena_close(list(
      x = c(-h, h, h, a, a, -a, -a, -h),
      y = c(0, 0, s, s, s + arm_width, s + arm_width, s, s)
    ))
  )
  parts <- list(outline)
  if (!is.null(labels)) {
    gap <- arm_width * 0.6
    parts <- c(
      parts,
      list(arena_shape(
        paste0("label_", labels),
        "label",
        c(0, -a - gap, a + gap),
        c(-gap, s + h, s + h),
        label = labels
      ))
    )
  }
  as_arena(do.call(rbind, parts), unit, "T-maze")
}

#' @rdname lab_arenas
#' @export
arena_plusmaze <- function(
  arm_length,
  arm_width,
  unit = "cm",
  closed = c("N", "S"),
  labels = c("N", "W", "S", "E")
) {
  check_dimensions(arm_length = arm_length, arm_width = arm_width)
  angles <- pi / 2 + (0:3) * pi / 2
  arm_names <- labels %||% c("N", "W", "S", "E")
  unknown <- setdiff(closed, arm_names)
  if (length(unknown) > 0) {
    cli::cli_abort("{.arg closed} names {.val {unknown}}, which is not an arm.")
  }
  zones <- lapply(match(closed, arm_names), function(i) {
    arena_xy(
      paste0("closed_", arm_names[i]),
      "zone",
      arena_arm_rect(angles[i], arm_width / 2, arm_length, arm_width)
    )
  })
  arms <- arena_arms(angles, arm_length, arm_width, labels)
  as_arena(do.call(rbind, c(list(arms), zones)), unit, "elevated plus maze")
}

#' @rdname lab_arenas
#' @export
arena_radial <- function(
  n_arms,
  arm_length,
  arm_width,
  hub_radius = NULL,
  unit = "cm",
  labels = as.character(seq_len(n_arms))
) {
  check_dimensions(
    n_arms = n_arms,
    arm_length = arm_length,
    arm_width = arm_width
  )
  if (n_arms < 3) {
    cli::cli_abort("A radial maze needs at least 3 arms, not {n_arms}.")
  }
  angles <- pi / 2 + (seq_len(n_arms) - 1) * 2 * pi / n_arms
  arms <- arena_arms(angles, arm_length, arm_width, labels, hub_radius)
  as_arena(arms, unit, "radial arm maze")
}

#' Sports Arenas
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Playing surfaces drawn to their rules, centred on the middle of the field
#' with x along its length, to draw under tracking data with
#' [plot_trajectory()] or [geom_arena()]. They are coloured in the `"sport"`
#' palette of [geom_arena()].
#'
#' * `arena_football()`: a football (soccer) pitch to the IFAB Laws of the
#'   Game, in metres. The markings are fixed by the Laws; the length and width
#'   vary between grounds.
#'
#' @param length,width Size of the pitch between its outer lines, in metres
#'   (default 105 by 68, the size FIFA recommends).
#'
#' @return An arena; see [as_arena()].
#'
#' @family arenas
#' @examples
#' plot(arena_football())
#' plot(arena_football(), mode = "dark")
#' plot(arena_football(), palette = "plain")
#' @name sports_arenas
NULL

#' @rdname sports_arenas
#' @export
arena_football <- function(length = 105, width = 68) {
  check_dimensions(length = length, width = width)
  if (length <= width) {
    cli::cli_abort("{.arg length} must be longer than {.arg width}.")
  }
  L <- length / 2
  W <- width / 2
  runoff <- 4
  centre_circle <- 9.15
  edges <- seq(-L, L, length.out = 13)

  parts <- list(
    arena_xy(
      "grass",
      "surface",
      arena_rect_xy(-L - runoff, L + runoff, -W - runoff, W + runoff)
    )
  )
  parts <- c(
    parts,
    lapply(seq(1, 11, by = 2), function(i) {
      arena_xy(
        "stripes",
        "stripe",
        arena_rect_xy(edges[i], edges[i + 1], -W, W),
        path = i
      )
    })
  )
  parts <- c(
    parts,
    list(
      arena_xy("boundary", "line", arena_rect_xy(-L, L, -W, W)),
      arena_shape("halfway_line", "line", c(0, 0), c(-W, W)),
      arena_xy("centre_circle", "line", arena_arc_xy(0, 0, centre_circle)),
      arena_shape("centre_mark", "spot", 0, 0)
    )
  )

  # The penalty arc is the part of a 9.15 m circle around the penalty mark
  # that lies outside the penalty area.
  theta <- acos((16.5 - 11) / centre_circle)
  for (side in c("left", "right")) {
    s <- if (side == "left") -1 else 1
    goal_line <- s * L
    box <- function(depth, half_width) {
      inner <- goal_line - s * depth
      arena_rect_xy(
        min(goal_line, inner),
        max(goal_line, inner),
        -half_width,
        half_width
      )
    }
    towards_centre <- if (s > 0) pi else 0
    parts <- c(
      parts,
      list(
        arena_xy(paste0("penalty_area_", side), "line", box(16.5, 20.16)),
        arena_xy(paste0("goal_area_", side), "line", box(5.5, 9.16)),
        arena_xy(paste0("goal_", side), "line", box(-2, 3.66)),
        arena_shape(
          paste0("penalty_mark_", side),
          "spot",
          goal_line - s * 11,
          0
        ),
        arena_xy(
          paste0("penalty_arc_", side),
          "line",
          arena_arc_xy(
            goal_line - s * 11,
            0,
            centre_circle,
            from = towards_centre - theta,
            to = towards_centre + theta,
            n = 40
          )
        )
      )
    )
    for (end in c("bottom", "top")) {
      t <- if (end == "bottom") -1 else 1
      # Quarter circle of 1 m from the corner, into the field
      start <- atan2(-t, -s) - pi / 4
      parts <- c(
        parts,
        list(arena_xy(
          paste0("corner_arc_", side, "_", end),
          "line",
          arena_arc_xy(
            goal_line,
            t * W,
            1,
            from = start,
            to = start + pi / 2,
            n = 12
          )
        ))
      )
    }
  }
  as_arena(do.call(rbind, parts), "m", "football pitch", style = "sport")
}

# Internal ---------------------------------------------------------------

check_dimensions <- function(..., call = rlang::caller_env()) {
  dims <- list(...)
  for (nm in names(dims)) {
    v <- dims[[nm]]
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) {
      cli::cli_abort(
        "{.arg {nm}} must be a single positive number.",
        call = call
      )
    }
  }
  invisible(TRUE)
}

# Outline of arms of equal width radiating from the centre, and their labels.
# Without a hub, the corner between two neighbouring arms lies on the bisector
# between them, half an arm width from both arms' axes. With a hub, the arms
# leave the edges of a regular polygon whose corners sit between the arms.
arena_arms <- function(
  angles,
  arm_length,
  arm_width,
  labels,
  hub_radius = NULL
) {
  n <- length(angles)
  h <- arm_width / 2
  if (!is.null(hub_radius)) {
    check_dimensions(hub_radius = hub_radius)
    if (h >= hub_radius * sin(pi / n)) {
      cli::cli_abort(
        "{.arg arm_width} is too wide for a hub of radius {hub_radius} with {n} arms.",
        call = rlang::caller_env()
      )
    }
  }
  x <- y <- numeric()
  for (i in seq_len(n)) {
    a <- angles[i]
    b <- angles[i %% n + 1]
    gap <- (b - a) %% (2 * pi)
    u <- c(cos(a), sin(a))
    v <- c(-sin(a), cos(a))
    if (is.null(hub_radius)) {
      start <- NULL
      corner <- (h / sin(gap / 2)) * c(cos(a + gap / 2), sin(a + gap / 2))
    } else {
      apothem <- hub_radius * cos(pi / n)
      start <- rbind(apothem * u - h * v)
      corner <- hub_radius * c(cos(a + gap / 2), sin(a + gap / 2))
    }
    pts <- rbind(
      start,
      arm_length * u - h * v,
      arm_length * u + h * v,
      if (!is.null(hub_radius)) apothem * u + h * v,
      corner
    )
    x <- c(x, pts[, 1])
    y <- c(y, pts[, 2])
  }
  outline <- arena_xy("wall", "wall", arena_close(list(x = x, y = y)))
  if (is.null(labels)) {
    return(outline)
  }
  r <- arm_length + arm_width * 0.6
  rbind(
    outline,
    arena_shape(
      paste0("label_", labels),
      "label",
      r * cos(angles),
      r * sin(angles),
      label = labels
    )
  )
}

arena_arm_rect <- function(angle, from, to, arm_width) {
  u <- c(cos(angle), sin(angle))
  v <- c(-sin(angle), cos(angle))
  h <- arm_width / 2
  pts <- rbind(
    from * u - h * v,
    to * u - h * v,
    to * u + h * v,
    from * u + h * v
  )
  arena_close(list(x = pts[, 1], y = pts[, 2]))
}
